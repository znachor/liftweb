package net.liftweb.json

/*
 * Copyright 2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

import java.lang.reflect.{Constructor => JConstructor, Type}
import java.util.Date
import scala.reflect.Manifest
import JsonAST._

/** Function to extract values from JSON AST using case classes.
 *
 *  FIXME: Add support to extract List of values too.
 *  FIXME: Add support for Optional values.
 *  FIXME: Add annnotation to configure path
 *
 *  See: ExtractionExamples.scala
 */
object Extraction {
  /** Intermediate format which describes the mapping.
   *  This ADT is constructed (and then memoized) from given case class using reflection.
   *
   *  Example mapping.
   *
   *  package xx 
   *  case class Person(name: String, address: Address, children: List[Child])
   *  case class Address(street: String, city: String)
   *  case class Child(name: String, age: BigInt)
   *
   *  will produce following Mapping:
   *
   *  Constructor(None, "xx.Person", List(
   *    Value("name"),
   *    Constructor(Some("address"), "xx.Address", List(Value("street"), Value("city"))),
   *    ListConstructor("children", "xx.Child", List(Value("name"), Value("age")))))
   */
  sealed abstract class Mapping
  case class Value(path: String, targetType: Class[_]) extends Mapping
  case class Constructor(path: Option[String], constructor: JConstructor[_], args: List[Mapping]) extends Mapping
  case class ListConstructor(path: String, constructor: JConstructor[_], args: List[Mapping]) extends Mapping
  case class ListOfPrimitives(path: String, elementType: Class[_]) extends Mapping
  case class Optional(mapping: Mapping) extends Mapping

  val memo = new Memo[Class[_], Mapping]

  def extract[A](json: JValue)(implicit formats: Formats, mf: Manifest[A]): A = 
    try {
      extract0(json, formats, mf)
    } catch {
      case e: MappingException => throw e
      case e: Exception => throw new MappingException("unknown error", e)
    }

  private def extract0[A](json: JValue, formats: Formats, mf: Manifest[A]): A = {
    val mapping = mappingOf(mf.erasure)

    def newInstance(constructor: JConstructor[_], args: List[Any]) = try {
      constructor.newInstance(args.map(_.asInstanceOf[AnyRef]).toArray: _*)
    } catch {
      case e @ (_:IllegalArgumentException | _:InstantiationException) => fail("Parsed JSON values do not match with class constructor\nargs=" + args.mkString(",") + "\narg types=" + args.map(_.asInstanceOf[AnyRef].getClass.getName).mkString(",")  + "\nconstructor=" + constructor)
    }

    def newPrimitive(elementType: Class[_], elem: JValue) = convert(elem, elementType, formats)

    def build(root: JValue, mapping: Mapping, argStack: List[Any]): List[Any] = mapping match {
      case Value(path, targetType) => convert(fieldValue(root, path), targetType, formats) :: argStack
      case Constructor(path, classname, args) => 
        val newRoot = path match {
          case Some(p) => root \ p
          case None => root
        }
        newInstance(classname, args.flatMap(build(newRoot, _, argStack))) :: Nil
      case ListConstructor(path, classname, args) => 
        val arr = fieldValue(root, path).asInstanceOf[JArray]
        arr.arr.map(elem => newInstance(classname, args.flatMap(build(elem, _, argStack)))) :: argStack
      case ListOfPrimitives(path, elementType) =>
        val arr = fieldValue(root, path).asInstanceOf[JArray]
        arr.arr.map(elem => newPrimitive(elementType, elem)) :: argStack
      case Optional(m) =>
        // FIXME Remove this try-catch.
        try { 
          val opt = build(root, m, argStack) 
          (opt(0) match {
            case null => None
            case x => Some(x)
          }) :: argStack
        } catch {
          case e: MappingException => None :: argStack
        }
    }

    def fieldValue(json: JValue, path: String) = (json \ path) match {
      case JField(_, value) => value
      case x => fail("Expected JField but got " + x + ", json='" + json + "', path='" + path + "'")
    }

    build(json, mapping, Nil).head.asInstanceOf[A]
  }

  private def mappingOf(clazz: Class[_]) = {
    import Reflection._

    def makeMapping(path: Option[String], clazz: Class[_], isList: Boolean): Mapping = isList match {
      case false => Constructor(path, clazz.getDeclaredConstructors()(0), constructorArgs(clazz))
      case true if primitive_?(clazz) => ListOfPrimitives(path.get, clazz)
      case true => ListConstructor(path.get, clazz.getDeclaredConstructors()(0), constructorArgs(clazz))
    }

    def constructorArgs(clazz: Class[_]) = clazz.getDeclaredFields.filter(!static_?(_)).map { x =>
      fieldMapping(x.getName, x.getType, x.getGenericType)
    }.toList.reverse

    def fieldMapping(name: String, fieldType: Class[_], genericType: Type): Mapping = 
      if (primitive_?(fieldType)) Value(name, fieldType)
      else if (fieldType == classOf[List[_]]) makeMapping(Some(name), typeParameter(genericType), true)
      else if (classOf[Option[_]].isAssignableFrom(fieldType))
        Optional(fieldMapping(name, typeParameter(genericType), null)) // FIXME is it possible to find out the next genericType here?
      else makeMapping(Some(name), fieldType, false)
    memo.memoize(clazz, makeMapping(None, _, false))
  }

  private def convert(value: JValue, targetType: Class[_], formats: Formats): Any = value match {
    case JInt(x) if (targetType == classOf[Int]) => x.intValue
    case JInt(x) if (targetType == classOf[Long]) => x.longValue
    case JInt(x) if (targetType == classOf[Short]) => x.shortValue
    case JInt(x) if (targetType == classOf[Byte]) => x.byteValue
    case JInt(x) if (targetType == classOf[String]) => x.toString
    case JDouble(x) if (targetType == classOf[Float]) => x.floatValue
    case JDouble(x) if (targetType == classOf[String]) => x.toString
    case JString(s) if (targetType == classOf[Date]) => formats.dateFormat.parse(s).getOrElse(fail("Invalid date '" + s + "'"))
    case JNull => null
    case JNothing => fail("Did not find value which can be converted into " + targetType.getName)
    case _ => value.values
  }

  private def fail(msg: String) = throw new MappingException(msg)

  class Memo[A, R] {
    private var cache = Map[A, R]()

    def memoize(x: A, f: A => R): R = synchronized {
      if (cache contains x) cache(x) else {
        val ret = f(x)
        cache += (x -> ret)
        ret
      }
    }
  }

  object Reflection {
    import java.lang.reflect._

    val primitives = Set[Class[_]](classOf[String], classOf[Int], classOf[Long], classOf[Double], 
                                   classOf[Float], classOf[Byte], classOf[BigInt], classOf[Boolean], 
                                   classOf[Short], classOf[java.lang.Integer], classOf[java.lang.Long], 
                                   classOf[java.lang.Double], classOf[java.lang.Float], 
                                   classOf[java.lang.Byte], classOf[java.lang.Boolean], 
                                   classOf[java.lang.Short], classOf[Date])

    def typeParameter(t: Type): Class[_] = {
      val ptype = t.asInstanceOf[ParameterizedType]
      ptype.getActualTypeArguments()(0).asInstanceOf[Class[_]]
    }

    def primitive_?(clazz: Class[_]) = primitives contains clazz
    def static_?(f: Field) = Modifier.isStatic(f.getModifiers)
  }
}

class MappingException(msg: String, cause: Exception) extends Exception(msg, cause) {
  def this(msg: String) = this(msg, null)
}

