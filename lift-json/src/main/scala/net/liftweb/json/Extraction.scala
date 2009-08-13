package net.liftweb.json

import scala.reflect.Manifest
import JsonAST._

/** Function to extract values from JSON AST using case classes.
 *
 *  FIXME: Add support to extract List of values too.
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
  case class Constructor(path: Option[String], classname: String, args: List[Mapping]) extends Mapping
  case class ListConstructor(path: String, classname: String, args: List[Mapping]) extends Mapping

  val memo = new Memo[Class[_], Mapping]

  def extract[A](json: JValue)(implicit mf: Manifest[A]): A = 
    try {
      extract0(json, mf)
    } catch {
      case e: Exception => throw new MappingException(e)
    }

  private def extract0[A](json: JValue, mf: Manifest[A]): A = {
    val mapping = mappingOf(mf.erasure)

    def newInstance(classname: String, args: List[Any]) = {
      val clazz = Class.forName(classname)
      val argTypes = Reflection.types(args)
      clazz.getConstructor(argTypes.toArray: _*).newInstance(args.map(_.asInstanceOf[AnyRef]).toArray: _*)
    }

    def build(root: JValue, mapping: Mapping, argStack: List[Any]): List[Any] = mapping match {
      case Value(path, targetType) => convert(fieldValue(root, path), targetType) :: argStack
      case Constructor(path, classname, args) => 
        val newRoot = path match {
          case Some(p) => root \ p
          case None => root
        }
        newInstance(classname, args.flatMap(build(newRoot, _, argStack))) :: Nil
      case ListConstructor(path, classname, args) => 
        val arr = fieldValue(root, path).asInstanceOf[JArray]
        arr.arr.map(elem => newInstance(classname, args.flatMap(build(elem, _, argStack)))) :: argStack
    }

    def fieldValue(json: JValue, path: String) = (json \ path).asInstanceOf[JField].value

    build(json, mapping, Nil).head.asInstanceOf[A]
  }

  private def mappingOf(clazz: Class[_]) = {
    def makeMapping(path: Option[String], clazz: Class[_], isList: Boolean): Mapping = isList match {
      case false => Constructor(path, clazz.getName, constructorArgs(clazz))
      case true  => ListConstructor(path.get, clazz.getName, constructorArgs(clazz))
    }

    def constructorArgs(clazz: Class[_]) = clazz.getDeclaredFields.map { x =>
      if (Reflection.primitive_?(x.getType)) Value(x.getName, x.getType)
      else if (x.getType == classOf[BigInt]) Value(x.getName, x.getType)
      else if (x.getType == classOf[List[_]]) makeMapping(Some(x.getName), Reflection.parametrizedType(x), true)
      else makeMapping(Some(x.getName), x.getType, false)
    }.toList.reverse

    memo.memoize(clazz, (x: Class[_]) => makeMapping(None, x, false))
  }

  // FIXME fails if value == JNull
  private def convert(value: JValue, targetType: Class[_]): Any = value match {
    case JInt(x) if (targetType == classOf[Int]) => x.intValue
    case JInt(x) if (targetType == classOf[Long]) => x.longValue
    case JInt(x) if (targetType == classOf[Short]) => x.shortValue
    case JInt(x) if (targetType == classOf[Byte]) => x.byteValue
    case JInt(x) if (targetType == classOf[String]) => x.toString
    case JDouble(x) if (targetType == classOf[Float]) => x.floatValue
    case JDouble(x) if (targetType == classOf[String]) => x.toString
    case _ => value.values
  }

  class Memo[A, R] {
    var cache = Map[A, R]()

    def memoize(x: A, f: A => R): R = {
      if (cache contains x) cache(x) else {
        val ret = f(x)
        cache += (x -> ret)
        ret
      }
    }
  }

  object Reflection {
    import java.lang.reflect._

    def parametrizedType(f: Field): Class[_] = {
      val ptype = f.getGenericType.asInstanceOf[ParameterizedType]
      ptype.getActualTypeArguments()(0).asInstanceOf[Class[_]]
    }

    def primitive_?(clazz: Class[_]) = {
      clazz == classOf[String] || clazz == classOf[Int] || clazz == classOf[Long] ||
      clazz == classOf[Double] || clazz == classOf[Float] || clazz == classOf[Byte] ||
      clazz == classOf[Boolean] || clazz == classOf[Short]
    }

    def types(xs: List[Any]) = xs.map {
      case x: List[_] => classOf[List[_]]
      case x: Int => classOf[Int]
      case x: Long => classOf[Long]
      case x: Short => classOf[Short]
      case x: Byte => classOf[Byte]
      case x: Double => classOf[Double]
      case x: Float => classOf[Float]
      case x: Boolean => classOf[Boolean]
      case x => x.asInstanceOf[AnyRef].getClass
    }
  }
}

class MappingException(cause: Exception) extends Exception(cause)

