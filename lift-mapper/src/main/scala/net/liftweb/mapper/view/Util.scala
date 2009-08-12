package net.liftweb.mapper.view

import net.liftweb.mapper.{Mapper,
                           MappedField
}

import net.liftweb.util.{Full, Box, Helpers, BindHelpers}

import Helpers._


import scala.xml.{NodeSeq, Elem}


/**
 * Provides a number of methods that make complex Mapper-based view snippets
 * easier to build.
 * @author nafg
 */
object Util {
  /**
   * Binds all nodes whose names are names of fields on the specified mapper.
   * This makes it unnecessary to write repetitious bindings like
   *    "field1" -> field1.toForm,
   *    "field2" -> field2.toform
   * Instead it automates such bindings but you have to pass it a function
   * that will generate a NodeSeq from the field, e.g.,
   *    (f: MappedField[_,_]) => f.toForm
   * Usage: Pass as a Full Box to the bind overload that takes a nodeFailureXform
   * argument.
   */
  def bindFields[T <: Mapper[T]](mapper: T, nsfn: MappedField[_,T]=>NodeSeq): NodeSeq=>NodeSeq = {
    case xml.Elem(_, name, _, _, _*) => 
      mapper.fieldByName(name) match {
        case _: net.liftweb.util.EmptyBox[_] =>
          xml.Group(Seq.empty)
        case Full(field) =>
          nsfn(field)
      }
    case ns => ns
  }
  
  /**
   * Iterates over the fields of the specified mapper. If the node currently being processed by bind
   * has an attribute "fields" then it is taken as a whitespace-delimited list of fields to iterate
   * over; otherwise all form fields are used. The specified function returns a BindParam for doing
   * processing specific to that field.
   * Returns a bind function (NodeSeq=>NodeSeq) that can be used to bind an xml node that should be
   * repeated for each field.
   * Usage: if you want to repeat xml markup for each field, the view should use the "field:" prefix
   * for field-specific nodes. The snippet should bind the containing (repeating) node to the function
   * returned by this method, passing this method the mapper instance whose fields should be used and
   * a function that returns BindParams to process the "field:" prefixed nodes.
   */
  def eachField[T<:net.liftweb.mapper.Mapper[T]](mapper: T, fn:MappedField[_,T]=>Seq[BindParam]):NodeSeq=>NodeSeq = (ns: NodeSeq) => BindHelpers.attr("fields") match {
    case Some(fields) =>
      NodeSeq.fromSeq(
        fields.text.split("\\s+").flatMap {f =>
            val params = mapper.fieldByName(f.toString)
            bind("field", ns, fn(params.open_!) : _*)
          }
      )
    case None =>
      NodeSeq.fromSeq(
        mapper.formFields.flatMap { case f: MappedField[_,T] =>
          bind("field",ns, fn(f): _*)
        }
      )
  }

  class BindableNodeSeq(ns: NodeSeq) {
    def bind(prefix: String, bindParams: BindParam*) = Helpers.bind(prefix, ns, bindParams: _*)
    def bind(prefix: String, nodeFailureXform: Box[NodeSeq => NodeSeq],
             paramFailureXform: Box[xml.PrefixedAttribute => xml.MetaData], bindParams: BindParam*) =
      Helpers.bind(prefix, nodeFailureXform, paramFailureXform, ns, bindParams: _*)
  }

  /**
   * Can be used to support a bind-chaining syntax, for use with multiple prefixes.
   * For example:
   * <pre>
   *  xhtml.bind("prefix1",
   *    bindParam1,
   *    bindParam2
   *  ).bind("prefix2",
   *    bindParam1,
   *    bindParam2
   *  )
   * Where bindParam can be the usual arrow -> syntax or any BindParam.
   * Can also be used with the bind overload that takes nodeFailureXform
   * and paramFailureXform arguments.
   * Just import this method, or Util._
   */
  implicit def nodeSeqToBindable(ns: NodeSeq): BindableNodeSeq = new BindableNodeSeq(ns)
  
  
  /**
   * If you want all your attributes specified in the view carried
   * over to the result of bind without prefixing them, use keepAttrs.
   * For example, say you bind &lt;my:text /&gt; to SHtml.text(...).
   * You can either pass attributes to SHtml.text as tuples in you code,
   * or put the attributes in the view, prefixed with lift:
   * lift:id, lift:size, lift:maxlength...
   * With keepAttrs, you can write the attributes as usual without the
   * prefix. Just wrap the call to SHtml.text with keepAttrs(...)
   * and all attributes in the view will be applied to the bound result.
   */
  def keepAttrs(elem: Elem) = (ns: NodeSeq) =>
    BindHelpers.currentNode match {
      case Full(inElem) =>
        elem % inElem.attributes
      case other =>
        elem
    }
}

