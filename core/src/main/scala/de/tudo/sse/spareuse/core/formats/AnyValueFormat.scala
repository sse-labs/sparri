package de.tudo.sse.spareuse.core.formats

import spray.json.{JsNumber, JsString, JsValue}

trait AnyValueFormat {

  def isValid(jsonValue: JsValue): Boolean

  def formatDescription(ident: Int = 0): String

  def isBaseValue: Boolean = false
}


case class NamedPropertyFormat(propertyName: String,
                               propertyFormat: AnyValueFormat,
                               explanation: String = "") extends AnyValueFormat{

  override def isValid(jsonValue: JsValue): Boolean = jsonValue match {
    case _ => true
  }

  override def formatDescription(indent: Int = 0): String = {
    val currIndent = "\t"*indent
    val extra = if(explanation.nonEmpty && !explanation.isBlank) s", that contains $explanation." else "."
    s"A property with name $propertyName$extra The property is formatted as: \n$currIndent\t${propertyFormat.formatDescription(indent + 1)}"
  }

}


trait BaseValueFormat extends AnyValueFormat {

  override def isBaseValue: Boolean = true

}

case object EntityReferenceFormat extends BaseValueFormat {


  override def isValid(jsonValue: JsValue): Boolean = jsonValue match {
    case _ :JsString => true
    case _ => false
  }

  override def formatDescription(indent: Int = 0): String = "A text value referencing the UID of a software entity"

}

case object StringFormat extends BaseValueFormat {

  override def isValid(jsonValue: JsValue): Boolean = jsonValue match {
    case _ :JsString => true
    case _ => false
  }

  override def formatDescription(indent: Int = 0): String = "A text value"

}

case object NumberFormat extends BaseValueFormat {

  override def isValid(jsonValue: JsValue): Boolean = jsonValue match {
    case _ :JsNumber => true
    case _ => false
  }

  override def formatDescription(indent: Int = 0): String = "A numeric value"

}

case object EmptyFormat extends BaseValueFormat {

  override def isValid(jsonValue: JsValue): Boolean = true

  override def formatDescription(indent: Int = 0): String = "An empty value"

}
