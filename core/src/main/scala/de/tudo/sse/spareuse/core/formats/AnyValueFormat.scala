package de.tudo.sse.spareuse.core.formats

import spray.json.{JsNumber, JsString, JsValue}

trait AnyValueFormat {

  def isValid(jsonValue: JsValue): Boolean

  def formatDescription(): String

  def isBaseValue: Boolean = false
}


case class NamedPropertyFormat(propertyName: String,
                               propertyFormat: AnyValueFormat,
                               explanation: String = "") extends AnyValueFormat{

  override def isValid(jsonValue: JsValue): Boolean = jsonValue match {
    case _ => true
  }

  override def formatDescription(): String = s"A property with name $propertyName, that contains $explanation. The property is formatted as: \n\t${propertyFormat.formatDescription()}"

}


trait BaseValueFormat extends AnyValueFormat {

  override def isBaseValue: Boolean = true

}

case object EntityReferenceFormat extends BaseValueFormat {


  override def isValid(jsonValue: JsValue): Boolean = jsonValue match {
    case _ :JsString => true
    case _ => false
  }

  override def formatDescription(): String = "A text value referencing the UID of a software entity"

}

case object StringFormat extends BaseValueFormat {

  override def isValid(jsonValue: JsValue): Boolean = jsonValue match {
    case _ :JsString => true
    case _ => false
  }

  override def formatDescription(): String = "A text value"

}

case object NumberFormat extends BaseValueFormat {

  override def isValid(jsonValue: JsValue): Boolean = jsonValue match {
    case _ :JsNumber => true
    case _ => false
  }

  override def formatDescription(): String = "A numeric value"

}

case object EmptyFormat extends BaseValueFormat {

  override def isValid(jsonValue: JsValue): Boolean = true

  override def formatDescription(): String = "An empty value"

}
