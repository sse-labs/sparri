package de.tudo.sse.spareuse.core.formats

import spray.json.{JsNumber, JsString, JsValue}

trait AnyValueFormat {

  def isValid(jsonValue: JsValue): Boolean

  def isBaseValue: Boolean = false
}


case class NamedPropertyFormat(propertyName: String,
                               propertyFormat: AnyValueFormat,
                               explanation: String = "") extends AnyValueFormat{

  override def isValid(jsonValue: JsValue): Boolean = jsonValue match {
    case _ => true
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

}

case object StringFormat extends BaseValueFormat {

  override def isValid(jsonValue: JsValue): Boolean = jsonValue match {
    case _ :JsString => true
    case _ => false
  }

}

case object NumberFormat extends BaseValueFormat {

  override def isValid(jsonValue: JsValue): Boolean = jsonValue match {
    case _ :JsNumber => true
    case _ => false
  }

}

case object EmptyFormat extends BaseValueFormat {

  override def isValid(jsonValue: JsValue): Boolean = true

}
