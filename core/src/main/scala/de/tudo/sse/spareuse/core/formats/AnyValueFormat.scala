package de.tudo.sse.spareuse.core.formats

trait AnyValueFormat {

  def asNamedPropertyFormat(propertyName: String): NamedPropertyFormat = NamedPropertyFormat(propertyName, this)

  def isBaseValue: Boolean = false
}

case object EntityReferenceFormat extends AnyValueFormat


case class NamedPropertyFormat(propertyName: String, propertyFormat: AnyValueFormat) extends AnyValueFormat


trait BaseValueFormat extends AnyValueFormat {

  override def isBaseValue: Boolean = true

}

case object StringFormat extends BaseValueFormat


case object NumberFormat extends BaseValueFormat

case object EmptyFormat extends BaseValueFormat
