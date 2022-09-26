package de.tudo.sse.spareuse.core.formats

trait AnyValueFormat {

  def asNamedPropertyFormat(propertyName: String): NamedPropertyFormat = NamedPropertyFormat(propertyName, this)

  def isBaseValue: Boolean = false
}

trait BaseValueFormat extends AnyValueFormat {

  override def isBaseValue: Boolean = true

}

object StringFormat extends BaseValueFormat {
}

object NumberFormat extends BaseValueFormat
