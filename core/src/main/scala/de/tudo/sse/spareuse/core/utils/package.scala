package de.tudo.sse.spareuse.core

package object utils {
  def toHex(bytes: Array[Byte]): String = bytes.map("%02X" format _).mkString
}
