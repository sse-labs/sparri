package org.anon.spareuse.core

package object utils {
  def toHex(bytes: Array[Byte]): String = bytes.map("%02X" format _).mkString

  def fromHex(hexString: String): Array[Byte] = BigInt(hexString, 16).toByteArray

}
