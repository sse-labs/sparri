package org.anon.spareuse.core.formats.json

import org.anon.spareuse.core.formats.{NamedPropertyFormat, NumberFormat, ObjectResultFormat, StringFormat}
import org.scalatest.funspec.AnyFunSpec
import spray.json.JsObject

class CustomObjectWriterTest extends AnyFunSpec {

  describe("The custom object writer"){
    it("should serialize case classes"){

      case class TheCaseClass(stringVal: String, intVal: Int)

      val theformat = new ObjectResultFormat(Set(
        NamedPropertyFormat("stringVal", StringFormat),
        NamedPropertyFormat("intVal", NumberFormat)
      ))

      val writer = new CustomObjectWriter(theformat)

      val result = writer.write(TheCaseClass("FooBar", 42))

      assert(result.isInstanceOf[JsObject])
      val resObj = result.asInstanceOf[JsObject]

      assert(resObj.fields.size == 2)
      assert(resObj.fields.contains("stringVal"))
      assert(resObj.fields.contains("intVal"))

    }
  }

}


