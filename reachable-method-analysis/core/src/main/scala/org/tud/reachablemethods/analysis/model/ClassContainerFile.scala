package org.tud.reachablemethods.analysis.model

import org.tud.reachablemethods.analysis.model.ClassContainerFile.isClassContainerFile
import org.tud.reachablemethods.analysis.model.ClassContainerType.ClassContainerType
import org.tud.reachablemethods.analysis.model.ClassList.ClassList

import java.io.File
import java.net.URL
import scala.util.{Success, Try}

class ClassContainerFile(val containerFile: File) {

  private var containedClassFilesTry: Option[Try[ClassList]] = None

  def isValid: Boolean = {
    isClassContainerFile(containerFile) && containerFile.exists()
  }

  def isJar: Boolean ={
    Try(getContainerType).map( _ == ClassContainerType.JAR).getOrElse(false)
  }

  def isJMod: Boolean ={
    Try(getContainerType).map( _ == ClassContainerType.JMOD).getOrElse(false)
  }

  def getUrl: URL = containerFile.toURI.toURL

  def getContainerType: ClassContainerType = {

    if(isValid){
      if(containerFile.getName.toLowerCase.endsWith(".jar")) ClassContainerType.JAR
      else ClassContainerType.JMOD
    } else {
      throw new IllegalStateException("Not a class container: " + containerFile.getPath)
    }

  }

  def getClassList(loadImplementation: Boolean): Try[ClassList] = {
    if(containedClassFilesTry.isEmpty){
      containedClassFilesTry = Some(ClassList.readFromClassContainer(this, loadImplementation))
    }

    containedClassFilesTry.get
  }

}

object ClassContainerFile {

  def isClassContainerFile(file: File): Boolean =
    file != null && (file.getName.toLowerCase.endsWith(".jar") || file.getName.toLowerCase.endsWith(".jmod"))

  def apply(containerFile: File): ClassContainerFile = new ClassContainerFile(containerFile)
}

object ClassContainerType extends Enumeration {

  val JAR, JMOD = Value

  type ClassContainerType = Value
}
