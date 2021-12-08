package org.tud.reachablemethods.analysis.model

import org.opalj.br.ClassFile
import org.tud.reachablemethods.analysis.opal.OPALProjectHelper

import java.io.{File, FileInputStream, InputStream}
import java.net.URL
import scala.util.{Failure, Try}

object ClassList {

  type ClassWithURL = (ClassFile, URL)
  type ClassList = List[ClassWithURL]

  def combine(listA: ClassList, listB: ClassList): ClassList = listA ++ listB

  def readFromClassContainer(container: ClassContainerFile, loadImplementation: Boolean): Try[ClassList] = {

    if(container.isJar){
      OPALProjectHelper.readClassesFromJarStream(new FileInputStream(container.containerFile), container.getUrl, loadImplementation)
    } else if(container.isJMod){
      OPALProjectHelper.readClassesFromJmodFile(container.containerFile, loadImplementation)
    } else {
      Failure(new IllegalStateException("Class container not valid: " + container.containerFile.getPath))
    }

  }

  def readClassesFromDirectory(directory: File, loadImplementation: Boolean, recurse: Boolean): Try[ClassList] = {
    OPALProjectHelper.readClassesFromDirectory(directory, loadImplementation, recurse)
  }

  def readFromJarStream(inputStream: InputStream, sourceUrl: URL, loadImplementation: Boolean): Try[ClassList] = {
    OPALProjectHelper.readClassesFromJarStream(inputStream, sourceUrl, loadImplementation)
  }
}
