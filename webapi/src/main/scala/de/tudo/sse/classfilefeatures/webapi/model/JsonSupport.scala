package de.tudo.sse.classfilefeatures.webapi.model

import de.tudo.sse.classfilefeatures.webapi.model.requests.RequestsJsonSupport

trait JsonSupport extends CoreModelJsonSupport
  with LibraryInformationJsonSupport
  with ReleaseInformationJsonSupport
  with ConcreteClassInformationJsonSupport
  with RequestsJsonSupport
  with LibraryClassActivationInformationJsonSupport
  with LibraryClassInformationJsonSupport
