package de.tudo.sse.classfilefeatures.webapi.model

import de.tudo.sse.classfilefeatures.webapi.model.requests.RequestsJsonSupport

trait JsonSupport extends AnalysisInformationReprJsonSupport
  with EntityReprJsonSupport
  with AnalysisRunReprJsonSupport
  with RequestsJsonSupport
  with AnalysisResultReprJsonSupport
  with AnalysisResultFormatReprJsonSupport
