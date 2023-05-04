package org.anon.spareuse.webapi.model

import org.anon.spareuse.webapi.model.requests.RequestsJsonSupport

trait JsonSupport extends AnalysisInformationReprJsonSupport
  with EntityReprJsonSupport
  with AnalysisRunReprJsonSupport
  with RequestsJsonSupport
  with AnalysisResultReprJsonSupport
  with AnalysisResultFormatReprJsonSupport
