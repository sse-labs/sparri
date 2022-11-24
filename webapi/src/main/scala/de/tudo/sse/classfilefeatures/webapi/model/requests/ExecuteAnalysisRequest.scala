package de.tudo.sse.classfilefeatures.webapi.model.requests

final case class ExecuteAnalysisRequest(Inputs: Array[String], Configuration: String, User: Option[String])
