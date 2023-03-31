package de.tudo.sse.spareuse.eval.studies.evometrics

case class EvolutionMetricsReport(ga: String, v1: String, v2: String, is: Double, es: Double, ie: Double, ee: Double) {
  def stability: Double = (es + is) / 2.0
  def evolution: Double = (ee + ie) / 2.0
}
