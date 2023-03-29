package de.tudo.sse.spareuse.core.model.analysis

trait GraphResult {

  val nodes: Seq[NodeWithId]
  val edges: Seq[EdgeWithIds]

}

trait NodeWithId {
  val __uid__ : Long
}

trait EdgeWithIds {
  val __from__ : Long
  val __to__ : Long
}

