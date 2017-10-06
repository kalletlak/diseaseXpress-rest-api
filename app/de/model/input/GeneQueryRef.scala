package de.model.input

// ===========================================================================  
sealed trait GeneQueryRef { val ref_id: Seq[String] }
  case class GeneIdQuery      (override val ref_id: Seq[String]) extends GeneQueryRef
  case class GeneSymbolQuery  (override val ref_id: Seq[String]) extends GeneQueryRef
  case class TranscriptIdQuery(override val ref_id: Seq[String]) extends GeneQueryRef

// ===========================================================================