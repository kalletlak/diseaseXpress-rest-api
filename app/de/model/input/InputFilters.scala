package de.model.input

// ===========================================================================
case class InputFilters( // TODO: subclass instead
  ref_id:    Option[GeneQueryRef] = None,
  study_id:  Option[Seq[String]]  = None,
  sample_id: Option[Seq[String]]  = None)


// ===========================================================================
