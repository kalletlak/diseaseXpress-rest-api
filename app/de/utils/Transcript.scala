package de.utils

// ===========================================================================
case class Transcript( // TODO: move
    chr:                String,
    strand:             String,
    gene_symbol:        String,
    gene_id:            String,
    gene_start:         Long,
    gene_end:           Long,
    gene_biotype:       String,
    transcript_id:      String,
    transcript_start:   Long,
    transcript_end:     Long,
    transcript_biotype: String,
    entrez_id:          Option[Seq[String]],
    refseq_mrna_id:     Option[Seq[String]],
    refseq_protein_id:  Option[Seq[String]])

  // ---------------------------------------------------------------------------
  object Transcript {
  
    def apply(line: String): Transcript = {
      val itr = line.split("\t", 14).iterator // TODO: sanity check
      
      Transcript(
        itr.next,
        itr.next,
        itr.next,
        itr.next,
        itr.next.toLong,
        itr.next.toLong,
        itr.next,
        itr.next,
        itr.next.toLong,
        itr.next.toLong,
        itr.next,
        toSeq(itr.next),
        toSeq(itr.next),
        toSeq(itr.next))
    }
  
    // ---------------------------------------------------------------------------
    //convert empty value as empty list
    private def toSeq(listAsString: String): Option[Seq[String]] =
      listAsString match {
      
        case "" =>
          None
          
        case nonEmptyListAsString =>
          Some(
            nonEmptyListAsString
              .split(",", -1)
              .map(_.trim)
              .toSeq)
      }    
  
  }

// ===========================================================================