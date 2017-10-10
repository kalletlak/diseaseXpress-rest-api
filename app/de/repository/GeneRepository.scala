package de.repository

import de.model.output.GeneInfo
import de.validators.{GeneIdQuery, GeneSymbolQuery, TranscriptIdQuery}
import de.dao.GeneDAO
import de.validators.PrimaryIdRef

object GeneRepository {
  
  private val dao = GeneDAO
  def getGeneInputRef(geneInputRef: PrimaryIdRef): Seq[GeneInfo] = {
    geneInputRef
      match {
      
        case gene: GeneIdQuery =>
          gene
            .ref_id
            .flatMap(dao.getGeneById)
        
        case gene: GeneSymbolQuery =>
          gene
            .ref_id
            .flatMap { dao.getGeneBySymbol }

        case gene: TranscriptIdQuery =>
          gene
            .ref_id
            .flatMap {dao.getTranscriptId}        
        
      }
    }

  // ---------------------------------------------------------------------------    
  def getGeneById  (gene_id: String):       Option[GeneInfo] = dao.getGeneById(gene_id)
  def getTranscriptId(transcript_id: String): Option[GeneInfo] = dao.getTranscriptId(transcript_id)
  
  def isGeneIdPresent(gene_id: String): Boolean = getGeneById(gene_id).isDefined
  def isGeneSymbolPresent(gene_symbol: String): Boolean = dao.getGeneBySymbol(gene_symbol).size>0
  def isTranscriptIdPresent(transcript_id: String): Boolean = getTranscriptId(transcript_id).isDefined
  
  def getGeneIds       = dao.getGeneIds
  def getGeneSymbols   = dao.getGeneSymbols
  def getTranscriptIds = dao.getTranscriptIds
  
}