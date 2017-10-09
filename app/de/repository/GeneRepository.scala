package de.repository

import de.validators.IdRef
import de.model.output.GeneInfo
import de.validators.GeneIdQuery
import de.validators.GeneSymbolQuery
import de.validators.TranscriptIdQuery
import de.dao.GeneDAO

object GeneRepository {
  
  private val dao = GeneDAO
  def getGeneInputRef(geneInputRef: IdRef): Seq[GeneInfo] = {
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
  def getTranscriptId(transcript_id: String): Option[GeneInfo] = dao.getGeneById(transcript_id)
  
  def isGeneIdPresent(gene_id: String): Boolean = getGeneById(gene_id).isDefined
  def isGeneSymbolPresent(gene_symbol: String): Boolean = dao.getGeneBySymbol(gene_symbol).size>0
  def isTranscriptIdPresent(transcript_id: String): Boolean = getTranscriptId(transcript_id).isDefined
  
  def getGeneIds       = dao.getGeneIds
  def getGeneSymbols   = dao.getGeneSymbols
  def getTranscriptIds = dao.getTranscriptIds
  
}