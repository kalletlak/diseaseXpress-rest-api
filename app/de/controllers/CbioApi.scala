package de.controllers

import de.Context
import de.model.Error
import de.model.input.InputFilters
import de.utils.Enums.Projection
import de.utils.LoggingAction
import de.validators.{ GeneIdFilters, GeneSymbolFilters, PrimaryIdsValidator, TranscriptIdFilters }
import play.api.Configuration
import play.api.libs.json.{ JsValue, Json }
import play.api.mvc.{ Controller, RequestHeader, Result }
import play.api.libs.json.JsArray
import play.api.libs.json.JsString
import de.model.output.GeneInfo
import de.repository.SamplesRepository
import de.model.tags.TumorSample
import play.api.libs.json.JsObject
import play.api.libs.json.JsNumber


case class Identifier(entityId:String, studyId:String)

object Identifier{
    implicit val JsonReadsIdentifier   = Json.reads [Identifier]
    implicit val JsonWritesIdentifier   = Json.writes [Identifier]
}
class CbioApi @javax.inject.Inject() (
  configuration: play.api.Configuration)
    extends Controller {
  
  private def getEntrezId(gene:GeneInfo):String = {
    val entrezIds = gene.transcripts
                        .flatMap { x => x.entrez_ids }
                        .flatten
    return entrezIds(0)
    
  }

  def getGenes(
    geneIdType: String,
    projection: String) =
    LoggingAction(bodyParser = parse.json) {
      implicit request =>

        val json: JsValue = request.body

        val gene_symbols = json match {
          case obj: JsArray => obj
                                  .value
                                  .map {
                                    case obj: JsString => Some(obj.value)
                                    case _             => None
                                  }
          case _            => Seq()
        }

        val result = GeneSymbolFilters(gene_symbols
                                            .filter { _.isDefined }
                                            .map { _.get })
                                            
                                            
                                            
                                            

        result match {
          case Left(errorObject) =>
            BadRequest(Json.toJson(errorObject.formatJson))

          case Right(genes) => {
            
            Ok(Json.toJson(genes.map { gene => Json.obj("entrezGeneId"-> getEntrezId(gene),
                                                    "hugoGeneSymbol"-> gene.gene_symbol,
                                                    "type"-> gene.biotype,
                                                    "cytoband"-> gene.chr,//wrong mapping
                                                    "length"-> (gene.end-gene.start),
                                                    "chromosome"-> gene.chr) }))
          }
        }
    }

  
    def getStudies(
    projection: String) =
    LoggingAction(bodyParser = parse.json) {
      implicit request =>

        val json: JsValue = request.body

        val inputStudies = json match {
          case obj: JsArray => obj
                                  .value
                                  .map {
                                    case obj: JsString => Some(obj.value)
                                    case _             => None
                                  }
          case _            => Seq()
        }
        
        val validStudies = SamplesRepository.getStudies
        
        val invalidStudies = inputStudies.filter { study => study.isDefined && !validStudies.contains(study.get) }
        
        if(!invalidStudies.isEmpty){
          BadRequest("Invalid studies in request")
        } else {
          //hardcoded samples count
          val studySamples = Map("PNOC" -> 27, "TCGA" -> 10661, "TARGET" -> 721, "GTEx" -> 7859, "CBTTC" -> 268)
  
          val result = inputStudies
                      .filter { _.isDefined }
                      .map { _.get}
                      .map { study => Json.obj( "name"-> study,
                                                "shortName"-> study,
                                                "description"->study,
                                                "publicStudy"->true,
                                                "groups"->"PUBLIC",
                                                "status"->0,
                                                "allSampleCount"->studySamples.get(study),
                                                "sequencedSampleCount"->0,
                                                "cnaSampleCount"->0,
                                                "mrnaRnaSeqSampleCount"->0,
                                                "mrnaRnaSeqV2SampleCount"->studySamples.get(study),
                                                "mrnaMicroarraySampleCount"->0,
                                                "miRnaSampleCount"->0,
                                                "methylationHm27SampleCount"->0,
                                                "rppaSampleCount"->0,
                                                "completeSampleCount"->0,
                                                "studyId"->study,
                                                "cancerTypeId"->study) }
                      
           Ok(Json.toJson(result))
        }
        
    }
    
      
    def getClinicalAttributes() =
    LoggingAction(bodyParser = parse.json) {
      implicit request =>

        val json: JsValue = request.body

        val inputStudies = json match {
          case obj: JsArray => obj
                                  .value
                                  .map {
                                    case obj: JsString => Some(obj.value)
                                    case _             => None
                                  }
          case _            => Seq()
        }
        
        val validStudies = SamplesRepository.getStudies
        
        val invalidStudies = inputStudies.filter { study => study.isDefined && !validStudies.contains(study.get) }
        
        if(!invalidStudies.isEmpty){
          BadRequest("Invalid studies in request")
        } else {
          val studies = inputStudies
                      .filter { _.isDefined }
                      .map { _.get}
          
          val attributes = scala.collection.mutable.Map[String, JsObject]()
                      
          val samples = SamplesRepository.getSamplesInfo(studies)
  
          samples.foreach { sample =>
            {
              
              sample.getAllTagsAsMap.foreach { obj =>
                {
                  val key = obj._1+"_"+sample.study_id
                  if(!attributes.contains(key)){
                    val datatype = obj._2 match {
                        case value:JsNumber => "NUMBER"
                        case _ => "STRING"
                    }
                    attributes(key) = Json.obj( "displayName"         -> obj._1,
                                                "description"         -> obj._1,
                                                "datatype"            -> datatype,
                                                "patientAttribute"    -> true,
                                                "priority"            -> "1",
                                                "clinicalAttributeId" -> obj._1,
                                                "studyId"             -> sample.study_id.value)
                  }
                  
                }
              }
            }
          }
          
          Ok(Json.toJson(attributes.values))          
        }
        
    }
    
    
    def getMolecularProfiles() =
    LoggingAction(bodyParser = parse.json) {
      implicit request =>

        val json: JsValue = request.body

        val inputStudies = json match {
          case obj: JsObject => {
            val studies = obj.value.get("studyIds")
            if(studies.isDefined) {
              studies.get match {
                case obj: JsArray => obj
                                        .value
                                        .map {
                                          case obj: JsString => Some(obj.value)
                                          case _             => None
                                        }
                case _            => Seq()
              }
            } else {
              Seq()
            }
          }
          case _            => Seq()
        }
        
        val validStudies = SamplesRepository.getStudies
        
        val invalidStudies = inputStudies.filter { study => study.isDefined && !validStudies.contains(study.get) }
        
        if(!invalidStudies.isEmpty){
          BadRequest("Invalid studies in request")
        } else {
          val molecularProfiles = 
            inputStudies
                      .filter { _.isDefined }
                      .map { _.get}
                      .map { study => Json.obj("molecularAlterationType"  -> "MRNA_EXPRESSION",
                                          "datatype"                 -> "Z-SCORE",
                                          "name"                     -> "mRNA Expression z-Scores (RNA Seq V2 RSEM)",
                                          "description"              -> "mRNA z-Scores (RNA Seq V2 RSEM) compared to the expression distribution of each gene tumors that are diploid for this gene.",
                                          "showProfileInAnalysisTab" -> true,
                                          "molecularProfileId"       -> (study+"_seq_v2_mrna_median_Zscores"),
                                          "studyId"                  -> study) }
                      
  
          Ok(Json.toJson(molecularProfiles))
        }
        

    }
     
    def getSampleIds(listId:String) =
    LoggingAction(bodyParser = parse.json) {
      implicit request =>

        val validStudies = SamplesRepository.getStudies
        
        val inputStudies = validStudies.filter { study => listId.equals(study+"_all") }
        
        if(inputStudies.isEmpty || inputStudies.length>1) {
          BadRequest("Invalid request")
        } else {
          val samples = SamplesRepository
                  .getSamplesInfo(inputStudies)
                  .map { sample =>
                    sample.sample_barcode match {
                      case Left(sample_barcode) => sample_barcode.value
                      case _                    => sample.sample_id.value
                    }
                  }
        
          Ok(Json.toJson(samples))
        }
    }
    
    
    def getSamples() =
    LoggingAction(bodyParser = parse.json) {
      implicit request =>
        
        val json: JsValue = request.body

        val sampleListIds = (json match {
          case obj: JsObject => {
            val _sampleListIds = obj.value.get("sampleListIds")
            if(_sampleListIds.isDefined) {
              _sampleListIds.get match {
                case obj: JsArray => obj
                                        .value
                                        .map {
                                          case obj: JsString => Some(obj.value)
                                          case _             => None
                                        }
                case _            => Seq()
              }
            } else {
              Seq()
            }
          }
          case _            => Seq()
        }).flatten

        val validStudies = SamplesRepository.getStudies
        
        val inputStudies = validStudies.filter { study => sampleListIds.contains(study+"_all") }
        
        if(inputStudies.isEmpty){
          BadRequest("Invalid request")
        } else {
          val samples = SamplesRepository
                         .getSamplesInfo(inputStudies)
                         .map { sample => {
                           val sampleId = sample.sample_barcode match {
                              case Left(sample_barcode) => sample_barcode.value
                              case _                    => sample.sample_id.value
                           }
                           val patientId = sample.patient_barcode match {
                              case Left(patient_barcode) => patient_barcode.value
                              case _                    => sampleId
                           }
                           val studyId = sample.study_id.value
                           Json.obj("uniqueSampleKey"  -> sampleId,
                                    "uniquePatientKey" -> patientId,
                                    "sampleId"         -> sampleId,
                                    "patientId"        -> patientId,
                                    "cancerTypeId"     -> studyId,
                                    "studyId"          -> studyId) 
                           }
                         }
        
        Ok(Json.toJson(samples))
          
        }
    }
    
    def getPatients() =
    LoggingAction(bodyParser = parse.json) {
      implicit request =>
        
        val json: JsValue = request.body

        val inputPatientKeys = (json match {
          case obj: JsObject => {
            val _patientKeys = obj.value.get("uniquePatientKeys")
            if(_patientKeys.isDefined) {
              _patientKeys.get match {
                case obj: JsArray => obj
                                        .value
                                        .map {
                                          case obj: JsString => Some(obj.value)
                                          case _             => None
                                        }
                case _            => Seq()
              }
            } else {
              Seq()
            }
          }
          case _            => Seq()
        }).flatten
        
        val patientKeySet = SamplesRepository.getSamplesInfo().map { sample => {
          
          val sampleId = sample.sample_barcode match {
                              case Left(sample_barcode) => sample_barcode.value
                              case _                    => sample.sample_id.value
                           }
          
          val patientId = sample.patient_barcode match {
                              case Left(patient_barcode) => patient_barcode.value
                              case _                    => sampleId
                           }
          patientId->sample }
        }.toMap
        
        val invalidPatientKeys = inputPatientKeys.filter { patientKey => !patientKeySet.contains(patientKey) }
        
        
        if(!invalidPatientKeys.isEmpty) {
          BadRequest("Invalid request")
        } else {
          val patients = inputPatientKeys.map { patientKey => {
            val patient = patientKeySet.get(patientKey).get
            Json.obj("uniquePatientKey" -> patientKey,
                     "patientId"        -> patientKey,
                     "studyId"          -> patient.study_id.value) 
            }
          }
        
          Ok(Json.toJson(patients))
        }
    }
    
    def getClinicalData(studyId:String,clinicalDataType:String) =
    LoggingAction(bodyParser = parse.json) {
      implicit request =>

        val json: JsValue = request.body

        val attributeIds = (json match {
          case obj: JsObject => {
            val _attributeIds = obj.value.get("attributeIds")
            if (_attributeIds.isDefined) {
              _attributeIds.get match {
                case obj: JsArray => obj
                  .value
                  .map {
                    case obj: JsString => Some(obj.value)
                    case _             => None
                  }
                case _ => Seq()
              }
            } else {
              Seq()
            }
          }
          case _ => Seq()
        }).flatten

        val sampleIds = (json match {
          case obj: JsObject => {
            val _ids = obj.value.get("ids")
            if (_ids.isDefined) {
              _ids.get match {
                case obj: JsArray => obj
                  .value
                  .map {
                    case obj: JsString => Some(obj.value)
                    case _             => None
                  }
                case _ => Seq()
              }
            } else {
              Seq()
            }
          }
          case _ => Seq()
        }).flatten
        
        if(attributeIds.isEmpty || sampleIds.isEmpty) {
          BadRequest("Invalid request")
        } else {
          val filteredSamples = SamplesRepository.getSamplesInfo().filter { sampleObj =>
            {
              val sampleId = sampleObj.sample_barcode match {
                case Left(sample_barcode) => sample_barcode.value
                case _                    => sampleObj.sample_id.value
              }

              sampleIds.contains(sampleId)
            }
          }

          val clinicalData = filteredSamples.flatMap { sample =>
            {
              val sampleAttributeData = sample.getAllTagsAsMap
              val sampleId = sample.sample_barcode match {
                case Left(sample_barcode) => sample_barcode.value
                case _                    => sample.sample_id.value
              }
              val patientId = sample.patient_barcode match {
                case Left(patient_barcode) => patient_barcode.value
                case _                     => sampleId
              }
              attributeIds
                .map { attributeId => attributeId -> sampleAttributeData.get(attributeId) }
                .filter { case (key, value) => value.isDefined }
                .map {
                  case (key, value) => Json.obj("uniqueSampleKey" -> sampleId,
                    "uniquePatientKey" -> patientId,
                    "sampleId" -> sampleId,
                    "patientId" -> patientId,
                    "studyId" -> sample.study_id.value,
                    "clinicalAttributeId" -> key,
                    "value" -> value.get)
                }
            }
          }

          Ok(Json.toJson(clinicalData))
        }
    }
    
    def getSamplesFromListId(sampleListId:String) =
    LoggingAction {
      implicit request =>
        
        val validSampleLists = SamplesRepository.getStudies.map { study => study+"_all" }
        
        if(validSampleLists.contains(sampleListId)) {
          val studyId = sampleListId.substring(0,sampleListId.indexOf("_all"))
          Ok(Json.toJson(SamplesRepository.getSamplesIds(Seq(studyId))))
          
        } else {
          BadRequest("Invalid request")
        }
    }
    
    def getsampleList(sampleListId:String) =
    LoggingAction {
      implicit request =>
        
        val validSampleLists = SamplesRepository.getStudies.map { study => study+"_all" }
        
        if(validSampleLists.contains(sampleListId)) {
          val studyId = sampleListId.substring(0,sampleListId.indexOf("_all"))
          val sampleIds = SamplesRepository.getSamplesIds(Seq(studyId))
          val noOfSamples = sampleIds.length
          
          Ok(Json.toJson(Json.obj("category"     -> "all_cases_in_study",
                                  "name"         -> "All Samples",
                                  "description"  -> s"""All tumor samples ($noOfSamples samples)""",
                                  "sampleCount"  -> noOfSamples,
                                  "sampleIds"    -> sampleIds,
                                  "sampleListId" -> sampleListId,
                                  "studyId"      -> studyId)))
          
        } else {
          BadRequest("Invalid request")
        }
    }
    
    def getStudy(studyId:String) =
    LoggingAction {
      implicit request =>
        
        if(SamplesRepository.getStudies.contains(studyId)) {
          val sampleIds = SamplesRepository.getSamplesIds(Seq(studyId))
          val noOfSamples = sampleIds.length
          
          Ok(Json.toJson(Json.obj("name"-> studyId,
                                  "shortName"-> studyId,
                                  "description"->studyId,
                                  "publicStudy"->true,
                                  "groups"->"PUBLIC",
                                  "status"->0,
                                  "allSampleCount"->noOfSamples,
                                  "sequencedSampleCount"->0,
                                  "cnaSampleCount"->0,
                                  "mrnaRnaSeqSampleCount"->0,
                                  "mrnaRnaSeqV2SampleCount"->noOfSamples,
                                  "mrnaMicroarraySampleCount"->0,
                                  "miRnaSampleCount"->0,
                                  "methylationHm27SampleCount"->0,
                                  "rppaSampleCount"->0,
                                  "completeSampleCount"->0,
                                  "studyId"->studyId,
                                  "cancerTypeId"->studyId,
                                  "cancerType"->Json.obj("name"->studyId,
                                                          "clinicalTrialKeywords"->studyId,
                                                          "dedicatedColor"->"Gray",
                                                          "shortName"->studyId,
                                                          "parent"->"tissue",
                                                          "cancerTypeId"->studyId))))
          
        } else {
          BadRequest("Invalid request")
        }
    }
    
    def getClinicalDataByIdentitiers(clinicalDataType:String) =
    LoggingAction(bodyParser = parse.json) {
      implicit request =>

        val json: JsValue = request.body

        val attributeIds = (json match {
          case obj: JsObject => {
            val _attributeIds = obj.value.get("attributeIds")
            if (_attributeIds.isDefined) {
              _attributeIds.get match {
                case obj: JsArray => obj
                  .value
                  .map {
                    case obj: JsString => Some(obj.value)
                    case _             => None
                  }
                case _ => Seq()
              }
            } else {
              Seq()
            }
          }
          case _ => Seq()
        }).flatten

        val identifiers:Seq[Identifier] = json match {
          case obj: JsObject => {
            val _ids = obj.value.get("identifiers")
            if (_ids.isDefined) {
              _ids.get match {
                case obj: JsArray => {
                  obj
                    .value
                    .map { value => value.as[Identifier] }
                }
                case _ => Seq()
              }
            } else {
              Seq()
            }
          }
          case _ => Seq()
        }
        
        val sampleIds = identifiers.map { _.entityId }
        
        if(attributeIds.isEmpty || sampleIds.isEmpty) {
          BadRequest("Invalid request")
        } else {
          val filteredSamples = SamplesRepository.getSamplesInfo().filter { sampleObj =>
            {
              val sampleId = sampleObj.sample_barcode match {
                case Left(sample_barcode) => sample_barcode.value
                case _                    => sampleObj.sample_id.value
              }

              sampleIds.contains(sampleId)
            }
          }

          val clinicalData = filteredSamples.flatMap { sample =>
            {
              val sampleAttributeData = sample.getAllTagsAsMap
              val sampleId = sample.sample_barcode match {
                case Left(sample_barcode) => sample_barcode.value
                case _                    => sample.sample_id.value
              }
              val patientId = sample.patient_barcode match {
                case Left(patient_barcode) => patient_barcode.value
                case _                     => sampleId
              }
              attributeIds
                .map { attributeId => attributeId -> sampleAttributeData.get(attributeId) }
                .filter { case (key, value) => value.isDefined }
                .map {
                  case (key, value) => Json.obj("uniqueSampleKey" -> sampleId,
                    "uniquePatientKey" -> patientId,
                    "sampleId" -> sampleId,
                    "patientId" -> patientId,
                    "studyId" -> sample.study_id.value,
                    "clinicalAttributeId" -> key,
                    "value" -> value.get)
                }
            }
          }

          Ok(Json.toJson(clinicalData))
        }
    }
    
}