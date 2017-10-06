# d3b-disease-express-rest-api
Rest API for disease express

### Queries
1. Give me all sample annotation & RSEM value data where symbol is GPC2

   This is done in two api calls
   
      * ```curl -X GET 'http://disease-express.dev.cavatica-dns.org/api/v1/samples' -H 'accept: application/json'```
      * ```curl -X GET 'http://disease-express.dev.cavatica-dns.org/api/v1/data/genes/symbols/GPC2/normalizations/rsem' -H  'accept: text/tab-separated-values'```

2. Give me all TPM data where transcript_id is ENST00000292377.3 for samples that have disease is ALL

    * ```curl -X POST 'http://disease-express.dev.cavatica-dns.org/api/v1/tags/data/transcripts/ids/ENST00000292377.3/normalizations/sample_abundance,sample_rsem_isoform' -H 'accept: text/tab-separated-values'  -H "Content-Type: application/json" -d '{"$eq":{"disease": "ALL"}}'```

3. Give me all sample annotation & TPM & FPKM value data for a subset of genes (gene symbol is in GPC2, MYCN, ALK) in study TARGET that have mycn_status is amplified?

    This is done in two api calls
   
      * ```curl -X GET 'http://disease-express.dev.cavatica-dns.org/api/v1/samples' -H 'accept: application/json'```
      * ```curl -X POST 'http://disease-express.dev.cavatica-dns.org/api/v1/tags/data/genes/symbols/GPC2,MYCN,ALK/normalizations/rsem,sample_abundance,sample_rsem_isoform?projection=summary' -H 'accept: text/tab-separated-values' -H 'Content-Type: application/json' -d '{"$and":[{"$eq":{"study":"TARGET"}},{"$eq":{"mycn_status":"amplified"}}]}'```
      
4. Give me all sample annotation and RSEM value for all genes where study is ‘TCGA’ AND disease is ‘THCA’ AND disease_subtype is ‘Papillary Thyroid Cancer’ AND definition is ‘Primary Solid Tumor’ AND extrathyroid_invasion_stage is T4a.
   
   This is done in two api calls
   
      * ```curl -X GET 'http://disease-express.dev.cavatica-dns.org/api/v1/samples' -H 'accept: application/json'```
      * ```curl -X POST 'http://disease-express.dev.cavatica-dns.org/api/v1/data/normalizations/rsem?projection=summary' -H 'accept: text/tab-separated-values' -H 'Content-Type: application/json' -d '{"$and":[{"$eq":{"study":"TCGA"}},{"$eq":{"disease":"THCA"}},{"$eq":{"disease_subtype":"Papillary Thyroid Cancer"}},{"$eq":{"definition":"Primary Solid Tumor"}},{"$eq":{"extrathyroid_invasion_stage":"T4a"}}]}'```

5. Give me all samples where mycn_status is amplified AND risk is high.

    *  ```curl -X POST 'http://disease-express.dev.cavatica-dns.org/api/v1/data/normalizations/rsem,sample_abundance,sample_rsem_isoform?projection=summary' -H 'accept: text/tab-separated-values' -H 'Content-Type: application/json' -d '{"$and":[{"$eq":{"risk":"high"}},{"$eq":{"mycn_status":"amplified"}}]}'```
6. I would like to get all data where disease is ALL AND gender is male AND cns_status is 2.
    
    * ```curl -X POST 'http://disease-express.dev.cavatica-dns.org/api/v1/data/normalizations/rsem,sample_abundance,sample_rsem_isoform?projection=summary' -H 'accept: text/tab-separated-values' -H 'Content-Type: application/json' -d '{"$and":[{"$eq":{"disease":"ALL"}},{"$eq":{"gender":"male"}}, {"$eq":{"cns_status":2}}]}'```

7. Give me sample, clinical annotation and RSEM value for all genes where study in [‘TCGA’, ‘TARGET’] AND rsem  > 50 AND gender is female.
  
    *  TODO
    
8. Give me all FPKM data for all genes where disease is NBL AND definition is Primary Solid Tumor AND mycn_status is amplified.

    * ```curl -X POST 'http://disease-express.dev.cavatica-dns.org/api/v1/data/normalizations/rsem?projection=summary' -H 'accept: text/tab-separated-values' -H 'Content-Type: application/json' -d '{"$and":[{"$eq":{"disease":"NBL"}},{"$eq":{"definition":"Primary Solid Tumor"}}, {"$eq":{"mycn_status":"amplified"}}]}'```
