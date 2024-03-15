## R CMD check results

0 errors | 0 warnings | 1 notes

Just the usual maintainer email address change.

## revdepcheck results

We checked 101 reverse dependencies (92 from CRAN + 9 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 8 new problems: 
 
   Andromeda: fix submitted at https://github.com/OHDSI/Andromeda/pull/63
   
   CMDConnector: fix submitted at https://github.com/darwin-eu/CDMConnector/pull/16

   CohortSurvival/DrugUtilisation/IncidencePrevalence/PatientProfiles:
   Failure due CDMConnector
   
   SCDB: fix submitted at https://github.com/ssi-dk/SCDB/pull/114
   
   diseasystore: due to SCDB
   
 * We failed to check 0 packages

The authors of failing packages have been contacted, and most have been supplied with patches. Detailed summarised at https://github.com/tidyverse/dbplyr/issues/1469
