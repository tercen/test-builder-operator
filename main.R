library(tercen)
library(tercenApi)
library(dplyr, warn.conflicts = FALSE)
library(tim)
library(jsonlite)


read_doc_id <- function(ctx, documentId){
  
  docId <- ctx$cselect("documentId")
  
  f.names <- tim::load_data(ctx, docId[[1]] )
  
  hosp <- read.csv(f.names[1])
  
  hosp <- hosp %>%
    select('Facility.Name', 'Rating.Safety', 'Procedure.Heart.Attack.Value') %>%
    mutate(.ci=0) %>% 
    mutate_at(".ci", as.integer)
  
  return(hosp)
  
  
}

# http://127.0.0.1:5402/test-team/w/644ee03767c11f751a0614ac820a4da0/ds/414a0287-55df-48b6-ac83-825a6a90a7c6
# options("tercen.workflowId"="644ee03767c11f751a0614ac820a4da0")
# options("tercen.stepId"="414a0287-55df-48b6-ac83-825a6a90a7c6")

ctx = tercenCtx()

tbl<- ctx %>%
  read_doc_id() %>%
  ctx$addNamespace() %>%
  ctx$save()

# http://127.0.0.1:5402/test-team/w/644ee03767c11f751a0614ac820a4da0/ds/414a0287-55df-48b6-ac83-825a6a90a7c6
#source("/home/rstudio/projects/test_builder_operator/test_utils.R")

#build_test_data( tbl, ctx, "auto_generated_1",
#                 test_folder = "/home/rstudio/projects/test_builder_operator/tests",
#                 version = '',
#                 docIdMapping = c("32d4b2986b98f3ebd5b5baa990000148"="hospitals.csv.zip"))

# -----
# Compare restuls
#check_test_local( tbl, "auto_generated_1", 
#                  test_folder = "/home/rstudio/projects/test_builder_operator/tests", 
#                              metric="r2",
#                              absTol=0, relTol=1e-15, r2=1 )

# -----

# Next steps
# 1 - Run test locally
# 2 - Run test at the function level
# 3 - Create a small report to append to the README.md (single line, might make a more extensive TEST.md)