library(tercen)
library(tercenApi)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(tim)
library(jsonlite)

library(stringi)
# Function definitions
source('../../operator_functions.R')
source('../../test_utils.R')

lapply( Sys.glob("../../tests/*.csv"), function(x) { unlink(x) } )
lapply( Sys.glob("../../tests/*.json"), function(x) { unlink(x) } )
lapply( Sys.glob("../../tests/*.Rda"), function(x) { unlink(x) } )
lapply( Sys.glob("../../tests/*.csv.schema"), function(x) { unlink(x) } )

# Test cases
wkfId <- "644ee03767c11f751a0614ac820a4da0"
options("tercen.workflowId"= wkfId)

stepIdList <- c("5490bf87-8f01-4cfa-a392-036e9eb51f92",
                "f9e18888-a6ad-4110-a06c-190d35eaf4a9")


# Steps with properties
propDictList <- list()
propDictList <- list("MaxComps"=list(stepId="5490bf87-8f01-4cfa-a392-036e9eb51f92",
                                      maxComp=8),
                     "Default"=list(stepId="5490bf87-8f01-4cfa-a392-036e9eb51f92"))


for( i in seq(1, length(stepIdList))){
  options("tercen.stepId"=stepIdList[i])  
  ctx = tercenCtx()
  
  # Get step name
  step_name <- get_step_name(ctx, wkfId, stepIdList[i])
  print(step_name)
  
  
  has_property_list <- lapply(  propDictList, function(x){
    x$stepId == stepIdList[i]
  })
  
  if( any(unname(unlist(has_property_list)))){

    prop_list_idx <- which(unname(unlist(has_property_list)))
    for(j in prop_list_idx){

      props <- propDictList[j]
      test_suff <- unlist(unname(names(props)))[1]
      props <- props[[test_suff]]
      props$stepId <- NULL

      plist <- props
      pnames <- unlist(unname(names(props)))


      step_name_ex <- paste0(step_name, "_", test_suff)


      params <- c(ctx, setNames(plist,pnames))
      res <- do.call('pca_func', params )

      tim::build_test_data( res, ctx, paste0(step_name_ex, "_absTol"),
                             version = '0.0.1',
                             absTol = 0.001,
                             gen_schema = TRUE,
                             skipCols=c("PC", ".pc.rids"),
                             props=setNames(plist,pnames))


    }
  }else{
    res <- pca_func( ctx )
    
    tim::build_test_data( res, ctx, paste0(step_name, "_absTol"),
                           version = '0.0.1',
                          skipCols=c("PC", ".pc.rids"),
                          gen_schema = TRUE,
                           absTol = 0.001)
  }
}

