library(tercen)
library(tercenApi)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(tim)
library(jsonlite)

library(stringi)
# Function definitions
source('operator_functions.R')
source('test_utils.R')

# This file is used to create tests for operator which return relations
wkfId <- "644ee03767c11f751a0614ac820a4da0"
options("tercen.workflowId"=wkfId)

stepIdList <- c("0c241e77-bd95-4720-87f0-8e206cbfbeab")


# Steps with properties
propDictList <- list()

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
      res <- do.call('hclust_func', params )
      
      build_test_data_local( res, ctx, paste0(step_name_ex, "_absTol"),
                             version = '0.0.1',
                             absTol = 0.001,
                             props=setNames(plist,pnames))
      
      
    }
  }else{
    res <- hclust_func( ctx )
    
    build_test_data_local( res, ctx, paste0(step_name, "_absTol"),
                           version = '0.0.1',
                           absTol = 0.001)

  }
}

