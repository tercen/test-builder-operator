library(tercen)
library(tercenApi)
library(dplyr, warn.conflicts = FALSE)
library(tim)
library(jsonlite)

library(stringi)
# Function definitions
source('operator_functions.R')
source('test_utils.R')

# This file is used to create tests for operator which return relations

# http://127.0.0.1:5402/test-team/w/b61686f3b2947cf9b9c68af72d01a234/ds/1b7a8768-ac04-40e6-8eaf-c18990d99a76
# Test cases
options("tercen.workflowId"="b61686f3b2947cf9b9c68af72d01a234")

stepIdList <- c("1b7a8768-ac04-40e6-8eaf-c18990d99a76")


# Steps with properties
propDictList <- list()
# propDictList <- list("Mult2"=list(stepId="a18c19cb-52e7-427c-a1fa-ad7af76dce95",
#                                   multiplicator=2),
#                      "AllProp"=list(stepId="a18c19cb-52e7-427c-a1fa-ad7af76dce95",
#                                     multiplicator=4,
#                                     power1="1",
#                                     power2="2")
# )



for( i in seq(1, length(stepIdList))){
  options("tercen.stepId"=stepIdList[i])  
  ctx = tercenCtx()
  
  # Get step name
  wkf <- ctx$client$workflowService$get("b61686f3b2947cf9b9c68af72d01a234")
  steps <- wkf$steps
  
  current_step <- lapply(steps, function(x){
    if( x$id == stepIdList[i] ){
      return(x$name)
    }else{
      return(NULL)
    }
  } )
  
  step_name <- unlist(current_step[vapply(current_step, Negate(is.null), NA)])
  print(step_name)
  
  
  has_property_list <- lapply(  propDictList, function(x){
    x$stepId == stepIdList[i]
  })
  
  if( any(unname(unlist(has_property_list)))){
    # 
    # prop_list_idx <- which(unname(unlist(has_property_list)))
    # for(j in prop_list_idx){
    #   
    #   props <- propDictList[j]
    #   test_suff <- unlist(unname(names(props)))[1]
    #   props <- props[[test_suff]]
    #   props$stepId <- NULL
    #   
    #   plist <- props
    #   pnames <- unlist(unname(names(props)))
    #   
    #   
    #   step_name_ex <- paste0(step_name, "_", test_suff)
    #   
    #   
    #   # scale = ctx$op.value("scale", type=as.logical, default = FALSE)  
    #   # center = ctx$op.value("center", type=as.logical, default = TRUE)  
    #   # tol = ctx$op.value("tol", type=as.double, default = 0)
    #   # maxComp = ctx$op.value("maxComp", type=as.integer, default=4) 
    #   # 
    #   # pca_res <- pca_func( ctx, scale = scale, 
    #   #                      center = center, 
    #   #                      tol = tol,
    #   #                      maxComp=maxComp)
    #   # 
    #   # save_relation(pca_res, ctx)
    #   
    #   
    #   tbl<- ctx %>%
    #     select(.y, .ci, .ri) %>% 
    #     group_by(.ci, .ri) %>%
    #     summarise(mean=do.call("cell_mean",
    #                            list(.y, !!!setNames(plist,pnames)))) %>%
    #     ctx$addNamespace() 
    #   
    #   #TODO: Add parameters here....
    #   build_test_data_local( tbl, ctx, paste0(step_name_ex, "_absTol"),
    #                          version = '0.0.1',
    #                          absTol = 0.001,
    #                          props=setNames(plist,pnames))
    # }
  }else{
    pca_res <- pca_func( ctx )
    
    build_test_data_local( pca_res, ctx, paste0(step_name, "_absTol"),
                           version = '0.0.1',
                           absTol = 0.001)
  }
}

