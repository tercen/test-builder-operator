library(tercen)
library(tercenApi)
library(dplyr, warn.conflicts = FALSE)
library(tim)
library(jsonlite)

library(stringi)
# Function definitions
source('operator_functions.R')
source('test_utils.R')

# http://127.0.0.1:5402/test-team/w/644ee03767c11f751a0614ac820a4da0/ds/c15f4867-126d-4380-8126-7383f2cca690
# http://127.0.0.1:5402/test-team/w/644ee03767c11f751a0614ac820a4da0/ds/3d88d8f8-0388-4748-93dc-3262262b6c34
# Test cases
options("tercen.workflowId"="644ee03767c11f751a0614ac820a4da0")

stepIdList <- c("c15f4867-126d-4380-8126-7383f2cca690",
                "3d88d8f8-0388-4748-93dc-3262262b6c34",
                "936c4988-a446-4c4c-b270-3faddb870bbb",
                "a18c19cb-52e7-427c-a1fa-ad7af76dce95")


# propDictList <- list(
#                    "a18c19cb-52e7-427c-a1fa-ad7af76dce95"=list("Mult2"=list("multiplicator"=2),
#                      "AllProp"=list("multiplicator"=4,"power1"="1", "power2"="2")
#                      ))


propDictList <- list("Mult2"=list(stepId="a18c19cb-52e7-427c-a1fa-ad7af76dce95",
                                  multiplicator=2),
                     "AllProp"=list(stepId="a18c19cb-52e7-427c-a1fa-ad7af76dce95",
                                  multiplicator=4,
                                  power1="1",
                                  power2="2")
                     )


for( i in seq(1, length(stepIdList))){
  options("tercen.stepId"=stepIdList[i])  
  ctx = tercenCtx()

  # Get step name
  wkf <- ctx$client$workflowService$get("644ee03767c11f751a0614ac820a4da0")
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
    
    prop_list_idx <- which(unname(unlist(has_property_list)))
    for(j in prop_list_idx){

      props <- propDictList[j]
      test_suff <- unlist(unname(names(props)))[1]
      props <- props[[test_suff]]
      props$stepId <- NULL
      
      plist <- props
      pnames <- unlist(unname(names(props)))
      
      
      step_name_ex <- paste0(step_name, "_", test_suff)

      tbl<- ctx %>%
        select(.y, .ci, .ri) %>% 
        group_by(.ci, .ri) %>%
        summarise(mean=do.call("cell_mean",
                               list(.y, !!!setNames(plist,pnames)))) %>%
        ctx$addNamespace() 
      
      #TODO: Add parameters here....
      build_test_data_local( tbl, ctx, paste0(step_name_ex, "_absTol"),
                             version = '0.0.1',
                             absTol = 0.001,
                             props=setNames(plist,pnames))
    }
  }else{
    tbl<- ctx %>%
      select(.y, .ci, .ri) %>% 
      group_by(.ci, .ri) %>%
      summarise(mean = cell_mean(.y)) %>%
      ctx$addNamespace() 
    
    build_test_data_local( tbl, ctx, paste0(step_name, "_absTol"),
                           version = '0.0.1',
                           absTol = 0.001)
  }
  
  
  

  # Tolerance
  #tim::build_test_data( tbl, ctx, paste0(step_name, "_absTol"),
  #                 version = '0.0.1',
  #                 absTol = 0.001)
  

  
  # tim::build_test_data( tbl, ctx, paste0(step_name, "_relTol"),
  #                  version = '0.0.1',
  #                  relTol = 0.01)
  # 
  # 
  # # Correlation
  # tim::build_test_data( tbl, ctx, paste0(step_name, "_r2"),
  #                  version = '0.0.1',
  #                  r2 = 0.99)
}
  




# -----
# Compare results
#check_test_local( tbl, "auto_generated_1", 
#                  test_folder = "/home/rstudio/projects/test_builder_operator/tests", 
#                              metric="r2",
#                              absTol=0, relTol=1e-15, r2=1 )

# -----

