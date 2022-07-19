library(tercen)
library(tercenApi)
library(dplyr, warn.conflicts = FALSE)
library(tim)
library(jsonlite)

library(stringi)
# Function definitions
source('../../operator_functions.R')
source('../../test_utils.R')

# # Remove old tests
lapply( Sys.glob("../../tests/*.csv"), function(x) { unlink(x) } )
lapply( Sys.glob("../..//tests/*.json"), function(x) { unlink(x) } )
lapply( Sys.glob("../../tests/*.Rda"), function(x) { unlink(x) } )
lapply( Sys.glob("../../tests/*.csv.schema"), function(x) { unlink(x) } )

wkfId <- "644ee03767c11f751a0614ac820a4da0"
options("tercen.workflowId"=wkfId)

# stepIdList <- c("c15f4867-126d-4380-8126-7383f2cca690",
#                 "3d88d8f8-0388-4748-93dc-3262262b6c34",
#                 "936c4988-a446-4c4c-b270-3faddb870bbb",
#                 "a18c19cb-52e7-427c-a1fa-ad7af76dce95")

stepIdList <- c("86eef707-3341-4936-a684-65f0c636e505")


# Steps with properties
propDictList <- list("Default"=list(stepId="a18c19cb-52e7-427c-a1fa-ad7af76dce95"),
                     "Mult2"=list(stepId="a18c19cb-52e7-427c-a1fa-ad7af76dce95",
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

      tbl<- ctx %>%
        select(.y, .ci, .ri) %>% 
        group_by(.ci, .ri) %>%
        summarise(mean=do.call("cell_mean",
                               list(.y, !!!setNames(plist,pnames)))) %>%
        ctx$addNamespace() 
      
      tim::build_test_data( tbl, ctx, paste0(step_name_ex, "_absTol"),
                             version = '0.0.1',
                             absTol = 0.001,
                             skipCols=c("mean"),
                             props=setNames(plist,pnames))
    }
  }else{
    tbl<- ctx %>%
      select(.y, .ci, .ri) %>% 
      group_by(.ci, .ri) %>%
      summarise(mean = cell_mean(.y)) %>%
      ctx$addNamespace() 
    
    tim::build_test_data( tbl, ctx, paste0(step_name, "_absTol"),
                           version = '0.0.1',
                           skipCols=c("mean"),
                           absTol = 0.001)
  }
  

}
  

