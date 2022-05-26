library(tercen)
library(tercenApi)
library(dplyr, warn.conflicts = FALSE)
library(tim)
library(jsonlite)

# Function definitions
source('operator_functions.R')
source('test_utils.R')

# http://127.0.0.1:5402/test-team/w/644ee03767c11f751a0614ac820a4da0/ds/c15f4867-126d-4380-8126-7383f2cca690
# http://127.0.0.1:5402/test-team/w/644ee03767c11f751a0614ac820a4da0/ds/3d88d8f8-0388-4748-93dc-3262262b6c34
# Test cases
stepIdList <- c("c15f4867-126d-4380-8126-7383f2cca690",
                "3d88d8f8-0388-4748-93dc-3262262b6c34")


options("tercen.workflowId"="644ee03767c11f751a0614ac820a4da0")
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
  tbl<- ctx %>%
    select(.y, .ci, .ri) %>% 
    group_by(.ci, .ri) %>%
    summarise(mean = cell_mean(.y)) %>%
    ctx$addNamespace() 
  
  # Equality
  build_test_data( tbl, ctx, step_name,
                   version = '0.0.1')#,
                   #docIdMapping = c("32d4b2986b98f3ebd5b5baa990000148"="hospitals.csv.zip"))
  
  # Tolerance
  build_test_data( tbl, ctx, paste0(step_name, "_absTol"),
                   version = '0.0.1',
                   absTol = 0.001)
  
  build_test_data( tbl, ctx, paste0(step_name, "_relTol"),
                   version = '0.0.1',
                   relTol = 0.01)
  
  
  # Correlation
  build_test_data( tbl, ctx, paste0(step_name, "_r2"),
                   version = '0.0.1',
                   r2 = 0.99)
}
  




# -----
# Compare results
#check_test_local( tbl, "auto_generated_1", 
#                  test_folder = "/home/rstudio/projects/test_builder_operator/tests", 
#                              metric="r2",
#                              absTol=0, relTol=1e-15, r2=1 )

# -----

