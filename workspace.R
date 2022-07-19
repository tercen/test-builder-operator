library(tercen)
library(tercenApi)
library(dplyr, warn.conflicts = FALSE)
library(tim)
library(jsonlite)
library(tidyr)

source('operator_functions.R')
source("test_utils.R")

test_type <- c("Single", "List", "Schema")
wkf_id   <- c("644ee03767c11f751a0614ac820a4da0","644ee03767c11f751a0614ac820a4da0","644ee03767c11f751a0614ac820a4da0")
stp_id <- c("86eef707-3341-4936-a684-65f0c636e505","512ff26e-1a0c-487c-a390-49204b9b274d","5490bf87-8f01-4cfa-a392-036e9eb51f92")

current_test <- "Single"

switch(current_test,
       "Single"={
         options("tercen.workflowId"=wkf_id[1])
         options("tercen.stepId"=stp_id[1])
         
         ctx = tercenCtx()
         res_tbl <- ctx %>%
           select(.y, .ci, .ri) %>%
           group_by(.ci, .ri) %>%
           summarise(mean = cell_mean(.y)) %>%
           ctx$addNamespace()
         
         test_name <- 'UC005_NoRow_absTol'
         # run_local_test( res_tbl, ctx, test_name, 
         #                             test_folder = NULL, 
         #                             absTol=0.1, docIdMapping=c())
         
         ctx$save(res_tbl)
       },
       "List"={
         options("tercen.workflowId"=wkf_id[2])
         options("tercen.stepId"=stp_id[2])
         
         ctx = tercenCtx()
         hclust_func <- function( ctx, scale = FALSE, fill = 0,
                                  method = 'single', metric = 'euclidean'  )
           
         scale = ctx$op.value("scale", type=as.logical, default = FALSE)
         fill = ctx$op.value("fill", type=as.integer, default = 0)
         method = ctx$op.value("method", type=as.character, default = 'single')
         metric = ctx$op.value("metric", type=as.character, default='euclidean')

         hclust_res <- hclust_func( ctx, scale = scale,
                              center = center,
                              tol = tol,
                              maxComp=maxComp)

         ctx$save(hclust_res)
       },
       "Schema"={
         options("tercen.workflowId"=wkf_id[3])
         options("tercen.stepId"=stp_id[3])
         
         ctx = tercenCtx()
         
         scale = ctx$op.value("scale", type=as.logical, default = FALSE)
         center = ctx$op.value("center", type=as.logical, default = TRUE)
         tol = ctx$op.value("tol", type=as.double, default = 0)
         maxComp = ctx$op.value("maxComp", type=as.integer, default=4)
         
         pca_res <- pca_func( ctx, scale = scale,
                              center = center,
                              tol = tol,
                              maxComp=maxComp)
         
         save_relation(pca_res, ctx)
       })
