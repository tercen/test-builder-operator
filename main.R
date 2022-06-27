library(tercen)
library(tercenApi)
library(dplyr, warn.conflicts = FALSE)
library(tim)
library(jsonlite)
library(tidyr)

source('operator_functions.R')

# ====================================================
# IF using test_build.R, then uncomment the section below
ctx = tercenCtx()
ctx %>%
  select(.y, .ci, .ri) %>%
  group_by(.ci, .ri) %>%
  summarise(mean = cell_mean(.y)) %>%
  ctx$addNamespace() %>%
  ctx$save()


# ====================================================
# IF using test_build_schema.R
# # http://127.0.0.1:5402/test-team/w/b61686f3b2947cf9b9c68af72d01a234/ds/1b7a8768-ac04-40e6-8eaf-c18990d99a76
# # http://127.0.0.1:5402/test-team/w/b61686f3b2947cf9b9c68af72d01a234/ds/a42fd3ad-5ad4-40a3-9741-c6b78beac28a
# options("tercen.workflowId"="b61686f3b2947cf9b9c68af72d01a234")
# options("tercen.stepId"="a42fd3ad-5ad4-40a3-9741-c6b78beac28a")
# 
# ctx = tercenCtx()
# 
# scale = ctx$op.value("scale", type=as.logical, default = FALSE)
# center = ctx$op.value("center", type=as.logical, default = TRUE)
# tol = ctx$op.value("tol", type=as.double, default = 0)
# maxComp = ctx$op.value("maxComp", type=as.integer, default=4)
# 
# pca_res <- pca_func( ctx, scale = scale,
#                      center = center,
#                      tol = tol,
#                      maxComp=maxComp)
# 
# save_relation(pca_res, ctx)
