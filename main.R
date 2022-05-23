library(tercen)
library(tercenApi)
library(dplyr, warn.conflicts = FALSE)
library(tim)
library(jsonlite)


source('operator_functions.R')


ctx = tercenCtx()

ctx %>%
  select(.y, .ci, .ri) %>% 
  group_by(.ci, .ri) %>%
  summarise(mean = cell_mean(.y)) %>%
  ctx$addNamespace() %>%
  ctx$save()
