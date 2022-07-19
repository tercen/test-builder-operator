# ----
library(tercen)
library(teRcenHttp)
library(tercenApi)

source('./examples/build_op_stats/generate_data.R')

# TODO Run test across a range of values

userId = 'admin'
userPw = 'admin'

team_name = 'test-team'
project_name = 'r_project'



# GET TEAM AND PROJECT
client = tercen::TercenClient$new()


# Get team
teams = client$teamService$findTeamByOwner('admin')
is_test_team = unlist(lapply( teams, function(x){
  x$name == team_name
}))

team = teams[[which(is_test_team)]]


# Create project test project
project = tercenApi::Project$new()
project$name = project_name
project$acl = team$acl
project$acl$owner = team$name
project$isPublic = TRUE
project$isDeleted = FALSE
project = client$projectService$create(project)


# END OF GET TEAM AND PROJECT
# =================================================== ----

source('./examples/build_op_stats/generate_data.R')

tasks <- c()

iseq <- c(1, 100, 200, 300, 400, 500)
jseq <- c(1, 100, 200, 300, 400, 500)


for( i in seq(1,length(iseq))){
  for( j in seq(1,length(jseq))){
    if( j < i) next
    df <- generate_c03_data( num_vars=iseq[i], 
                             num_obs=jseq[j],
                             replicates=10,
                             meas_min_val=0, 
                             meas_max_val=1 )
    
    # TODO -->
    # Upload input table ----
    filename = "c03_simple_small.tsv"
    # df = as.data.frame(read.csv("/home/rstudio/projects/test_builder_operator/tmp/c03_simple_small.tsv", sep="\t"))
    tbl = tercen::dataframe.as.table(df)
    bytes = memCompress(teRcenHttp::to_tson(tbl$toTson()),
                        type = 'gzip')
    
    fileDoc = FileDocument$new()
    fileDoc$name = paste0(filename)
    fileDoc$projectId = project$id
    fileDoc$acl$owner = project$acl$owner
    fileDoc$metadata$contentEncoding = 'gzip'
    
    
    fileDoc = client$fileService$upload(fileDoc, bytes)
    
    task = CSVTask$new()
    task$state = InitState$new()
    task$fileDocumentId = fileDoc$id
    task$owner = project$acl$owner
    task$projectId = project$id
    
    task = client$taskService$create(task)
    client$taskService$runTask(task$id)
    task = client$taskService$waitDone(task$id)
    if (inherits(task$state, 'FailedState')){
      stop(task$state$reason)
    }
    
    
    table_schema = client$tableSchemaService$get(task$schemaId)
    client$tableSchemaService$update(table_schema)
    
    
    
    
    
    # END of upload ----
    
    
    
    
    # ----QueryBlock ----
    # Mean operator Id = a1b3f2921d2ef5c6c59908b9740015dc
    cube_query = tercenApi::CubeQuery$new()
    cube_query$relation = tercen::as_relation(table_schema)
    
    
    col = tercenApi::Factor$new()
    col$name = 'Variable'
    col$type = 'string'
    cube_query$colColumns = list(col)
    
    row = tercenApi::Factor$new()
    row$name = 'Observation'
    row$type = 'string'
    cube_query$rowColumns = list(row)
    
    axis_query = tercenApi::CubeAxisQuery$new()
    axis_query$yAxis$name = "Measurement"
    axis_query$yAxis$type = "double"
    cube_query$axisQueries = list(axis_query)
    
    
    
    mean_op = client$operatorService$get('a1b3f2921d2ef5c6c59908b9740015dc')
    
    cube_query$operatorSettings$namespace = 'test02'
    cube_query$operatorSettings$operatorRef$operatorId = mean_op$id
    cube_query$operatorSettings$operatorRef$operatorKind = "ROperator" #mean_op$kind = kind is on summary, but is not accessible
    cube_query$operatorSettings$operatorRef$name = mean_op$name
    cube_query$operatorSettings$operatorRef$version = mean_op$version
    
    
    # ----
    # task = tercenApi::CSVTask$new()
    task = tercenApi::RunComputationTask$new()
    
    # task$fileDocumentId = proj_document$id
    task$query = cube_query
    task$state = tercenApi::InitState$new()
    
    task$owner = project$acl$owner
    task$projectId = project$id
    
    task = client$taskService$create(task)
    client$taskService$runTask(task$id)
    task = client$taskService$waitDone(task$id)
    
    
    task$state
    
    tasks <- append( tasks, client$taskService$get(task$id))
    
  }
}

# ----

ncells <- c()

for( i in seq(1,length(iseq))){
  for( j in seq(1,length(jseq))){
    ncells <- append( npars, iseq[i]*jseq[j])
  }
}

mem_usage <- unlist(lapply( tasks, function(x){
  as.numeric(x$meta[[20]]$value)/(1024*1024)
}))


durations <- unlist(lapply( tasks, function(x){
  as.numeric(x$duration)
}))
      
plot(ncells, mem_usage)
# ----
# DO CLEANUP 

# client$taskService$delete(task$id)

# tmp<- 
# tmp <-  client$projectDocumentService$findFileByLastModifiedDate(
#   startKey = project$id,
#   endKey = '',
#   useFactory = FALSE
# )
# 
# 
# unlist(lapply(tmp, function(x){
#   x$name
# }))
# 
# 
# 
# file_stats = Find(function(p) identical(p$name, "stats-summary.csv"), tmp)
# 
# bytes <- client$fileService$download(file_stats$id)
# char <- rawToChar(bytes)
# df_raw <- read.table(text = char, header = TRUE)


# userSession = client$userService$connect(userId, userPw)
# baseRestUri = client$operatorService$baseRestUri

# projs = client$projectService$findByTeamAndIsPublicAndLastModifiedDate(
#   startKey=team_name,
#   endKey='')
# 
# 
# 
# name_match = unlist(lapply(projs, function(x){
#   x$name == project_name
#   # x$name
# }))
# 
# if( any(name_match==TRUE)){
#   proj = projs[[which(name_match==TRUE)]]
# }else{
#   proj = tercenApi::Project$new()
#   proj$name = project_name
#   proj$acl = team$acl
#   proj$acl$owner = team$name
#   proj$isPublic = TRUE
#   proj$isDeleted = FALSE
#   proj = client$projectService$create(proj)
# }
# =========================================================
# Object examples
# wkf = client$workflowService$get('b417efa5e0df231fc5968497a302cce5')
# stp = wkf$steps[[3]]
# stp
