# ----
library(tercen)
library(teRcenHttp)
library(tercenApi)

source('./examples/build_op_stats/generate_data.R')

# TODO Run test across a range of values ----

userId = 'admin'
userPw = 'admin'
operator_name = 'mean_operator'


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


# GET OPERATOR ----
# If the operator is not in the library, the ID must be manually retrieved
# response = client$documentService$client$post(url, body = params)


op_list <- client$documentService$getTercenOperatorLibrary(0, 10) # NO Ids returned...

op <- Find(function(p) identical(p$name, "PCA"), op_list)

operator <- client$operatorService$create(op)

mean_op = client$operatorService$get('a1b3f2921d2ef5c6c59908b9740015dc')

unlist(lapply(op_list, function(x){
  x$name
}))

#client$operatorService$delete(operator$id, operator$rev)

# END OF GET TEAM AND PROJECT
# =================================================== ----

source('./examples/build_op_stats/generate_data.R')

tasks <- c()

iseq <- c(5, 10)
jseq <- c(5, 10) #, 100, 300, 600)
its <- 1

prof_df <- data.frame()

file.opened <- function(path) {
  suppressWarnings(
    "try-error" %in% class(
      try(file(path, 
               open = "a"), 
          silent = TRUE
      )
    )
  )
}

# http://tercen:5400/api/v1/api/v1/operator
mem_usage_list <- c()

for( i in seq(1,length(iseq))){
  for( j in seq(1,length(jseq))){
    # if( j < i ) next
    for( k in seq(1,its)){
      
      

      
      df <- generate_c03_data( num_vars=iseq[i], 
                               num_obs=jseq[j],
                               replicates=1,
                               meas_min_val=0, 
                               meas_max_val=1 )
      

      # Upload input table ---
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
      
      
      
      
      
      # END of upload ---
      
      if( file.exists("stop.txt")){
        system('rm stop.txt')
      }
      
      if( file.exists("mem.txt")){
        system('rm mem.txt')
      }
      system("Rscript -e 'source(\"track_memory.R\")'", wait=FALSE)
      
      
      # ----QueryBlock ---
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
      
      
      
      # mean_op = client$operatorService$get('a1b3f2921d2ef5c6c59908b9740015dc')
      kind <- class(operator)
      cube_query$operatorSettings$namespace = 'test02'
      cube_query$operatorSettings$operatorRef$operatorId = operator$id
      cube_query$operatorSettings$operatorRef$operatorKind = kind[1] 
      cube_query$operatorSettings$operatorRef$name = operator$name
      cube_query$operatorSettings$operatorRef$version = operator$version
      
      
      # ---
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
      
      system("touch stop.txt")
      
      # mem_usage <- unname(unlist(read.csv2('mem.txt')))
      while( file.opened('mem.txt')  ){
        Sys.sleep(0.05)
      }
      
      mem_usage <- as.numeric(scan('mem.txt', character(), quote = ""))
      
      # plot(unname(unlist(mem_usage)))
      
      q <- quantile(mem_usage, c(0.02, 0.98))
      used_mem <- unname(q[2]) - unname(q[1])
      # used_mem <- max(mem_usage) - min(mem_usage)
      
      mem_usage_list <- append( mem_usage_list, used_mem )
      
      prof_df <- rbind(prof_df, 
                       data.frame(i=i, j=j, it=k, mem=used_mem))
      
      client$tableSchemaService$delete(table_schema$id, table_schema$rev)
    }
  }
}

system("touch stop.txt")

Sys.sleep(1)

if( file.exists("stop.txt")){
  system('rm stop.txt')
}

if( file.exists("mem.txt")){
  system('rm mem.txt')
}
# ----
plot(sort(mem_usage_list))
# ----
mem_usage <- read.csv('mem.txt')
plot(unname(unlist(mem_usage)))

# 227 pra 200 x 200
# 447 pra 400 x 400
used_mem <- max(mem_usage) - min(mem_usage)

# ----

# PLOTTING FOR columns


it <- 1

idx_columns <- c()
n_columns <- c()
idx_rows <- c()
n_rows <- c()

ncells <- c()

for( i in seq(1,length(iseq))){
  for( j in seq(1,length(jseq))){
    # if( j < i) next
    for( k in seq(1,its)){
    
      if( iseq[i] == 1){
        idx_rows <- append(idx_rows, it)
      }
      
      if( jseq[j] == 1){
        idx_columns <- append(idx_columns, it)
      }
      
      n_columns <- append(n_columns, iseq[i])
      n_rows <- append(n_rows, jseq[j])
      ncells <- append( ncells, iseq[i]*jseq[j])
      it <- it + 1
    }
  }
}

durations <- unlist(lapply( tasks, function(x){
  as.numeric(x$duration)
}))
      
# summary(lm( mem_usage ~ ncells, data=data.frame(mem_usage, ncells) ))



mdl <- lm( mem_usage  ~ num_cells, 
           data=data.frame('mem_usage'=mem_usage_list, 
                           'num_cells'=ncells,
                           'num_cols'=n_columns,
                           'num_rows'=n_rows))

summary(mdl)

mdl$coefficients

x_pred <- seq(1, 8e5, by=100)
coeffs <- mdl$coefficients
smdl <- summary(mdl)
sd_coeffs <- c( smdl$coefficients[1,2], smdl$coefficients[2,2] )

y_pred <- smdl$coefficients[1,1] + x_pred * smdl$coefficients[2,1]
y_pred_3sd <- (smdl$coefficients[1,1] + smdl$coefficients[1,2]*3) + 
              x_pred * (smdl$coefficients[2,1] + smdl$coefficients[2,2]*3)


xticks <- c()
xtickslabels <- c()
step <- round((max(x_pred)-min(x_pred))/10)
for( i in seq(min(x_pred), max(x_pred), by=step)){
  xticks <- append(xticks, i)
  
  lbl <- as.character(i )
  
  if(i >= 1e3){
    lbl <- paste0(as.character( round(i/1e3)  ), 'K')  
  }
  
  if(i >= 1e6){
    lbl <- paste0(as.character( round(i/1e6)  ), 'M')  
  }
  
  xtickslabels <- append(xtickslabels, lbl)
}



pred_memory_mean_title <- paste0('Est. mean mem.: ', 
                            round(smdl$coefficients[1,1]), 'mb + ',
                            signif(smdl$coefficients[2,1], digits=2), 'mb per cell' )


pred_memory_meansd_title <- paste0('Est. 3sd mem.: ', 
                            round(smdl$coefficients[1,1] + smdl$coefficients[1,2]*3), 'mb + ',
                            signif(smdl$coefficients[2,1] + smdl$coefficients[2,2]*3, digits=2), 'mb per cell' )



plot(x_pred, y_pred, type='l', lwd=3, col=rgb(0.75,0.5,0.5,1),
     ylim=c(0,1000),
     xlab = "N. columns x N. rows [a.u.]",
     ylab = "Estimated allocated memory [mb]",
     xaxt='n', axes = FALSE)

lines(x_pred, y_pred_3sd, lwd=3, col='red')

# X-axis
axis(1, at = xticks, labels=xtickslabels)
axis(2)

text(220000,800,pred_memory_meansd_title, col=rgb(0.75,0.5,0.5,1))
text(220000,900,pred_memory_mean_title, col='red')

points(ncells, mem_usage_list,
       pch=21, col='black', bg=rgb( 0.4, 0.4, 0.4, 0.5 ))


# ----

plot(ncells, mem_usage_list)

# plot( n_columns[idx_columns] + (4*(runif(length(idx_columns))-0.5)), 
#       mem_usage_list[idx_columns], col='black', pch=21, bg='lightblue' )
# 
# 
# df <- data.frame('cols'=n_columns[idx_columns], 'mem'=mem_usage_list[idx_columns]) %>%
#   group_by(cols) %>%
#   summarise(., mean=mean(mem), std=sd(mem), min=min(mem), max=max(mem))
# points( unlist(df['cols']), 
#       unlist(df['mean']), col='black', pch=22, bg='blue', cex=3 )
# 
# 
# points( n_rows[idx_rows] + (4*(runif(length(idx_rows))-0.5)), 
#       mem_usage_list[idx_rows], col='black', pch=21, bg='lightgreen' )
# 
# df <- data.frame('rows'=n_rows[idx_rows], 'mem'=mem_usage_list[idx_rows]) %>%
#   group_by(rows) %>%
#   summarise(., mean=mean(mem), std=sd(mem), min=min(mem), max=max(mem))
# points( unlist(df['rows']), 
#         unlist(df['mean']), col='black', pch=22, bg='green', cex=3 )

# plot(ncells, mem_usage)
# ----
# DO CLEANUP 
client$operatorService$delete(operator$id, operator$rev)
client$projectService$delete(project$id, project$rev)

# ----

# PLOTTING RESULTS


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
