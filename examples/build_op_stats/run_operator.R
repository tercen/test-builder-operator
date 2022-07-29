# ----
# ===============================
# VARIABLE DEFINITION
# ===============================
library(tercen)
library(teRcenHttp)
library(tercenApi)

source('./examples/build_op_stats/generate_data.R')
source('./examples/build_op_stats/set_env.R')


# ::NOTE::
# To track memory using docker stats:
# START ./mem_tracker.sh at host terminal

# Sys.setenv(PWSECRET='tercen')
userId <- Sys.getenv('TERCENUSER') #'testbot'
userPw <- Sys.getenv('TERCENPW') # 'testbot'
userEmail <- Sys.getenv('TERCENEMAIL') #'testbot@tercen.com'
team_name <- Sys.getenv('TERCENTEAM') #'test_team_bot'

operator_name <- Sys.getenv('OPERATORNAME') #'PCA'
operator_version <- Sys.getenv('OPERATORVERSION') #'PCA'
operator_url <- Sys.getenv('OPERATORURL') #'PCA'

github_token <- Sys.getenv('GITHUBTOKEN')

project_name <- paste0( 'operator_tester', 
                        '_', 
                        operator_name)




tasks <- c()

min_cols <- ifelse( Sys.getenv("MINCOLS") == '', 1, as.numeric(Sys.getenv("MINCOLS")) )
max_cols <- ifelse( Sys.getenv("MAXCOLS") == '', 1000, as.numeric(Sys.getenv("MINCOLS")) )
n_step_cols <- ifelse( Sys.getenv("NSTEPCOLS") == '', 5, as.numeric(Sys.getenv("MINCOLS")) )


min_rows <- ifelse( Sys.getenv("MINROWS") == '', 1, as.numeric(Sys.getenv("MINROWS")) )
max_rows <- ifelse( Sys.getenv("MAXROWS") == '', 1000, as.numeric(Sys.getenv("MAXROWS")) )
n_step_rows <- ifelse( Sys.getenv("NSTEPROWS") == '', 5, as.numeric(Sys.getenv("NSTEPROWS")) )

n_its <- ifelse( Sys.getenv("OPEXECITS") == '', 3, as.numeric(Sys.getenv("OPEXECITS")) )



# ----
# ===============================
# GET/CREATE TEST TEAM
# ===============================
client = tercen::TercenClient$new()


# Get team
teams = client$teamService$findTeamByOwner('admin')
team <- Find(function(p) identical(p$name, team_name), teams)
if( is.null(team) ){
  team <- tercenApi::Team$new()
  team$name <- team_name
  team$isDeleted <- FALSE
  
  team <- client$teamService$create(team)
}


# ----
# ===============================
# GET/CREATE TEST PROJECT
# ===============================
projects <- client$projectService$findByIsPublicAndLastModifiedDate('')
project <- Find(function(p) identical(p$name, project_name), projects)

if( is.null(project) ){
  # Create project test project
  project = tercenApi::Project$new()
  project$name = project_name
  project$acl = team$acl
  project$acl$owner = team$name
  project$isPublic = TRUE
  project$isDeleted = FALSE
  project = client$projectService$create(project)
}

# ----
# ===============================
# GET/INSTALL OPERATOR
# ===============================

# Searches the operator in 3 stages:
# 1: [Operator is installed] -> Operator with ID is returned with
#            documentService$findOperatorByOwnerLastModifiedDate(userId)
# 2: [Operator is not installed] -> Searches the available operators 
#            (previously installed and removed or operator library) with
#            documentService$getTercenOperatorLibrary and installs it
# 3: [Operator is not available] -> Must first create operator @TODO


# Check if operator is installed 
installed_ops <- client$documentService$findOperatorByOwnerLastModifiedDate(userId)

operator <-Find(function(p) identical(p$name, operator_name), installed_ops)

# If is.null(operator) is TRUE, then operator is not installed
if( is.null(operator)){
  # Retrieve list of available operators and try to install the desired one
  op_list <- client$documentService$getTercenOperatorLibrary(0, 10) 
  
  operator <- Find(function(p) identical(p$name, operator_name), op_list)
  
  if( is.null(operator)){
    # TODO: Operator is not in the available list. Check how to proceed here;
    # Situation no. 3 described above
  }else{
    install_task <- tercenApi::CreateGitOperatorTask$new()
    
    install_task$state = InitState$new()
    install_task$url$uri <- operator_url
    install_task$version <- operator_version
    install_task$isDeleted <- FALSE
    install_task$testRequired <- FALSE
    install_task$gitToken <- github_token
    install_task$owner <- project$acl$owner
    
    install_task = client$taskService$create(install_task)
    client$taskService$runTask(install_task$id)
    install_task = client$taskService$waitDone(install_task$id)
    
    # @TODO Check fail state
    operator <- client$operatorService$get( install_task$operatorId )
  }
}





# ----
# ===============================
# RUN THE OPERATOR
# ===============================

tasks <- c()

iseq <- as.integer(  round(
  seq( min_cols, max_cols, by=(max_cols-min_cols)/n_step_cols)
))

jseq <- as.integer(    round(
  seq( min_rows, max_rows, by=(max_rows-min_rows)/n_step_rows)
))
its <- n_its

# Function to test if a file is opened by a different process
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

# List of memory usage estimation using the 'free' Linux command
mem_usage_list <- c()

# List of memory usage estimation using the 'docker-stats'
# running independently on the host machine (see OperatorTest.md)
mem_usage_list_ext <- c()

for( i in seq(1,length(iseq))){
  for( j in seq(1,length(jseq))){
    if( j < i ) next
    for( k in seq(1,its)){
      # ++++ Upload dataset ++++
      # @TODO Enable generation of other types of data designs
      df <- generate_c03_data( num_vars=iseq[i], 
                               num_obs=jseq[j],
                               replicates=1, # No. data points within each cell
                               meas_min_val=-6, 
                               meas_max_val=6 )
      

      filename = "input_datatable"

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
      # END of upload 
      
      
      # ++++ Memory tracking management files ++++
      if( file.exists("stop.txt")){
        system('rm stop.txt')
      }
      
      if( file.exists("stop_ext.txt")){
        system('rm stop_ext.txt')
      }
      
      if( file.exists("mem.txt")){
        system('rm mem.txt')
      }
      
      if( file.exists("mem_ext.txt")){
        system('rm mem_ext.txt')
      }
      
      system("Rscript -e 'source(\"track_memory.R\")'", wait=FALSE)
      system("Rscript -e 'source(\"track_memory_external.R\")'", wait=FALSE)
      

      # ++++ CubeQuery definition and task execution ++++
      # @TODO Add support for other data designs
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
      
      
      
      kind <- class(operator)
      cube_query$operatorSettings$namespace = 'test_ns'
      cube_query$operatorSettings$operatorRef$operatorId = operator$id
      cube_query$operatorSettings$operatorRef$operatorKind = kind[1] 
      cube_query$operatorSettings$operatorRef$name = operator$name
      cube_query$operatorSettings$operatorRef$version = operator$version
      

      
      # TASK Definition
      task = tercenApi::RunComputationTask$new()
      

      task$query = cube_query
      task$state = tercenApi::InitState$new()
      task$owner = project$acl$owner
      task$projectId = project$id
      
      task = client$taskService$create(task)
      
      
      client$taskService$runTask(task$id)
      
      task = client$taskService$waitDone(task$id)
      tasks <- append( tasks, client$taskService$get(task$id))
      

      # Stop memory tracking R scripts
      system("touch stop.txt")
      system("touch stop_ext.txt")
      
      
      # Avoid concurrent access, which raises an error
      while( file.opened('mem.txt')  ){
        Sys.sleep(0.05)
      }
      

      mem_usage <- as.numeric(scan('mem.txt', character(), quote = ""))
      mem_usage_ext <- as.numeric(scan('mem_ext.txt', character(), quote = ""))
      
      used_mem <- max(mem_usage) - min(mem_usage)
      used_mem_ext <- max(mem_usage_ext) - min(mem_usage_ext)
      
      mem_usage_list <- append( mem_usage_list, used_mem )
      mem_usage_list_ext <- append( mem_usage_list_ext, used_mem_ext )
      
      client$tableSchemaService$delete(table_schema$id, table_schema$rev)
    }
  }
}

# Clean up memory tracking management files
system("touch stop.txt")
system("touch stop_ext.txt")

Sys.sleep(1)

if( file.exists("stop.txt")){
  system('rm stop.txt')
}

if( file.exists("stop_ext.txt")){
  system('rm stop_ext.txt')
}

if( file.exists("mem.txt")){
  system('rm mem.txt')
}
# ----

# PLOTTING 
# TODO <- Incorporate the two metrics in the plot
it <- 1

idx_columns <- c()
n_columns <- c()
idx_rows <- c()
n_rows <- c()

ncells <- c()

for( i in seq(1,length(iseq))){
  for( j in seq(1,length(jseq))){
    if( j < i) next
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

plot( ncells, mem_usage_list, pch=21, bg='blue', ylim=c(50, 450))
points(ncells, mem_usage_list_ext, pch=21, bg='red')

# ----
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

