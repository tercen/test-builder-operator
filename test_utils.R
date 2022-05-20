library(stringr)


build_test_data <- function( out_table, ctx, test_name, 
                             test_folder = NULL, version = '',
                             docIdMapping=c()){
  if( is.null(test_folder)){
    test_folder <- paste0( getwd(), '/tests' )
  }
  
  namespace <- ctx$namespace
  
  testTbl <- tbl
  save(testTbl,file= file.path(test_folder, paste0(test_name, '.Rda')) )
  proj_names <- ctx$names
  
  # Always present even if there is nothing set for it
  select_names <- c(".y", ".ci",".ri")
  yAxis <- ''
  xAxis <- ''

  has_y <- FALSE  
  has_x <- FALSE  
  
  # Check whether y and x axis are set
  # if( ".y" %in% proj_names){
  #   select_names <- append(select_names, ".y")
  #   yAxis <- "y_values"
  #   has_y <- TRUE
  # }
  if( ".x" %in% proj_names){
    select_names <- append(select_names, ".x")  
    xAxis <- "x_values"
    has_x <- TRUE
  }
  # if( ".ci" %in% proj_names){select_names <- append(select_names, ".ci")  }
  # if( ".ri" %in% proj_names){select_names <- append(select_names, ".ri")  }
  
  # Select, if available, .y, .x, .ci and.ri and corresponding row and column tables
  in_tbl <- ctx$select(select_names) 
  in_rtbl <- ctx$rselect()
  in_ctbl <- ctx$cselect()

  
  
  if(has_y == TRUE){
    in_tbl <- in_tbl%>% rename("y_values"=".y") 
  }else{
    in_tbl <- in_tbl%>% select(-".y")
  }
  
  if(has_x == TRUE){
    in_tbl <- in_tbl%>% rename("x_values"=".x") 
  }
  
  
  has_row_tbl <- FALSE
  has_col_tbl <- FALSE
  
  # .all -> Empty row or column projection table
  if( names(in_rtbl) != ".all" ){
    in_rtbl <- in_rtbl %>% mutate( .ri=seq(0,nrow(.)-1) )
    in_tbl <- dplyr::full_join( in_tbl, in_rtbl, by=".ri" ) %>%
      select(-".ri") 
    has_row_tbl <- TRUE
  }else{
    in_tbl <- select(in_tbl, -".ri")
    tryCatch({
      out_table <- out_table %>% select(-".ri")
    }, error=function(cond){
      # Ignore, no .ri column in result
    })
  }
  
  if( names(in_ctbl) != ".all" ){
    in_ctbl <- in_ctbl %>% mutate( .ci=seq(0,nrow(.)-1) )
    in_tbl <- dplyr::full_join( in_tbl, in_ctbl, by=".ci" ) %>%
      select(-".ci")

    has_col_tbl <- TRUE
  }else{
    in_tbl <- select(in_tbl, -".ci")
    tryCatch({
      out_table <- out_table %>% select(-".ci")
    }, error=function(cond){
      # Ignore, no .ci column in result
    })
  }
  
  if( length(docIdMapping) > 0 ){
    
    # Find documentId instances and replace them
    for( i in seq(1, length(docIdMapping))  ){
      in_tbl <- in_tbl %>%
        mutate_all( ~str_replace(., unlist(names(docIdMapping[i])), unname(docIdMapping[i])) )
    }
    
  }
  
  # If no version is supplied, try to get the latest version in the repo,
  # and failing that, the latest commit
  if(version == ''){
    version <- system("git describe --tags", intern = TRUE)
  }
  
  if(length(version) == 0){
    version <- system("git rev-parse --short HEAD", intern = TRUE)
  }
  
  in_tbl_file <- file.path(test_folder, paste0(test_name, "_in", '.csv'))
  
  
  # @TODO
  # Add support for lists of tables & associated row/cols
  out_tbl_files <- list()
  out_tbl_files <- append( out_tbl_files, 
                           paste0(test_name, '_out_1.csv') )
  write.csv(out_table,
            file.path(test_folder, paste0(test_name, '_out_1.csv') ) ,
            row.names = FALSE)
  
  
  if(has_col_tbl == TRUE){
    out_tbl_files <- append( out_tbl_files, 
                             paste0(test_name, '_out_2.csv') )
    
    if( length(docIdMapping) > 0 ){
      # Find documentId instances and replace them
      out_ctbl <- in_ctbl
      
      
      for( i in seq(1, length(docIdMapping))  ){
        out_ctbl <- out_ctbl %>%
          mutate_all( ~str_replace(., unlist(names(docIdMapping[i])), unname(docIdMapping[i])) )
      }
    }
    
    
    write.csv(out_ctbl %>% select(-".ci"),
              file.path(test_folder, paste0(test_name, '_out_2.csv') ) ,
              row.names = FALSE)
  }
  
  if(has_row_tbl == TRUE){
    out_tbl_files <- append( out_tbl_files, 
                             paste0(test_name, '_out_3.csv') )
    
    write.csv(in_rtbl %>% select(-".ri"),
              file.path(test_folder, paste0(test_name, '_out_3.csv') ) ,
              row.names = FALSE)
  }
  
  json_data = list("kind"="OperatorUnitTest",
                   "name"=test_name,
                   "namespace"=namespace,
                   "inputDataUri"=basename(in_tbl_file),
                   "outputDataUri"=out_tbl_files,
                   "columns"=if(unname(unlist(ctx$cnames)) == "") list() else unname(unlist(ctx$cnames)),
                   "rows"=if(unname(unlist(ctx$rnames)) == "") list() else unname(unlist(ctx$rnames)),
                   "colors"=ctx$colors,
                   "labels"=ctx$labels,
                   "yAxis"=yAxis,
                   "xAxis"=xAxis,
                   "generatedOn"=format(Sys.time(), "%x %X %Y"),
                   "version"=version)
  
  json_data <- toJSON(json_data, pretty=TRUE, auto_unbox = TRUE,
                      digits=16)
  
  json_file <- file.path(test_folder, paste0(test_name, '.json'))

  write(json_data, json_file) 
  write.csv(in_tbl, in_tbl_file, row.names = FALSE)
}


check_test_local <- function( out_table, test_name, test_folder = NULL, 
                              metric="eq",
                              absTol=0, relTol=0, r2=0 ){
  if( is.null(test_folder)){
    test_folder <- paste0( getwd(), '/tests' )
  }
  # Loads a variable called testTbl
  load(file.path(test_folder, paste0(test_name, '.Rda')) )
  
  
  compare_results(out_table, testTbl, metric = metric,
                  absTol=absTol, relTol=relTol, r2=r2)
  
  
  # If no error occurs ...
  print( paste0("Test ", test_name, " completed successfuly.\n")  )
}

# metric: c("eq", "tol", "r2")
compare_results <- function( newTbl, testTbl, metric="eq",
                             absTol=0, relTol=0, r2=0){
  newCols <- names(newTbl)
  testCols <- names(testTbl)
  
  if( length(newCols) != length(testCols) ){
    err_msg <- paste0("ERROR: Mismatch between number of columns\n\n",
                     "The new result has ", length(newCols), "columns [",
                     paste(newCols, collapse = ", "), "]\n\n",
                     "The saved result has ", length(testCols), "columns [",
                     paste(testCols, collapse = ", "), "]\n\n")
    
    
    stop(err_msg)
  }
  
  if( nrow(newTbl) != nrow(testTbl) ){
    err_msg <- paste0("ERROR: Mismatch between number of rows.\n\n",
                     "The new result has ", nrow(newTbl), "rows.\n\n",
                     "The saved result has ", nrow(testTbl), "rows.\n\n")
    stop(err_msg)
  }
  
  if(metric == "eq"){
    comparetbl_eq_tol(newTbl, testTbl, newCols, relTol=NULL, absTol=0) 
  }
  
  if(metric == "tol"){
    comparetbl_eq_tol(newTbl, testTbl, newCols, relTol=relTol, absTol=absTol) 
  }
  
  if(metric == "r2"){
    comparetbl_r2(newTbl, testTbl, newCols, test_r2=r2)
  }
}



comparetbl_r2 <- function(newTbl, testTbl, newCols, test_r2=0){
  dt <- sapply(newTbl, class)
  for(i in seq(1, length(newCols))){
    if( !startsWith(newCols[i], ".") && (
      dt == "integer" || dt == "numeric")){
      
      r2 <- cor( newTbl[,i], testTbl[,i]  )[[1]] ** 2
      
      if( all(newTbl[,i]==0) && all(testTbl[,i]==0) ){
        # If both contain only 0's, cor will return NA
        # For the purposes of this test, this will be considered 1
        r2 <- 1
      }
      
      if( r2 <  test_r2 ){
        err_msg <- paste0('Correlation test failed for column ',
                          newCols[i], " --- ",
                          "R2 = ", r2)
        
        stop(err_msg)
      }
      
    }
  }
}


comparetbl_eq_tol <- function(newTbl, testTbl, newCols, relTol=NULL, absTol=0){
  # For the sake of clarity, run the test row-wise
  # Might update this if performance becomes a problem in large tables
  
  dt <- sapply(newTbl, class)
  
  for(i in seq(1, nrow(newTbl))){
    newRow <- newTbl[i,]
    testRow <- testTbl[i,]
    
    for( j in seq(1, length(newCols)) ){
      dtCol <- dt[j]
      if( dtCol == "integer" || dtCol == "numeric" ){
          deltaAbs <- abs(newRow[j] - testRow[j]  )[[1]]
          deltaRel <- abs(1- (newRow[j][[1]]/testRow[j][[1]]  ))
          
          deltaRelInTol <- is.null(relTol) || (
            (newRow[j] == 0 && testRow[j] == 0) ||
              (deltaRel <= relTol)
          )
          
          deltaAbsInTol <- deltaAbs <= absTol
          
          isEq <- deltaAbsInTol && deltaRelInTol
      }else{
        isEq <- newRow[j] == testRow[j]
      }

      if( !isEq ){
        err_msg <- paste0('Comparison test failed at row ',
                         i, " column ", newCols[j], "\n\n.",
                         "Values: ", newRow[j], "  --  ",
                         testRow[j])
        
        stop(err_msg)
      }
    }
  }
}



