build_test_data_local <- function( out_table, ctx, test_name, 
                             test_folder = NULL, version = '',
                             absTol=NULL, relTol=NULL, r2=NULL,
                             docIdMapping=c(),
                             props=c()){
  if( is.null(test_folder)){
    test_folder <- paste0( getwd(), '/tests' )
  }

  # Save for running the tests locally
  # Changes the variable name to a more sensible testTbl name
  testTbl <- out_table
  save(testTbl,file= file.path(test_folder, paste0(test_name, '.Rda')) )
  
  in_proj <- create_input_projection(ctx, docIdMapping)

  # Check if operator output was a table or relation
  if( class(out_table)[[1]] == "JoinOperator"){
    
    #return(list(in_tbl, has_col_tbl, in_ctbl, has_row_tbl, in_ctbl, ctx_colors, labels))
    in_ctbl <- in_proj[[3]]
    has_col_tbl <- in_proj[[4]]
    in_rtbl <- in_proj[[5]]
    has_row_tbl <- in_proj[[6]]
    
    # Main output table
    rr <- out_table$rightRelation
    
    jo <- rr$joinOperators
    outTable1 <- rr$mainRelation$inMemoryTable
    outTable2 <- jo[[1]]$rightRelation$mainRelation$inMemoryTable
    
    outTable3 <- in_ctbl # Why 3
    
    
    outTable4 <- jo[[2]]$rightRelation$inMemoryTable # Why no main?
    outTable5 <- jo[[3]]$rightRelation$mainRelation$inMemoryTable # Why 5?
    
    # If there is no row set, this table is present, but empty
    outTable6 <- in_rtbl # Why 6?
    
    browser()
    #ctx$client$tableSchemaService$list()
    
    
    out_tbl_files <- c()
    
    out_tbl_files <- append( out_tbl_files, 
                             unbox(paste0(test_name, '_out_1.csv') ))
    write.csv(out_table,
              file.path(test_folder, paste0(test_name, '_out_1.csv') ) ,
              row.names = FALSE)
    
    
    if(has_col_tbl == TRUE){
      out_tbl_files <- append( out_tbl_files, 
                               unbox(paste0(test_name, '_out_2.csv')) )
      out_ctbl <- in_ctbl
      if( length(docIdMapping) > 0 ){
        # Find documentId instances and replace them
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
                               unbox(paste0(test_name, '_out_3.csv')) )
      
      write.csv(in_rtbl %>% select(-".ri"),
                file.path(test_folder, paste0(test_name, '_out_3.csv') ) ,
                row.names = FALSE)
    }
    
    unbox_cnames <- lapply( c(unname(unlist(ctx$cnames))), function(x){
      unbox(x)
    })
    
    unbox_rnames <- lapply( c(unname(unlist(ctx$rnames))), function(x){
      unbox(x)
    })
    
    
    unbox_labels <- lapply( labels, function(x){
      unbox(x)
    })
    
    unbox_colors <- lapply( ctx_colors, function(x){
      unbox(x)
    })
    
    
    propVals = c()
    if( length(props) > 0){
      propVals <- data.frame( kind=c("PropertyValue"),
                              name=names(props),
                              value=unlist(unname(props)))
    }
    
    
    in_tbl_file <- file.path(test_folder, paste0(test_name, "_in", '.csv'))
    write.csv(in_tbl, in_tbl_file, row.names = FALSE)
    
    json_data = list("kind"=unbox("OperatorUnitTest"),
                     "name"=unbox(test_name),
                     "namespace"=unbox(namespace),
                     "inputDataUri"=unbox(basename(in_tbl_file)),
                     "outputDataUri"=out_tbl_files,
                     "columns"=if(length(ctx$cnames) == 1 && unname(unlist(ctx$cnames)) == "") list() else unbox_cnames,
                     "rows"=if(length(ctx$rnames) == 1 && unname(unlist(ctx$rnames)) == "") list() else c(unname(unlist(ctx$rnames))),
                     "colors"=if(length(ctx_colors) == 1 && ctx_colors == "") list() else unbox_colors,
                     "labels"=if(is.null(labels) ) list() else unbox_labels,
                     "yAxis"=unbox(yAxis),
                     "xAxis"=unbox(xAxis),
                     "propertyValues"=propVals,
                     "generatedOn"=unbox(format(Sys.time(), "%x %X %Y")),
                     "version"=unbox(version))
    
    
  }else{
    build_test_for_table(in_proj, out_table, ctx, test_name, 
                         test_folder = test_folder, version = version,
                         absTol=absTol, relTol=relTol, r2=r2,
                         docIdMapping=docIdMapping,
                         props=props)  
  }
}

create_input_projection <- function( ctx, docIdMapping ){
  namespace <- ctx$namespace
  proj_names <- ctx$names
  
  # .y, .ci and .ri are always present even if there is nothing set for them
  select_names <- c(".y", ".ci",".ri")
  yAxis <- 'y_values'
  xAxis <- ''
  
  has_y <- TRUE  
  has_x <- FALSE  
  
  # Check whether x axis is set
  if( ".x" %in% proj_names){
    select_names <- append(select_names, ".x")  
    xAxis <- "x_values"
    has_x <- TRUE
  }
  
  labels <- unname(unlist(ctx$labels))
  if(!is.null(labels) ){
    for(i in seq(1, length(labels))){
      select_names <- append(select_names, labels[[i]])
    }
  }
  
  
  ctx_colors <- unname(unlist(ctx$colors))
  if(length(ctx_colors) > 0 && ctx_colors != ""){
    if( any(unlist(lapply(ctx$names, function(x){
      ".colorLevels" == x
    })) ) ){
      select_names <- append(select_names, ".colorLevels")  
    }
  }
  
  in_tbl <- ctx$select(select_names) 
  in_rtbl <- ctx$rselect()
  in_ctbl <- ctx$cselect()
  
  
  if(has_y == TRUE){
    in_tbl <- in_tbl %>%
      rename("y_values"=".y") 
  }
  
  if(has_x == TRUE){
    in_tbl <- in_tbl %>%
      rename("x_values"=".x") 
  }
  
  has_row_tbl <- FALSE
  has_col_tbl <- FALSE
  
  has_clr_tbl <- FALSE
  has_lbl_tbl <- FALSE
  
  # .all -> Empty row or column projection table
  if( length(names(in_rtbl)) > 0 || names(in_rtbl) != ".all" ){
    in_rtbl <- in_rtbl %>% 
      mutate( .ri=seq(0,nrow(.)-1) )
    
    in_tbl <- dplyr::full_join( in_tbl, in_rtbl, by=".ri" ) %>%
      select(-".ri") 
    
    has_row_tbl <- TRUE
  }else{
    in_tbl <- select(in_tbl, -".ri")
  }
  
  if( length(names(in_ctbl)) > 1 || names(in_ctbl) != ".all" ){
    in_ctbl <- in_ctbl %>% mutate( .ci=seq(0,nrow(.)-1) )
    in_tbl <- dplyr::full_join( in_tbl, in_ctbl, by=".ci" ) %>%
      select(-".ci")
    
    has_col_tbl <- TRUE
  }else{
    in_tbl <- select(in_tbl, -".ci")
  }
  
  if(length(ctx_colors) > 0 && ctx_colors != ""){
    for(i in seq(1,length(ctx_colors))){
      in_tbl <- cbind(in_tbl, ctx$select(ctx_colors[[i]]) )
    }
    
    if( any(unlist(lapply(ctx$names, function(x){
      ".colorLevels" == x
    })) ) ){
      in_tbl <- in_tbl %>% select(-".colorLevels")
    }
  }
  
  # Find documentId instances and replace them
  if( length(docIdMapping) > 0 ){
    for( i in seq(1, length(docIdMapping))  ){
      in_tbl <- in_tbl %>%
        mutate_all( ~str_replace(., unlist(names(docIdMapping[i])), unname(docIdMapping[i])) )
    }
  }
  
  
  return(list(in_tbl, has_col_tbl, in_ctbl, has_row_tbl, in_rtbl, ctx_colors, labels))
}

build_test_for_table <- function( in_proj, out_table, ctx, test_name, 
                                   test_folder = NULL, version = '',
                                   absTol=NULL, relTol=NULL, r2=NULL,
                                   docIdMapping=c(),
                                   props=c()
                                   ){
  namespace <- ctx$namespace
  proj_names <- ctx$names
  
  # .y, .ci and .ri are always present even if there is nothing set for them
  select_names <- c(".y", ".ci",".ri")
  yAxis <- 'y_values'
  xAxis <- ''
  
  has_y <- TRUE  
  has_x <- FALSE  
  
  # Check whether x axis is set
  if( ".x" %in% proj_names){
    select_names <- append(select_names, ".x")  
    xAxis <- "x_values"
    has_x <- TRUE
  }
  
  labels <- unname(unlist(ctx$labels))
  if(!is.null(labels) ){
    for(i in seq(1, length(labels))){
      select_names <- append(select_names, labels[[i]])
    }
  }
  
  
  ctx_colors <- unname(unlist(ctx$colors))
  if(length(ctx_colors) > 0 && ctx_colors != ""){
    if( any(unlist(lapply(ctx$names, function(x){
      ".colorLevels" == x
    })) ) ){
      select_names <- append(select_names, ".colorLevels")  
    }
  }
  
  in_tbl <- ctx$select(select_names) 
  in_rtbl <- ctx$rselect()
  in_ctbl <- ctx$cselect()
  
  
  if(has_y == TRUE){
    in_tbl <- in_tbl %>%
      rename("y_values"=".y") 
  }
  
  if(has_x == TRUE){
    in_tbl <- in_tbl %>%
      rename("x_values"=".x") 
  }
  
  has_row_tbl <- FALSE
  has_col_tbl <- FALSE
  
  has_clr_tbl <- FALSE
  has_lbl_tbl <- FALSE
  
  # .all -> Empty row or column projection table
  if( length(names(in_rtbl)) > 0 || names(in_rtbl) != ".all" ){
    in_rtbl <- in_rtbl %>% 
      mutate( .ri=seq(0,nrow(.)-1) )
    
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
  
  if( length(names(in_ctbl)) > 1 || names(in_ctbl) != ".all" ){
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
  
  if(length(ctx_colors) > 0 && ctx_colors != ""){
    for(i in seq(1,length(ctx_colors))){
      in_tbl <- cbind(in_tbl, ctx$select(ctx_colors[[i]]) )
    }
    
    if( any(unlist(lapply(ctx$names, function(x){
      ".colorLevels" == x
    })) ) ){
      in_tbl <- in_tbl %>% select(-".colorLevels")
    }
  }
  
  # Find documentId instances and replace them
  if( length(docIdMapping) > 0 ){
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
  
  
  # @TODO
  # Add support for lists of tables & associated row/cols
  out_tbl_files <- c()
  
  out_tbl_files <- append( out_tbl_files, 
                           unbox(paste0(test_name, '_out_1.csv') ))
  write.csv(out_table,
            file.path(test_folder, paste0(test_name, '_out_1.csv') ) ,
            row.names = FALSE)
  
  
  if(has_col_tbl == TRUE){
    out_tbl_files <- append( out_tbl_files, 
                             unbox(paste0(test_name, '_out_2.csv')) )
    out_ctbl <- in_ctbl
    if( length(docIdMapping) > 0 ){
      # Find documentId instances and replace them
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
                             unbox(paste0(test_name, '_out_3.csv')) )
    
    write.csv(in_rtbl %>% select(-".ri"),
              file.path(test_folder, paste0(test_name, '_out_3.csv') ) ,
              row.names = FALSE)
  }
  
  unbox_cnames <- lapply( c(unname(unlist(ctx$cnames))), function(x){
    unbox(x)
  })
  
  unbox_rnames <- lapply( c(unname(unlist(ctx$rnames))), function(x){
    unbox(x)
  })
  
  
  unbox_labels <- lapply( labels, function(x){
    unbox(x)
  })
  
  unbox_colors <- lapply( ctx_colors, function(x){
    unbox(x)
  })
  
  
  propVals = c()
  if( length(props) > 0){
    propVals <- data.frame( kind=c("PropertyValue"),
                            name=names(props),
                            value=unlist(unname(props)))
  }
  
  
  in_tbl_file <- file.path(test_folder, paste0(test_name, "_in", '.csv'))
  write.csv(in_tbl, in_tbl_file, row.names = FALSE)
  
  json_data = list("kind"=unbox("OperatorUnitTest"),
                   "name"=unbox(test_name),
                   "namespace"=unbox(namespace),
                   "inputDataUri"=unbox(basename(in_tbl_file)),
                   "outputDataUri"=out_tbl_files,
                   "columns"=if(length(ctx$cnames) == 1 && unname(unlist(ctx$cnames)) == "") list() else unbox_cnames,
                   "rows"=if(length(ctx$rnames) == 1 && unname(unlist(ctx$rnames)) == "") list() else c(unname(unlist(ctx$rnames))),
                   "colors"=if(length(ctx_colors) == 1 && ctx_colors == "") list() else unbox_colors,
                   "labels"=if(is.null(labels) ) list() else unbox_labels,
                   "yAxis"=unbox(yAxis),
                   "xAxis"=unbox(xAxis),
                   "propertyValues"=propVals,
                   "generatedOn"=unbox(format(Sys.time(), "%x %X %Y")),
                   "version"=unbox(version))
  
  if( length(docIdMapping) > 0 ){
    fileUris <- unname((docIdMapping))
    json_data <- c(json_data, "inputFileUris"=list(fileUris))
  }
  
  if( !is.null( absTol )){
    json_data <- c(json_data, list("absTol"=unbox(absTol)) )
  }
  
  if( !is.null( relTol )){
    json_data <- c(json_data, list("relTol"=unbox(relTol)))
  }
  if( !is.null( r2 )){
    json_data <- c(json_data, list("r2"=unbox(r2)))
    json_data <- c(json_data, list("equalityMethod"=unbox("R2")))
  }
  
  
  json_data <- toJSON(json_data, pretty=TRUE, auto_unbox = FALSE,
                      digits=16)
  
  json_file <- file.path(test_folder, paste0(test_name, '.json'))
  
  write(json_data, json_file) 
}



