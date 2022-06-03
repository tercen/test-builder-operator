build_test_data_local <- function( out_table, ctx, test_name, 
                             test_folder = NULL, version = '',
                             absTol=NULL, relTol=NULL, r2=NULL,
                             docIdMapping=c(),
                             props=c()){
  if( is.null(test_folder)){
    test_folder <- paste0( getwd(), '/tests' )
  }
  
  namespace <- ctx$namespace
  
  testTbl <- tbl
  save(testTbl,file= file.path(test_folder, paste0(test_name, '.Rda')) )
  proj_names <- ctx$names
  
  # Always present even if there is nothing set for them
  select_names <- c(".y", ".ci",".ri")
  yAxis <- ''
  xAxis <- ''
  
  has_y <- TRUE  
  has_x <- FALSE  
  
  # Check whether x axis is set
  yAxis <- "y_values"

  if( ".x" %in% proj_names){
    select_names <- append(select_names, ".x")  
    xAxis <- "x_values"
    has_x <- TRUE
  }
  
  if(!is.null(unname(unlist(ctx$labels))) ){
    ulbl <- unname(unlist(ctx$labels))
    for(i in seq(1, length(ulbl))){
      select_names <- append(select_names, ulbl[[i]])
    }
  }

  
  if(length(ctx$colors) > 0 && unname(unlist(ctx$colors)) != ""){
    
    if( any(unlist(lapply(ctx$names, function(x){
      ".colorLevels" == x
    })) ) ){
      select_names <- append(select_names, ".colorLevels")  
    }
    
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
  
  #TODO --> ADD THIS
  has_clr_tbl <- FALSE
  has_lbl_tbl <- FALSE
  
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
  
  if(length(ctx$colors) > 0 && unname(unlist(ctx$colors)) != ""){
    clrs <- ctx$colors
    
    for(i in seq(1,length(clrs))){
      in_tbl <- cbind(in_tbl, ctx$select(ctx$colors[[i]]) )
    }
    
    if( any(unlist(lapply(ctx$names, function(x){
      ".colorLevels" == x
    })) ) ){
      in_tbl <- in_tbl %>% select(-".colorLevels")
    }
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
  
  
  unbox_labels <- lapply( c(unname(unlist(ctx$labels))), function(x){
    unbox(x)
  })
  
  unbox_colors <- lapply( c(unname(unlist(ctx$colors))), function(x){
    unbox(x)
  })
  
  
  propVals = c()
  if( length(props) > 0){
    propVals <- data.frame( kind=c("PropertyValue"),
                            name=names(props),
                            value=unlist(unname(props)))
  }
  
  
  
  # Adicionar aqui os metodos de comapração
  json_data = list("kind"=unbox("OperatorUnitTest"),
                   "name"=unbox(test_name),
                   "namespace"=unbox(namespace),
                   "inputDataUri"=unbox(basename(in_tbl_file)),
                   "outputDataUri"=out_tbl_files,
                   "columns"=if(unname(unlist(ctx$cnames)) == "") list() else unbox_cnames,
                   "rows"=if(unname(unlist(ctx$rnames)) == "") list() else c(unname(unlist(ctx$rnames))),
                   "colors"=if(length(ctx$colors) >0 && unname(unlist(ctx$colors)) == "") list() else unbox_colors,
                   "labels"=if(is.null(unname(unlist(ctx$labels))) ) list() else unbox_labels,
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
  
  
  write.csv(in_tbl, in_tbl_file, row.names = FALSE)
}


# "propertyValues": [
# {
#   "kind": "PropertyValue",
#   "name": "multiplicator",
#   "value": "1"
# },
# {
#   "kind": "PropertyValue",
#   "name": "seed",
#   "value": "42"
# }
# ]