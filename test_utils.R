
build_test_data <- function( out_table, ctx, test_name, test_folder = NULL, version = '' ){
  if( is.null(test_folder)){
    test_folder <- paste0( getwd(), '/tests' )
  }
  
  namespace <- ctx$namespace
  
  
  save(tbl,file= file.path(test_folder, paste0(test_name, '.Rda')) )
  proj_names <- ctx$names
  
  select_names <- c()
  yAxis <- ''
  xAxis <- ''

  has_y <- FALSE  
  has_x <- FALSE  
  
  # Check whether y and x axis are set
  if( ".y" %in% proj_names){
    select_names <- append(select_names, ".y")
    yAxis <- "y_values"
    has_y <- TRUE
  }
  if( ".x" %in% proj_names){
    select_names <- append(select_names, ".x")  
    xAxis <- "x_values"
    has_x <- TRUE
  }
  if( ".ci" %in% proj_names){select_names <- append(select_names, ".ci")  }
  if( ".ri" %in% proj_names){select_names <- append(select_names, ".ri")  }
  
  # Select, if available, .y, .x, .ci and.ri and corresponding row and column tables
  in_tbl <- ctx$select(select_names) 
  in_rtbl <- ctx$rselect()
  in_ctbl <- ctx$cselect()
  
  if(has_y == TRUE){
    in_tbl <- in_tbl%>% rename("y_values"=".y") 
  }
  
  if(has_x == TRUE){
    in_tbl <- in_tbl%>% rename("x_values"=".x") 
  }
  
  
  has_row_tbl <- FALSE
  has_col_tbl <- FALSE
  
  # .all -> Empty row or column projection table
  if( names(in_rtbl) != ".all" ){
    in_rtbl <- in_rtbl %>% mutate( .ri=seq(0,nrow(.)-1) )
    in_tbl <- dplyr::left_join( in_tbl, in_rtbl, by=".ri" ) %>%
      select(-".ri") 
    has_row_tbl <- TRUE
  }else{
    in_tbl <- select(in_tbl, -".ri")
    out_table <- out_table %>% select(-".ri")
  }
  
  if( names(in_ctbl) != ".all" ){
    in_ctbl <- in_ctbl %>% mutate( .ci=seq(0,nrow(.)-1) )
    in_tbl <- dplyr::left_join( in_tbl, in_ctbl, by=".ci" ) %>%
      select(-".ci")

    has_col_tbl <- TRUE
  }else{
    in_tbl <- select(in_tbl, -".ci")
    out_table <- out_table %>% select(-".ci")
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
    
    write.csv(in_ctbl %>% select(-".ci"),
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
