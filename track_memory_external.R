
it <- 0
max_its <- 5000000



get_mem <- function(mem_usage_text, image_name){
  tercen_idx <- which(unlist(lapply(mem_usage$V1, function(x){
    x == image_name
  })))
  
  mem_usage_line <- mem_usage$V2[tercen_idx[length(tercen_idx)]]
  mem_usage_line <- strsplit( mem_usage_line, '/', fixed=TRUE )[[1]]
  mem_usage_line <- mem_usage_line[1]
  
  is_mb <- FALSE
  
  spl <- strsplit(mem_usage_line, 'GiB', fixed=TRUE)[[1]]
  if( length(spl) == 1 ){
    spl <- strsplit(mem_usage_line, 'MiB', fixed=TRUE)[[1]]
    is_mb <- TRUE
  }
  
  spl <- as.numeric(spl[1])
  
  if(is_mb == FALSE){
    spl <- spl * 1024
  }
  
  return(spl)
}

while( it < max_its && !file.exists('stop_ext.txt') ){
  
  if( file.size('/home/rstudio/mem_track/mem_usage.txt') == 0L ){
    Sys.sleep(0.25)
    next
  } 
  mem_usage <- read.csv('/home/rstudio/mem_track/mem_usage.txt', sep=',', header=FALSE)
  
  
  used_mem <- get_mem(mem_usage, 'tercen_studio-tercen-1') +
    get_mem(mem_usage, 'tercen_studio-runtime-docker-1') + 
    get_mem(mem_usage, 'tercen_studio-couchdb-1') 
  
  if( is.finite(used_mem )){
    system(paste0('echo ', used_mem, ' >> mem_ext.txt'))
  }
  
  
  
  Sys.sleep(0.75)
  
  if( file.exists('stop.txt')){
    it <- max_its *2
  }
}
