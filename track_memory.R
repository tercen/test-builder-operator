it <- 0
max_its <- 5000000

while( it < max_its){
  mem_usage <- system('free --mega', intern=TRUE)
  mem_usage <- strsplit( mem_usage[2], ' ', fixed=TRUE )[[1]]
  mem_usage <- strsplit( paste(unique(mem_usage), collapse = ' '), ' ', fixed=TRUE)[[1]]
  
  used_mem <- as.numeric(mem_usage[5]) + as.numeric(mem_usage[7])
  
  if( is.finite(used_mem )){
    system(paste0('echo ', used_mem, ' >> mem.txt'))
    # CHANGE TO
    # write(used_mem,file="mem.txt",append=TRUE)
  }
  
  
  
  Sys.sleep(0.05)
  
  if( file.exists('stop.txt')){
    it <- max_its *2
  }
}
