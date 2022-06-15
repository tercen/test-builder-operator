# NOTE: For dev purposes only. DELETE.
lapply( Sys.glob("./tests/*.csv"), function(x) { unlink(x) } )
lapply( Sys.glob("./tests/*.json"), function(x) { unlink(x) } )
lapply( Sys.glob("./tests/*.Rda"), function(x) { unlink(x) } )

source('test_build_schema.R')
source('test_build_list.R')
source('test_build.R')

# lapply( Sys.glob("./tests/*.csv"), function(x) { unlink(x) } )
# lapply( Sys.glob("./tests/*.json"), function(x) { unlink(x) } )
# lapply( Sys.glob("./tests/*.Rda"), function(x) { unlink(x) } )