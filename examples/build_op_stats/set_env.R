Sys.setenv(TERCENUSER='testbot') #'testbot'
Sys.setenv(TERCENPW='testbot') # 'testbot'
Sys.setenv(TERCENEMAIL='testbot@tercen.com') #'testbot@tercen.com'
Sys.setenv(TERCENTEAM='test_team_bot') #'test_team_bot'

# Sys.setenv(OPERATORNAME='PCA') #'PCA'
# Sys.setenv(OPERATORVERSION='1.3.2') #'PCA'
# Sys.setenv(OPERATORURL='http://github.com/tercen/pca_operator') #'PCA'
# Sys.setenv(DATADESIGN='c03')

Sys.setenv(OPERATORNAME='Minus')
Sys.setenv(OPERATORVERSION='0.1.2')
Sys.setenv(OPERATORURL='https://github.com/tercen/minus_operator') #'PCA'
Sys.setenv(DATADESIGN='c04_groups')


Sys.setenv(MINCOLS=5)
Sys.setenv(MINROWS=5)


Sys.setenv(MAXCOLS=500)
Sys.setenv(MAXROWS=500)


Sys.setenv(NSTEPCOLS=3)
Sys.setenv(NSTEPROWS=3)

Sys.setenv(OPEXECITS=3)


Sys.setenv(GITHUBTOKEN='')


report_file <- paste0( 
  '/home/rstudio/projects/test_builder_operator/examples/build_op_stats/report_',
  Sys.getenv('OPERATORNAME'),
  '_',
  Sys.getenv('OPERATORVERSION'),
  '.txt'
  )

report_img <- paste0( 
  '/home/rstudio/projects/test_builder_operator/examples/build_op_stats/report_',
  Sys.getenv('OPERATORNAME'),
  '_',
  Sys.getenv('OPERATORVERSION'),
  '.png'
)

Sys.setenv(EXTERNALMEMFILE='/home/rstudio/mem_track/mem_usage.txt')
Sys.setenv(REPORTFILE=report_file)
Sys.setenv(REPORTIMAGE=report_img)
