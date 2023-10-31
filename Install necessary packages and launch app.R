# Install necessary packages and launch robustness app, locally (will open in web browser)

packages=c(
  "knitr",
  'flexdashboard',
  'plotly',
  'ggplot2',
  'shiny',
  'shinyWidgets',
  'stringr',
  'gdata',
  'nsga2R',
  'DT',
  'openxlsx',
  'prospectr',
  'shinyBS',
  'Kendall',
  'here'
  
)

to_install=packages[!(packages %in% installed.packages()[,"Package"])]

if(length(to_install)>0) install.packages(to_install)

library(here)

####### Commands to render Flexdashboard HTML

setwd(here())
filename='Web app.Rmd'

# make errors print in r console
# options(shiny.error=browser)
# stop errors from printing in r console
# options(shiny.error=NULL)

# launch app to your web browser
rmarkdown::run(filename, shiny_args = list(port = 3838, host = "0.0.0.0", launch.browser=T))
