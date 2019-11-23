library(rsconnect)
library(rstudioapi)    
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rsconnect::deployApp("2020_Primary_Scenarios")
