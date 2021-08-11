# This script is used for updating the EpiNow2 package and other required
# packages to latest stable version. 

install.packages("drat")
drat:::add("epiforecasts")

# EpiNow stuff
# install.packages("EpiNow") # Deprecated replaced by EpiNow2 
install.packages("EpiNow2")
# install.packages("NCoVUtils") # Deprecated replaced by 
install.packages("EpiSoon")
#install.packages("NCoVUtils") # Deprecated replaced by covidregionaldata
install.packages("covidregionaldata")

# Other packages used
install.packages("tidyverse")
install.packages("lubridate")
install.packages("future")
