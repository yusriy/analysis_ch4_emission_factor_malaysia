#### Malaysian Emission Factor Analysis ####
## Author: Yusri Yusup, Ph.D.##

##### Install and load packages #####

#install.packages(c("httr","jsonlite","readr","arrow"))
library(httr)
library(jsonlite)
library(readr)
library(arrow)


##### Data Download #####

id_db <- "89a78907-1dd9-49f7-87d3-8d4e8b1baaa5"
id_table <- "ff844ee0-03d8-4531-ae43-14f4bbdf4cd4"
url <- "https://tidbrepo.usm.my/"

