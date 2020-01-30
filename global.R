library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)


#library(shinydashboard)
#library(shinysky)


input_file_path = file.path('www','Input')
(input_file_list = list.files(input_file_path))
(table_list = gsub('.rds','',input_file_list))

for(entry in table_list){
 (cmd = paste0(entry,' = readRDS("',file.path(input_file_path,entry),'.rds")'))
 eval(parse(text = cmd))
}

initial_file = 'datafile1'

annotate = F
