### Data Cleaning

# Name: Aidan Patt
# Sat May  3 14:41:43 2025 ------------------------------

library(dplyr)
library(readr)
library(vroom)
library(tidyverse)

# getting file names
wd = getwd()
wd = paste0(wd,"/island_data")
fnames = list.files(path = wd,
                    pattern = "\\.csv$",
                    full.names = TRUE)

fnames
# list to store data frames
dlist = list()

# make list of all the files
for(file in fnames){
  data = read_csv(file)
  dlist[[file]] = data
}

# change the file names
# for(i in length(dlist)){
#   dlist[i] = 
# } 


dlist[[1]]
is_tibble(dlist[[1]])
