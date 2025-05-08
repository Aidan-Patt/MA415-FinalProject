#############

## FUNCTIONS FOR DATA CLEANING FOR FINAL PROJECT
library(dplyr)


# Function to read multiple files from a folder and make them into tibbles
fttib = function(filenames, x = list()){
  for(file in filenames){
    data = read_csv(file)
    x[[file]] = data
  }
  return(x)
}

# Function to combine a list of tibble into one tibble
ltotib = function(xl, tot = list()){
  for(i in seq(1,length(xl))){
    tot = bind_rows(tot,xl[[i]])
  }
  return(tot)
}

#######
# Function combining preivous two to read multiple files, then make one tibble
fto1tib = function(filenames){
  
  # section that gets filenames and reads them into a list containing
  # multiple tibbles
  x = list()
  for(file in filenames){
    data = read_csv(file)
    x[[file]] = data
  }
  
  # turning multiple tibbles into one tibble
  tot = list()
  for(i in seq(1,length(x))){
    tot = bind_rows(tot,x[[i]])
  }
  return(tot)
}

# Function to filter for only regions in North Island
    # census is all of new zealand so have to cut out a lot

nicut = function(x){
  x = x |> filter(Area_description == "Northland Region" |
                    Area_description == "Auckland Region" |
                    Area_description == "Coromandel" |
                    grepl("Bay of Plenty", x$Area_description) |
                    Area_description == "Gisborne Region" |
                    Area_description == "Waikato Region" |
                    Area_description == "Lake Taupo Bays" |
                    Area_description == "Hawke's Bay Region" |
                    Area_description == "Manawatu-Wanganui Region" |
                    Area_description == "Wellington Region"|
                    Area_description == "South Wairarapa District")
  
}



# Need a function to remove all columns that only have one unique answer
drop_one_val_col = function(x){
  
  # getting number of columns of data set
  ncol = dim(x)[2]
  # creating a list to put in names of columns to eliminate
  cols_to_elim = c()
  
  # collect all column names to be eliminated
  for(i in 1:ncol){
    
    
    
    if(is_tibble(x) == TRUE){
      num_uvals = nrow(unique(x[,i]))
    }
    else{
      num_uvals = length(unique(x[,i]))
    }
    if(num_uvals == 1){
      cols_to_elim = c(cols_to_elim, colnames(x)[i])
    }
  }
  
  # print(cols_to_elim)
  # used during report but commented out so not constantly listing columns
  # that are deleted
  
  # get rid of columns from the data set
  t = x |> select(!all_of(cols_to_elim))
  return(t)
  
}

## Focus on one region ##
focR = function(data, region){
  data |> filter(Area_description == region) |>
    drop_one_val_col()
}




