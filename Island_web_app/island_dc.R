### Data Cleaning

# Name: Aidan Patt
# Sat May  3 14:41:43 2025 ------------------------------

library(dplyr)
library(readr)
library(vroom)
library(tidyverse)


########
source("My_functions.R")

# set up directory, make sure set directory to folder where you see
# .gitignore, README, etc

wd = getwd()
wd = paste0(wd,"/island_data")

folders = list("/education_data", 
               "/ethnicity_data",
               "/expat_data",
               "/housing_data",
               "/income_data",
               "/job_data",
               "/maori_data",
               "/religion_data",
               "/smoking_data",
               "/transportation_data")

all = list()
  for(i in seq(1,length(folders))){
    x_wd = paste0(wd, folders[i])
    x_names = list.files(path = x_wd,
                         pattern = "\\.csv$",
                         full.names = TRUE)
    all[[i]] = fto1tib(x_names)
    all[[i]] = nicut(all[[i]])
  }

## make note population count is  usually those 15 yrs or greater ##

#### EDUCATION #####
edu = all[[1]]

##  removing code columns that are redundant
  # bc they are explained by more intuitive columns with same data 
  #e.g area code removed but keeping region name

edu = edu |> select(!Area_code &
                      !Highest_qualification_code &
                      !Maori_ethnic_group_indicator_summary_code) |>
  rename(c("Highest Qualification" = `Highest_qualification_description`,
           "Ethnicity" = `Maori_ethnic_group_indicator_summary_description`,
           "Population Count" = `Census_usually_resident_population_count_aged_15_years_and_over`,
           "Percentage of Maori who Obtained Qualifcation Level" = `Highest_qualification_by_Maori_ethnic_group_indicator_summary_percent`,
           "Population Percent that Obtained Qualificaton Level" = `Highest_qualification_percent`))

edu$`Population Percent that Obtained Qualificaton Level` = as.numeric(edu$`Population Percent that Obtained Qualificaton Level`)
edu$`Population Count` = as.numeric(edu$`Population Count`)

## Focusing on Auckland
edu_a = edu |> focR("Auckland Region")

# Focus on total population education without differing maori or not
tot_edu_a = edu_a |> filter(Ethnicity == "Total") |>
  drop_one_val_col() |> filter(!`Highest Qualification` == "Total" &
                                 !`Highest Qualification` == "Total stated")

# Changing certificate values so can merge them together
for(i in seq(1,length(tot_edu_a$Year))){
  
  # have all the certificateand diploma level 5 as certifications
  if(grepl("certificate", tot_edu_a$`Highest Qualification`[i])|
     grepl("5",tot_edu_a$`Highest Qualification`[i])){
    tot_edu_a$`Highest Qualification`[i] = "Certifications"
    
  }
    
  # classigy bachelors and diploma level 6 as Bachelors or equivalent
  if(grepl("6 diploma",tot_edu_a$`Highest Qualification`[i])|
     grepl("Bach", tot_edu_a$`Highest Qualification`[i])){
    tot_edu_a$`Highest Qualification`[i] = "Bachelors or equivalent"
    
  }
     # Classify no qualification and not elseqhere include as other
  if(grepl("No", tot_edu_a$`Highest Qualification`[i])){
    tot_edu_a$`Highest Qualification`[i] = "Other"
  }
  
     
}


test = tot_edu_a

test = test |> group_by(`Year`, `Highest Qualification`) |>
  summarise(across(c(`Population Count`, `Population Percent that Obtained Qualificaton Level`),
                   sum))
# library(esquisse)
# esquisser(test)


### ETHNICITY ####
ethnicity = all[[2]]

##  removing code columns that are redundant
  # bc they are explained by more intuitive columns with same data 
  #e.g area code removed but keeping region name

ethnicity = ethnicity |> select(!Area_code &
                                  !Ethnic_group_total_responses_code)

## Focusing on Auckland
  # getting rid of region/place columns bc only auckland
eth_a = ethnicity |> focR("Auckland Region")


    ### could probably go right to esquisse/ ggplot with this ####


### EXPAT ####
expat = all[[3]]

##  removing code columns that are redundant
  # bc they are explained by more intuitive columns with same data 
  #e.g area code removed but keeping region name

expat = expat |> select(!Area_code &
                          !Years_since_arrival_in_New_Zealand_code)

# focus on Auckland
exp_a = expat |> focR("Auckland Region")


##### HOUSING #####
house = all[[4]]

##  removing code columns that are redundant
  # bc they are explained by more intuitive columns with same data 
  #e.g area code removed but keeping region name

house = house |> 
  select(!Area_code &
        !Tenure_of_household_code)|> 
  rename(c("Occupation Type" = `Tenure_of_household_description`,
            "Number of Households" = `Households_in_occupied_private_dwellings`,
           "Percent" = `Tenure_of_household_percent`))

## Auckland focus
house_a = house |> focR("Auckland Region")
  


##### INCOME ####
income = all[[5]]

##  removing code columns that are redundant
  # bc they are explained by more intuitive columns with same data 
  #e.g area code removed but keeping region name
income = income |>
  select(!Area_code &
           !Total_personal_income_code &
           !Maori_ethnic_group_indicator_summary_code)

# Focusing on auckland
income_a = income |> focR("Auckland Region")

#### JOB ####
job = all[[6]]

##  removing code columns that are redundant
  # bc they are explained by more intuitive columns with same data 
  #e.g area code removed but keeping region name
job = job |>
  select(!Area_code &
           !Work_and_labour_force_status_code &
           !Maori_ethnic_group_indicator_summary_code) |>
  rename(c("Census(usually pop count age >= 15 yrs)" = 
             `Census_usually_resident_population_count_aged_15_years_and_over`))

# Auckland focus
job_a = job |> focR("Auckland Region")


###### MAORI #####
maori = all[[7]]

##  removing code columns that are redundant
  # bc they are explained by more intuitive columns with same data 
  #e.g area code removed but keeping region name
maori = maori |>
  select(!Area_code &
          !Maori_ethnic_group_indicator_summary_code)

## Focus Auckland
maori_a = maori |> focR("Auckland Region")


#### RELIGION ####
religion = all[[8]]

##  removing code columns that are redundant
  # bc they are explained by more intuitive columns with same data 
  #e.g area code removed but keeping region name
religion = religion |>
  select(!Area_code &
           !Religious_affiliation_total_responses_code)

# Focus Auckland
religion_a = religion |> focR("Auckland Region")

#### SMOKING #####
smoking = all[[9]]

##  removing code columns that are redundant
  # bc they are explained by more intuitive columns with same data 
  #e.g area code removed but keeping region name
smoking = smoking |>
  select(!Area_code &
           !Cigarette_smoking_behaviour_code &
           !Ethnic_group_total_responses_code)

# Focus on Auckland
smoking_a = smoking |> focR("Auckland Region")


#### TRANSPORTATION ####
transport = all[[10]]

##  removing code columns that are redundant
  # bc they are explained by more intuitive columns with same data 
  #e.g area code removed but keeping region name
transport = transport |>
  select(!Area_code &
           !Main_means_of_travel_to_education_code)

# Focus on Auckland only 2018
transport_a = transport |> focR("Auckland Region")
