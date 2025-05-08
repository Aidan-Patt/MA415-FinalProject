### Data Cleaning

# Name: Aidan Patt
# Sat May  3 14:41:43 2025 ------------------------------

library(dplyr)
library(readr)
library(vroom)
library(tidyverse)
library(esquisse)
#####

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

# focus on population total, Maori and non Maori
edu = edu |> filter(Ethnicity == "Total") |>
  drop_one_val_col() |> filter(!`Highest Qualification` == "Total" &
                                 !`Highest Qualification` == "Total stated")

# Changing certificate values so can merge them together
for(i in seq(1,length(edu$Year))){
  
  # have all the certificateand diploma level 5 as certifications
  if(grepl("certificate", edu$`Highest Qualification`[i])|
     grepl("5",edu$`Highest Qualification`[i])){
    edu$`Highest Qualification`[i] = "Certifications"
    
  }
  
  # classigy bachelors and diploma level 6 as Bachelors or equivalent
  if(grepl("6 diploma",edu$`Highest Qualification`[i])|
     grepl("Bach", edu$`Highest Qualification`[i])){
    edu$`Highest Qualification`[i] = "Bachelors or equivalent"
    
  }
  # Classify no qualification and not elseqhere include as other
  if(grepl("No", edu$`Highest Qualification`[i])){
    edu$`Highest Qualification`[i] = "Other"
  }
}

## Group similar levels of education together
edu = edu |> group_by(`Year`, `Area_description`, `Highest Qualification`) |>
  summarise(across(c(`Population Count`, `Population Percent that Obtained Qualificaton Level`),
                   sum))

edu = edu |> filter(!`Area_description` == "Oceanic Bay of Plenty Region" &
                      !`Area_description` == "Islands Bay of Plenty Region" &
                      !`Highest Qualification` == "Other")
  


### ETHNICITY ####
ethnicity = all[[2]]



##  removing code columns that are redundant
  # bc they are explained by more intuitive columns with same data 
  #e.g area code removed but keeping region name

ethnicity = ethnicity |> 
  select(!Area_code & !Ethnic_group_total_responses_code &!Area_type) |>
  rename(c("Population Count" = `Census_usually_resident_population_count`,
           "Ethnic Group" = `Ethnic_group_total_responses_description`,
           "Percent of Population" = `Ethnic_group_total_responses_percent`)) |>
  filter(`Ethnic Group`!= "Total" & `Ethnic Group` != "Total stated" &
           `Ethnic Group` != "Not elsewhere included")
  
ethnicity$`Population Count` = as.numeric(ethnicity$`Population Count`)
ethnicity$`Percent of Population` = as.numeric(ethnicity$`Percent of Population`)

# Making Count in the thousands
ethnicity = ethnicity |> mutate(`Population Count` = `Population Count`/1000)


### EXPAT ####
expat = all[[3]]

##  removing code columns that are redundant
  # bc they are explained by more intuitive columns with same data 
  #e.g area code removed but keeping region name

  # !Years_since_arrival_in_New_Zealand_code
expat = expat |> 
  select(!Area_code & !Area_type) |>
  rename(c("Years since arrival" = `Years_since_arrival_in_New_Zealand_description`,
           "Number Born Overseas" = `Overseas_born_census_usually_resident_population_count`,
           "Percentage" = `Years_since_arrival_in_New_Zealand_percent`)) |>
  filter(`Years since arrival` != "Total" & 
           `Years since arrival` != "Total stated" &
           `Years since arrival` != "Not elsewhere included")

expat$`Number Born Overseas` = as.numeric(expat$`Number Born Overseas`)
expat$`Percentage` = as.numeric(expat$`Percentage`)
expat$`Years_since_arrival_in_New_Zealand_code` = as.numeric(expat$Years_since_arrival_in_New_Zealand_code)

test = expat |> group_by(`Years_since_arrival_in_New_Zealand_code`,
                         `Years since arrival`) |> arrange(`Years_since_arrival_in_New_Zealand_code`)
# focus on Auckland
# exp_a = test |> focR("Auckland Region") |> filter(`Year` == 2018)



##### HOUSING #####
house = all[[4]]

##  removing code columns that are redundant
  # bc they are explained by more intuitive columns with same data 
  #e.g area code removed but keeping region name
# making better column names, and filtering out redundant rows

house = house |> 
  select(!Area_code &
        !Tenure_of_household_code &
          !Area_type)|> 
  rename(c("Tenure Type" = `Tenure_of_household_description`,
            "Number of Households" = `Households_in_occupied_private_dwellings`,
           "Percent" = `Tenure_of_household_percent`)) |>
  filter(!`Tenure Type` == "Total" & 
           !`Tenure Type` == "Total stated" )

# changing the values to numbers
house$`Number of Households` = as.numeric(house$`Number of Households`)
house$Percent = as.numeric(house$Percent)

## Auckland focus
# house_a = house |> focR("Auckland Region")
  

##### INCOME ####
income = all[[5]]

##  removing code columns that are redundant
  # bc they are explained by more intuitive columns with same data 
  #e.g area code removed but keeping region name

#  !Total_personal_income_code &
income = income |>
  select(!Area_code &
           !Maori_ethnic_group_indicator_summary_code &
           !Sex_code &
         !Area_type) |>
  rename(c("Income" = `Total_personal_income_description`,
           "Maori Ethnic Indicator" = `Maori_ethnic_group_indicator_summary_description`,
           "Census" = `Census_usually_resident_population_count_aged_15_years_and_over`,
           "Maori Percentage" = `Total_personal_income_by_Maori_ethnic_group_indicator_summary_percent`,
           "Sex Percentage" = `Total_personal_income_by_sex_percent`))

income$Census = as.numeric(income$Census)
income$`Maori Percentage` = as.numeric(income$`Maori Percentage`)
income$`Sex Percentage` = as.numeric(income$`Sex Percentage`)
income$Total_personal_income_code = as.numeric(income$Total_personal_income_code)

# Focusing on Total instead of Maori ethnicity or gender
income = income |> filter(`Maori Ethnic Indicator` == "Total" &
                            `Sex_description` == "Total") |>
                  filter(`Income` != "Total" &
                           `Income` != "Total stated" &
                           `Income` != "Not stated") |>
                  select(!`Sex Percentage` & !`Maori Percentage`
                         & !`Maori Ethnic Indicator` &
                           !`Sex_description`)
income = income |> group_by(`Total_personal_income_code`, `Income`) |>
  arrange(`Total_personal_income_code`)




#### JOB ####
job = all[[6]]

##  removing code columns that are redundant
  # bc they are explained by more intuitive columns with same data 
  #e.g area code removed but keeping region name
# renaming columns to more succint names and filtering redundants rows
job = job |>
  select(!Area_code &
           !Work_and_labour_force_status_code &
           !Maori_ethnic_group_indicator_summary_code) |>
  rename(c("Census" = 
             `Census_usually_resident_population_count_aged_15_years_and_over`,
           "Job Status Description" =
             `Work_and_labour_force_status_description`
             )) |>
  filter(`Maori_ethnic_group_indicator_summary_description` == "Total" &
           !`Job Status Description` == "Total" &
           !`Job Status Description` == "Total stated")

# making numeric columns numeric
job$`Census` = as.numeric(job$`Census`)
job$`Work_and_labour_force_status_percent` = as.numeric(job$`Work_and_labour_force_status_percent`)

# Auckland focus
# job_a = job |> focR("Auckland Region")




###### MAORI #####
maori = all[[7]]

##  removing code columns that are redundant
  # bc they are explained by more intuitive columns with same data 
  #e.g area code removed but keeping region name
maori = maori |>
  select(!Area_code &
          !Maori_ethnic_group_indicator_summary_code &
         !Area_type)|>
  rename(c("Census" = `Census_usually_resident_population_count`))

# making columns taht should be numeric
maori$`Census` = as.numeric(maori$`Census`)


## Focus Auckland
# maori_a = maori |> focR("Auckland Region")


#### RELIGION ####
religion = all[[8]]

##  removing code columns that are redundant
  # bc they are explained by more intuitive columns with same data 
  #e.g area code removed but keeping region name
# renaming columns to more succint names, and getting rid of rendundant rows
religion = religion |>
  select(!Area_code &
           !Religious_affiliation_total_responses_code &
           !Area_type) |>
  rename(c("Religion" = `Religious_affiliation_total_responses_description`,
           "Census" = `Census_usually_resident_population_count`,
           "Percent" = Religious_affiliation_total_responses_percent)) |>
  filter(!`Religion` == "Total" & !`Religion`== "Total stated" &
           !`Religion` == "Spiritualism and New Age religions" &
           !`Religion` == "Other religions, beliefs, and philosophies" &
           !`Religion` == "Not elsewhere included" &
           !`Religion` == "Object to answering")

religion$`Census` = as.numeric(religion$`Census`)
religion$`Percent` = as.numeric(religion$`Percent`)
# Focus Auckland
# religion_a = religion |> focR("Auckland Region")


#### SMOKING #####
smoking = all[[9]]

##  removing code columns that are redundant
  # bc they are explained by more intuitive columns with same data 
  #e.g area code removed but keeping region name
# rename to more succinct columns and filter unnecessary rows of data
  # want to examine all smoking behavior not interested in smoking behavior by
  # ethnic demographic
smoking = smoking |>
  select(!Area_code &
           !Cigarette_smoking_behaviour_code &
           !Ethnic_group_total_responses_code &
           !Area_type) |>
  rename(c("Cigarette Smoking Behavior" = `Cigarette_smoking_behaviour_description`,
           "Ethnic Group" = `Ethnic_group_total_responses_description`,
           "Census" = `Census_usually_resident_population_count_aged_15_years_and_over`,
           "Percent" = `Cigarette_smoking_behaviour_percent`)) |>
  filter(`Ethnic Group` == "Total stated")

# making number columns numeric
smoking$`Census` = as.numeric(smoking$`Census`)
smoking$`Percent` = as.numeric(smoking$`Percent`)


smoking = smoking |> filter(!`Cigarette Smoking Behavior` == "Total" &
                              !`Cigarette Smoking Behavior` == "Total stated" &
                              !`Cigarette Smoking Behavior` == "Not elsewhere included")


#### TRANSPORTATION ####
transport = all[[10]]

##  removing code columns that are redundant
  # bc they are explained by more intuitive columns with same data 
  #e.g area code removed but keeping region name
transport = transport |>
  select(!Area_code &
           !Main_means_of_travel_to_education_code &
           !Area_type) |>
  rename(c("Means of Transport" = `Main_means_of_travel_to_education_description`,
           "Census" = `Census_usually_resident_population_count_participating_in_study`,
           "Percent" = `Main_means_of_travel_to_education_percent`)) |>
  filter(!`Means of Transport` == "Total" &
           !`Means of Transport` == "Total stated" &
           !`Means of Transport` == "Not elsewhere included")

transport$`Census` = as.numeric(transport$`Census`)
transport$`Percent` = as.numeric(transport$`Percent`)

# Focus on Auckland only 2018
#transport_a = transport |> focR("Auckland Region")


