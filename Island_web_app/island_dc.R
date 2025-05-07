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
# 
# ## Focusing on Auckland
# edu_a = edu |> focR("Auckland Region")
# edu_a$"Population Count"

#library(esquisse)
# esquisser(edu_a)


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

## Focusing on Auckland
  # getting rid of region/place columns bc only auckland
eth_a = ethnicity |> focR("Auckland Region")

# esquisser(eth_a)
    ### could probably go right to esquisse/ ggplot with this ####


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

# focus on Auckland
exp_a = expat |> focR("Auckland Region")

# esquisser(exp_a)


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

#  !Total_personal_income_code &
income = income |>
  select(!Area_code &
           !Maori_ethnic_group_indicator_summary_code &
           !Sex_code) |>
  rename(c("Income" = `Total_personal_income_description`,
           "Maori Ethnic Indicator" = `Maori_ethnic_group_indicator_summary_description`,
           "Census" = `Census_usually_resident_population_count_aged_15_years_and_over`,
           "Maori Percentage" = `Total_personal_income_by_Maori_ethnic_group_indicator_summary_percent`,
           "Sex Percentage" = `Total_personal_income_by_sex_percent`))

income$Census = as.numeric(income$Census)
income$`Maori Percentage` = as.numeric(income$`Maori Percentage`)
income$`Sex Percentage` = as.numeric(income$`Sex Percentage`)

# Focusing on Total instead of Maori ethnicity or gender
income = income |> filter(`Maori Ethnic Indicator` == "Total" &
                            `Sex_description` == "Total") |>
                  filter(`Income` != "Total" &
                           `Income` != "Total stated" &
                           `Income` != "Not stated") |>
                  select(!`Sex Percentage` & !`Maori Percentage`) |>
                  arrange(`Total_personal_income_code`)
  
  


# Focusing on auckland
income_a = income |> focR("Auckland Region")

View(income_a)
esquisser(income_a)

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
