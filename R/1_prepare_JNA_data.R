##Clear environment, if needed
rm(list = ls())

#### Load packages

#install
# install.packages('visdat')
# install.packages("remotes")
# install.packages("DiagrammeR")
# install.packages('dplyr')
# install.packages("tidyverse")
# install.packages("srvyr")
# install.packages("writexl")
# install.packages("janitor")
# install.packages("robotoolbox")

##load
library(haven)
library(tidyverse)
library(readxl)
library(srvyr)
library(ggplot2)
library(robotoolbox)
library(labelled)
library(remotes)
library(dm)
library(janitor)
library(visdat)
library(dplyr)
library(writexl)

###Once you enter, you will receive your token from KoBO which you need to insert as below
kobo_setup(url = "https://eu.kobotoolbox.org", token = "XXX")


###Run the script below to see list of your surveys
asset_list <- kobo_asset_list()
asset_list

##Find the survey you want to analyse and enter the name as below

uid <- filter(asset_list, name == "XXX") |> ## change the survey name accordingly as shown in KoBo
  pull(uid)

###You will see the number of submissions and name.

asset <- kobo_asset(uid)
asset

###Your data frame will be displayed here without the need for you to download it from KoBo
df <- kobo_data(asset)


main <- pull_tbl(df, main, keyed = TRUE) 
ind <- pull_tbl(df, household_members, keyed = TRUE)

#Select the ones with consent

###Clean dataset

#confirm by using the script below to see if there are any duplicates, if there are, please delete

duplicated(main) # Check if there are any duplicates
sum(duplicated(main)) # Number of duplicates
get_dupes(main)

duplicated(ind) # Check if there are any duplicates
sum(duplicated(ind)) # Number of duplicates
get_dupes(ind)

vis_dat(main) # Example of a missing ind heatmap using the `visdat` package
vis_dat(ind) # Example of a missing ind heatmap using the `visdat` package


###Merge household data to individual data by parent index

main_merged <- left_join(ind, main, by = c("_parent_index"="_index")) %>%
  rename(id_hogar = "_parent_index")%>%
  rename(id_individual = "_index")%>%
  filter(# eligibility criterias for survey
    DEMO_00.6 != "no",
    DEMO_5 != "no",
    DEMO_7 != "return_venezuela",
    DEMO_7 != "move_different_country"
  ) %>%
  mutate(    # need to transform to double for indicator calculation
    FS_D1_Q1 = as.double(as.factor(FS_D1_Q1)),
    FS_D1_Q2 = as.double(as.factor(FS_D1_Q2)),
    FS_D1_Q3 = as.double(as.factor(FS_D1_Q3)),
    FS_D1_Q4 = as.double(as.factor(FS_D1_Q4)),
    FS_D1_Q5 = as.double(as.factor(FS_D1_Q5)),
    FS_D1_Q8 = as.double(as.factor(FS_D1_Q8)),
    FS_D1_Q9 = as.double(as.factor(FS_D1_Q9)),
    FS_D1_Q10 = as.double(as.factor(FS_D1_Q10)),
    FS_D2_Q1 = as.double(as.factor(FS_D2_Q1)),
    FS_D2_Q2 = as.double(as.factor(FS_D2_Q2)),
    FS_D2_Q3 = as.double(as.factor(FS_D2_Q3)),
    FS_D2_Q4 = as.double(as.factor(FS_D2_Q4)),
    FS_D2_Q5 = as.double(as.factor(FS_D1_Q10))
  )



