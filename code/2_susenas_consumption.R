# This file is used to combine SUSENAS consumption data with IDN food composition tables 
#
# Name: Laura G. Elsler, Farid Annam
# Affiliation: Harvard T.H. Chan School of Public Health
# Email: l.elsler@outlook.com
# For: north_maluku
# Date updated: 8/6/2026
###################################################################################################################
# clear workspace
rm(list = ls())
graphics.off()

# libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(purrr)

# directories
wk_dir <- '~Documents/Github/north_maluku'
susenas_dir <- '~Desktop/susenas_data'

## food composition tables
nonaquatic_fct <- read.csv(file.path(wk_dir, 'data/indonesia_fct_nonaquatic_food.csv'))
aquatic_fct <- read.csv(file.path(wk_dir, 'data/indonesia_fct_aquatic_food.csv'))

## read DRI file
DRI_LIST <- read.csv(file.path(wk_dir, 'data/0_DRI_natl_academies.csv'))
DRI <- DRI_list[1,2:20]
# units for minerals from here: https://www.ncbi.nlm.nih.gov/books/NBK545442/table/appJ_tab3/?report=objectonly
# units for vitamins from here: https://www.ncbi.nlm.nih.gov/books/NBK56068/table/summarytables.t2/?report=objectonly
#! check if the units are the same as in the food composition tables

## units
susenas_units <- read_xlsx(file.path(wk_dir, 'data/food_composition_units.xlsx')) 

## susenas consumption data
blok_41 <- read.dbf(file.path(susenas_dir, 'blok41_51_94.dbf'))


###################################################################################################################
## Clean up SUSENAS blok41

# your vector of new names
new_names <- c("province", "district", "urban1_rural2", "census_complete1_noncomplete2345", 
               "hh_members", 
               "coicop", 
               "subgroup_code",
               "food_item_id_urut", 
               "volume_weekly_hh_purchased", "value_weekly_hh_purchased", 
               "volume_weekly_hh_subsistence", "value_weekly_hh_subsistence", 
               "volume_weekly_hh_total", "value_weekly_hh_total", 
               "kalories_weekly_hh_total", "protein_weekly_hh_total", "fat_weekly_hh_total", "carbohydrates_weekly_hh_total",
               "hh_estimate_scaling", "citizen_estimate_scaling", "sample_id", "hh_id",
               "primary_sampling_unit", "secondary_sampling_unit",
               "strata", 
               "hh_id_data_link")

# assign
names(blok_41) <- new_names

## METADATA
# subgroup_code: 0 = indicates a food category in food_item_id_urut, 
#               non-0 = corresponds to the food category numbers in SUSENAS survey (e.g., 1-PADIAN) 
# food_item_id_urut corresponds to the number linked to each food item and food category in SUSENAS survey


###################################################################################################################
### add units

susenas_units_clean <- susenas_units %>% 
  ## clean up units
  mutate(
    new_unit   = "gram",
    new_value  = case_when(
      unit_clean == "gram"  ~ Value,
      unit_clean == "kg"    ~ Value * 1000,
      unit_clean == "ounce" ~ Value * 100,        # farids comment: ...
      unit_clean == "ml"    ~ Value * 1,          # assumes water
      unit_clean == "liter" ~ Value * 1000,       # assumes water
      unit_clean == "galon" ~ Value * 19000,      # indonesian water gallon
      TRUE                  ~ NA_real_
    )
  ) %>% 
  ## remove NA and batang values
  filter(!Units == 'batang') %>% 
  filter(!is.na(Units)) 


###################################################################################################################
### data join

consumption_nutrient <- blok_41 %>% 
  ## add nonaquatic foods fct
  left_join(nonaquatic_fct %>% 
              select(susenas_code, food_group, water:bdd) %>% 
              mutate(aqua = 'nonaquatic'), 
              by = c('food_item_id_urut' = 'susenas_code')) %>%
  ## add aquatic foods fct
  left_join(aquatic_fct %>% 
              select(susenas_code, food_group, water:bdd) %>% 
              mutate(aqua = 'aquatic'), 
            by = c('food_item_id_urut' = 'susenas_code')) %>% 
  ## add units
  left_join(susenas_units_clean %>% 
              select(susenas_code, new_unit, new_value) %>% 
              rename(unit = new_unit, unit_value = new_value),
              by = c('susenas_code' = 'susenas_code'))

## example
consumption_beef <- consumption_nutrient %>% 
  filter(susenas_code == 53) %>% # daging sapi
  mutate(
    consumption_weekly_hh_purchased = unit_value * volume_weekly_hh_purchased, 
    consumption_weekly_hh_subsistence = unit_value * volume_weekly_hh_subsistence, 
    consumption_total = unit_value * volume_weekly_hh_total
    )


###################################################################################################################
# to do list 

## julius.ai prompt: my dataset contains these variables (change the variable names e.g. food_item_id_urut = food_id), command + F
# laura's example prompt: suppose my data contains bilateral trade records that is exporter, importer, year, volume, furniture_part, furniture_item, furniture_category, production_cat, tox1, tox2, tox3, tox4, ... tox20
# production_cat is wood or plastic. 
# what to do: 
# understand if volume variable is reasonable ... 
# first calculate the weighted avg concentration of tox1 to tox20 by production_cat. 
# create a table with the values and relative difference. 
# create two spider plots with the toxin concentrations (all using their own scale). 
# all in R, use dplyr and pipe like this %>% heavily annotate code and provide brief intro and interpretation for each step.

# total hh consumption:
# a. in grams by district
# b. aquatic foods in grams by district
# c. nonaquatic foods in grams by district

# total hh consumption by member: in grams by district / hh_members
# a,b,c 

# total hh consumption by member and by urban1_rural2
# a,b,c

# nutrients consumed by hh member
# total hh consumption by member * iron / zinc / niacin
consumption_nutrients <- consumption_nutrient %>% 
  ## consumption
  mutate(
    consumption_weekly_member_total = (unit_value * volume_weekly_hh_total) / hh_members
  ) %>% 
  ## edible portion
  mutate(edible_portion = bdd / 100) %>% 
  mutate(
    edible_weekly_member_total = consumption_weekly_member_total * edible_portion 
  ) %>% 
  ## nutrient totals
  mutate(
    iron_weekly_member_total = edible_weekly_member_total * (iron / 100) # iron is per 100g
  ) %>% 
  ## standardize 
  mutate(iron_standardized_weekly_member_total = iron_weekly_member_total / DRI$Iron) %>% 
  ## summarize the weekly consumption by aquatic and nonaquatic
  group_by(aqua, hh_id, hh_id_data_link) %>% 
  summarize(iron_standardized_weekly_member_total = sum(iron_standardized_weekly_member_total, na.rm = T), # ratio of total requirement met
            edible_weekly_member_total = sum(edible_weekly_member_total, na.rm = T)) # in grams
# this answers how much of the iron requirements have been met for a household from one food item
# assuming all hh members are women of reproductive age

  
# wrong numbers: unit issue (DRI?), crazy volume, crazy FCT, wrong code


