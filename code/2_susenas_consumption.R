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
library(foreign)

# directories
wk_dir      <- '~/Documents/Github/north_maluku'
susenas_dir <- '~/Desktop/susenas_data'

## food composition tables
nonaquatic_fct <- read.csv(file.path(wk_dir, 'data/indonesia_fct_nonaquatic_food.csv'))
aquatic_fct    <- read.csv(file.path(wk_dir, 'data/indonesia_fct_aquatic_food.csv'))


## read DRI file
DRI_LIST <- read.csv(file.path(wk_dir, 'data/0_DRI_natl_academies.csv'))
DRI <- DRI_LIST[1, 2:20]
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

#Farid adjusted code
# Drop category SUBTOTAL rows. Per the metadata note above, subgroup_code == 0
# marks a category header (PADI-PADIAN, IKAN, etc.). Summing them alongside
# the item rows would double-count every food: 3,873 vs 1,936 kcal/capita/day.
blok_41 <- blok_41 %>% filter(subgroup_code != 0) %>%
filter(food_item_id_urut <= 182)
stopifnot(all(blok_41$subgroup_code != 0))

###################################################################################################################
### add units

#Farid adjusted code: --> use this one
susenas_units_clean <- susenas_units %>%
  mutate(
    unit_lc = tolower(trimws(unit)),           # collapses Ounce vs ounce
    new_unit  = "gram",
    new_value = case_when(
      unit_lc == "gram"  ~ value,
      unit_lc == "kg"    ~ value * 1000,
      unit_lc == "ounce" ~ value * 100,        # Indonesian ons = 100 g
      unit_lc == "ml"    ~ value * 1,          # assumes water
      unit_lc == "liter" ~ value * 1000,       # assumes water
      unit_lc == "galon" ~ value * 19000,      # Indonesian water gallon = 19 L
      TRUE               ~ NA_real_
    )
  ) %>%
  filter(unit != "batang") %>%                 # `!` outside the equality, safer
  filter(!is.na(unit))

###################################################################################################################
### data join

# Farid adjusted code
consumption_nutrient <- blok_41 %>%
  left_join(nonaquatic_fct %>%
              select(susenas_code, food_group, water:bdd) %>%
              mutate(aqua = "nonaquatic"),
            by = c("food_item_id_urut" = "susenas_code")) %>%
  left_join(aquatic_fct %>%
              select(susenas_code, food_group, vitamin_a:dha_epa) %>%
              mutate(aqua = "aquatic"),
            by = c("food_item_id_urut" = "susenas_code")) %>%
  left_join(susenas_units_clean %>%
              select(susenas_code, new_unit, new_value) %>%
              rename(unit = new_unit, unit_value = new_value),
            by = c("food_item_id_urut" = "susenas_code"))

#nonaquatic_fct add column called dha_epa all values = zero
#aquatic_fct and nonaquatic_fct have the same nutrients
#delete all nutrients from AFCD that are not in nonaquatic
#nonaquatic_fct delete columns source and source_code

#blok41 left_join nonaquatic_fct --> consumption_nonaquatic
#consumption_nonaquatic --> consumption_aquatic

consumption_nonaquatic <- blok_41 %>%
  left_join(nonaquatic_fct %>%
              select(susenas_code, food_group, water:bdd) %>%
              mutate(aqua = "nonaquatic"),
            by = c("food_item_id_urut" = "susenas_code"))

consumption_aquatic <- consumption_nonaquatic %>%
  drop_na(water) %>%
  select(-c(water:aqua)) %>%   #Farid: all the columns from nonaquatic_fct must be removed here
  left_join(aquatic_fct %>%
              mutate(aqua = "aquatic"),
            by = c("food_item_id_urut" = "susenas_code"))

consumption_nutrient <- consumption_nonaquatic %>%
  drop_na(water) %>%
  rbind(consumption_aquatic) %>%
  left_join(susenas_units_clean %>%
              select(susenas_code, new_unit, new_value) %>%
              rename(unit = new_unit, unit_value = new_value), #check if culumn names if they are the same
            by = c("food_item_id_urut" = "susenas_code"))

#make sure all blok 41 and all nutrient from TKPI (+dha_epa) are there
#20-30 sample size (QC)

## example
consumption_beef <- consumption_nutrient %>% 
  filter(susenas_code == 53) %>% # daging sapi
  mutate(
    consumption_weekly_hh_purchased = unit_value * volume_weekly_hh_purchased, 
    consumption_weekly_hh_subsistence = unit_value * volume_weekly_hh_subsistence, 
    consumption_total = unit_value * volume_weekly_hh_total
    )

# Farid adjusted code
consumption_beef <- consumption_nutrient %>%
  filter(food_item_id_urut == 53) %>%    # daging sapi
  mutate(
    consumption_weekly_hh_purchased   = unit_value * volume_weekly_hh_purchased,
    consumption_weekly_hh_subsistence = unit_value * volume_weekly_hh_subsistence,
    consumption_total                 = unit_value * volume_weekly_hh_total
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

library(readr)
library(dplyr)

DIR <- "/Users/faridannam/Documents/GitHub/north_maluku"

consumption_nutrient <- read_csv(file.path(DIR, "consumption_nutrient.csv"))

# grams consumed = amount recorded x grams per recorded unit.
# Five residual items have no unit conversion and drop out via na.rm.
household <- consumption_nutrient %>%
  mutate(grams_weekly_hh = unit_value * volume_weekly_hh_total) %>%
  group_by(hh_id, district) %>%
  summarise(
    total      = sum(grams_weekly_hh, na.rm = TRUE),
    aquatic    = sum(grams_weekly_hh[aqua == "aquatic"],    na.rm = TRUE),
    nonaquatic = sum(grams_weekly_hh[aqua == "nonaquatic"], na.rm = TRUE),
    .groups = "drop"
  )

# average household in each district, grams per week
consumption_district <- household %>%
  group_by(district) %>%
  summarise(
    n_households = n(),
    across(c(total, aquatic, nonaquatic), mean),
    .groups = "drop"
  ) %>%
  mutate(pct_aquatic = 100 * aquatic / total)

# total hh consumption by member: in grams by district / hh_members
# a,b,c 

household <- consumption_nutrient %>%
  mutate(grams_weekly_member = unit_value * volume_weekly_hh_total / hh_members) %>%
  group_by(hh_id, district) %>%
  summarise(
    total      = sum(grams_weekly_member, na.rm = TRUE),
    aquatic    = sum(grams_weekly_member[aqua == "aquatic"],    na.rm = TRUE),
    nonaquatic = sum(grams_weekly_member[aqua == "nonaquatic"], na.rm = TRUE),
    .groups = "drop"
  )

consumption_member <- household %>%
  group_by(district) %>%
  summarise(
    n_households = n(),
    across(c(total, aquatic, nonaquatic), mean),
    .groups = "drop"
  ) %>%
  mutate(pct_aquatic = 100 * aquatic / total)

write_csv(consumption_member, file.path(DIR, "consumption_member.csv"))

# total hh consumption by member and by urban1_rural2
# a,b,c

household <- consumption_nutrient %>%
  mutate(grams_weekly_member = unit_value * volume_weekly_hh_total / hh_members) %>%
  group_by(hh_id, district, urban1_rural2) %>%
  summarise(
    total      = sum(grams_weekly_member, na.rm = TRUE),
    aquatic    = sum(grams_weekly_member[aqua == "aquatic"],    na.rm = TRUE),
    nonaquatic = sum(grams_weekly_member[aqua == "nonaquatic"], na.rm = TRUE),
    .groups = "drop"
  )

consumption_member_area <- household %>%
  group_by(district, urban1_rural2) %>%
  summarise(
    n_households = n(),
    across(c(total, aquatic, nonaquatic), mean),
    .groups = "drop"
  ) %>%
  mutate(
    area        = if_else(urban1_rural2 == 1, "urban", "rural"),
    pct_aquatic = 100 * aquatic / total
  )

write_csv(consumption_member_area, file.path(DIR, "consumption_member_area.csv"))

# nutrients consumed by hh member
# total hh consumption by member * iron / zinc / niacin

DIR <- "/Users/faridannam/Documents/GitHub/north_maluku"

consumption_nutrient <- read_csv(file.path(DIR, "consumption_nutrient.csv"))

# DRI reference values, females row
DRI <- read_csv(file.path(DIR, "0_DRI_natl_academies.csv")) %>%
  filter(sex_cat == "females")

consumption_nutrients <- consumption_nutrient %>%
  ## consumption
  mutate(
    consumption_weekly_member_total = (unit_value * volume_weekly_hh_total) / hh_members
  ) %>%
  ## edible portion
  # bdd is missing for all aquatic items (the fish source has no refuse data)
  # and for three beverages. Treated as 100% edible for now - without this,
  # every fish would silently drop out of the nutrient totals.
  mutate(edible_portion = if_else(is.na(bdd), 1, bdd / 100)) %>%
  mutate(
    edible_weekly_member_total = consumption_weekly_member_total * edible_portion
  ) %>%
  ## nutrient totals (values are per 100 g edible portion)
  mutate(
    iron_weekly_member_total   = edible_weekly_member_total * (iron   / 100),
    zinc_weekly_member_total   = edible_weekly_member_total * (zinc   / 100),
    niacin_weekly_member_total = edible_weekly_member_total * (niacin / 100)
  ) %>%
  ## standardize
  # intake is weekly but the DRI is per day, so divide by 7 first;
  # 1 = the daily requirement is met on average
  mutate(
    iron_standardized_weekly_member_total   = (iron_weekly_member_total   / 7) / DRI$Iron,
    zinc_standardized_weekly_member_total   = (zinc_weekly_member_total   / 7) / DRI$Zinc,
    niacin_standardized_weekly_member_total = (niacin_weekly_member_total / 7) / DRI$Niacin
  ) %>%
  ## summarize the weekly consumption by aquatic and nonaquatic
  # (11 items without composition values contribute grams but no nutrients;
  #  na.rm skips them until their nutrient values are obtained)
  group_by(aqua, hh_id, hh_id_data_link) %>%
  summarize(
    iron_standardized_weekly_member_total   = sum(iron_standardized_weekly_member_total,   na.rm = TRUE),
    zinc_standardized_weekly_member_total   = sum(zinc_standardized_weekly_member_total,   na.rm = TRUE),
    niacin_standardized_weekly_member_total = sum(niacin_standardized_weekly_member_total, na.rm = TRUE),
    edible_weekly_member_total              = sum(edible_weekly_member_total,              na.rm = TRUE),
    .groups = "drop"
  )
# each household now has two rows: what its fish supplied, and everything else.
# A household that bought no fish this week simply has no aquatic row.

write_csv(consumption_member_iron_niacin_zinc,
          file.path(DIR, "consumption_member_iron_niacin_zinc.csv"))

########################################################################################################

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

# Hunting for wrong numbers. Four suspects:
#   1. unit issues in the DRI standardization  - all 18 assessable nutrients
#   2. implausible volume  - survey entry errors
#   3. implausible FCT values - composition table errors
#   4. food code misclassification - an item matched to the wrong food's values
# Nothing is changed here; this script only points at records and items
# worth a closer look. A flag is a suspicion, not a verdict.

library(readr)
library(dplyr)
library(tidyr)

DIR <- "/Users/faridannam/Documents/GitHub/north_maluku"

consumption_nutrient <- read_csv(file.path(DIR, "consumption_nutrient.csv"))

DRI <- read_csv(file.path(DIR, "0_DRI_natl_academies.csv")) %>%
  filter(sex_cat == "females")

# the 41 nutrient columns (per 100 g edible portion)
nutrient_cols <- c(
  "water","energy","fat","carbohydrate","fiber","ash",
  "retinol","beta_carotene","total_carotene",
  "protein","calcium","phosphorus","iron","sodium","potassium",
  "copper","zinc","thiamin","riboflavin","niacin","vitamin_c",
  "vitamin_a","vitamin_b6","folate","vitamin_b12","vitamin_d","vitamin_e",
  "chromium","iodine","magnesium","manganese","selenium",
  "leucine","lysine","methionine","phenylalanine","threonine",
  "tryptophan","valine","ala","dha_epa"
)

## 1. unit check on the DRI standardization -----------------------------------
# All 18 nutrients that exist in both our table and the DRI file (molybdenum
# is in the DRI file but in no composition source, so it cannot be checked).
# If a unit slipped anywhere, the average household lands wildly off the
# requirement - 10x or 1000x. Two things are NOT alarms:
#   * dha_epa is grams in our table and mg in the DRI - the x1000 below is
#     the correct conversion, not an error;
#   * eight nutrients exist only for aquatic foods (selenium, iodine,
#     magnesium, manganese, chromium, vitamin_b6, vitamin_b12, dha_epa), so
#     a LOW percentage there is the fish contribution, not a unit problem.
dri_nutrients <- c(                      # our column = DRI file column
  protein = "Protein",   calcium   = "Calcium",   phosphorus = "Phosphorus",
  iron    = "Iron",      potassium = "Potassium", copper     = "Copper",
  zinc    = "Zinc",      thiamin   = "Thiamin",   riboflavin = "Riboflavin",
  niacin  = "Niacin",    magnesium = "Magnesium", selenium   = "Selenium",
  iodine  = "Iodine",    chromium  = "Chromium",  manganese  = "Manganese",
  vitamin_b6 = "VitaminB6", vitamin_b12 = "VitaminB12", dha_epa = "DHAEPA"
)
aquatic_only <- c("selenium","iodine","magnesium","manganese","chromium",
                  "vitamin_b6","vitamin_b12","dha_epa")

intake <- consumption_nutrient %>%
  mutate(
    edible = (unit_value * volume_weekly_hh_total / hh_members) *
      if_else(is.na(bdd), 1, bdd / 100)
  ) %>%
  group_by(hh_id) %>%
  summarise(across(all_of(names(dri_nutrients)),
                   ~ sum(edible * .x / 100, na.rm = TRUE) / 7),
            .groups = "drop")

unit_check <- tibble(
  nutrient   = names(dri_nutrients),
  mean_daily = sapply(names(dri_nutrients), function(n) mean(intake[[n]])),
  dri        = as.numeric(DRI[1, unname(dri_nutrients)])
) %>%
  mutate(
    # the one real unit gap: grams in our table, mg in the DRI file
    mean_daily = if_else(nutrient == "dha_epa", mean_daily * 1000, mean_daily),
    pct_of_dri = round(100 * mean_daily / dri, 1),
    coverage   = if_else(nutrient %in% aquatic_only,
                         "aquatic foods only", "whole diet"),
    verdict = case_when(
      nutrient == "copper"              ~ "known problem - TKPI mixes mg and mcg",
      pct_of_dri > 300                  ~ "CHECK - implausibly high",
      pct_of_dri < 1                    ~ "CHECK - implausibly low",
      TRUE                              ~ "ok"
    )
  ) %>%
  arrange(desc(pct_of_dri))

cat("1. average household as % of the daily requirement, all 18 nutrients\n",
    "   (for 'aquatic foods only' rows the figure is the fish contribution):\n")
print(unit_check, n = 18)

## 2. implausible volume --------------------------------------------------------
# Compare every record to the typical (median) amount of ITS OWN item -
# 5 kg/day of drinking water is normal, 5 kg/day of salt is not, so items
# are judged against themselves, not against each other.
# Threshold: 10x the item median, decided by Farid (17 Aug 2026) after
# reviewing the flagged records. Note many flags are daily-routine buyers of
# occasionally-purchased foods (weekly volumes in multiples of 7); judge
# flagged records by the absolute g/day, not the ratio alone.
volumes <- consumption_nutrient %>%
  mutate(g_member_day = unit_value * volume_weekly_hh_total / hh_members / 7) %>%
  filter(!is.na(g_member_day), g_member_day > 0) %>%
  group_by(food_item_id_urut) %>%
  mutate(item_median = median(g_member_day)) %>%
  ungroup() %>%
  mutate(times_median = g_member_day / item_median)

implausible_volume <- volumes %>%
  group_by(food_item_id_urut) %>%
  summarise(
    n_records              = n(),
    median_g_day           = median(g_member_day),
    max_g_day              = max(g_member_day),
    n_implausible_volume   = sum(times_median > 10),
    .groups = "drop"
  )

cat("\n2. implausible volumes: records above 10x their item's median:\n")
print(implausible_volume %>% filter(n_implausible_volume > 0) %>%
        arrange(desc(n_implausible_volume)) %>% head(10))

## 3. implausible FCT values -----------------------------------------------------
# One row per food item; a value is suspicious if it is physically impossible
# (more than 100 g of a component per 100 g of food) or more than 10x the
# median of all items that carry that nutrient.
fct <- consumption_nutrient %>%
  select(food_item_id_urut, all_of(nutrient_cols)) %>%
  distinct() %>%
  pivot_longer(-food_item_id_urut, names_to = "nutrient", values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(nutrient) %>%
  mutate(median_all_items = median(value)) %>%
  ungroup()

gram_nutrients <- c("water","protein","fat","carbohydrate","fiber","ash",
                    "ala","dha_epa")

implausible_fct <- fct %>%
  filter(
    (nutrient %in% gram_nutrients & value > 100) |          # impossible
      (median_all_items > 0 & value > 10 * median_all_items)  # extreme
  ) %>%
  arrange(desc(value / pmax(median_all_items, 1e-9)))

cat("\n3. implausible composition values (worst first):\n")
print(head(implausible_fct, 10))

## 4. food code misclassification -------------------------------------------------
# The survey ships its own weekly protein and calorie totals per record,
# computed from BPS's own food-code matching. Recomputing them from our
# values, the ratio survey/ours should sit near 1 for every item. A median
# ratio far from 1 points at a misclassified food code - or a wrong unit,
# or a genuinely different composition source; the flag says where to look,
# the linking file says which it is.
misclassification <- consumption_nutrient %>%
  mutate(
    edible_hh    = unit_value * volume_weekly_hh_total *
      if_else(is.na(bdd), 1, bdd / 100),
    ours_protein = protein / 100 * edible_hh,
    ours_energy  = energy  / 100 * edible_hh,
    r_protein    = if_else(ours_protein > 0,
                           protein_weekly_hh_total  / ours_protein, NA),
    r_energy     = if_else(ours_energy > 0,
                           kalories_weekly_hh_total / ours_energy,  NA)
  ) %>%
  group_by(food_item_id_urut) %>%
  summarise(
    median_r_protein = median(r_protein, na.rm = TRUE),
    median_r_energy  = median(r_energy,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(suspect_misclassification =
           (median_r_protein < 0.5 | median_r_protein > 2 |
              median_r_energy  < 0.5 | median_r_energy  > 2) %in% TRUE)

cat("\n4. suspected food code misclassification (ratio far from 1):\n")
print(misclassification %>% filter(suspect_misclassification) %>%
        arrange(desc(pmax(abs(log(median_r_protein)),
                          abs(log(median_r_energy)), na.rm = TRUE))) %>%
        head(15))

## one summary table: one row per item, checks 2-4 side by side -----------------
wrong_numbers <- implausible_volume %>%
  full_join(misclassification, by = "food_item_id_urut") %>%
  left_join(implausible_fct %>% count(food_item_id_urut,
                                      name = "n_implausible_fct"),
            by = "food_item_id_urut") %>%
  mutate(n_implausible_fct = coalesce(n_implausible_fct, 0L)) %>%
  arrange(desc(suspect_misclassification), desc(n_implausible_fct),
          desc(n_implausible_volume))

write_csv(wrong_numbers, file.path(DIR, "wrong_numbers.csv"))
