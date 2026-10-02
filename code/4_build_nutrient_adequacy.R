# This step builds nutrient_adequacy.csv: one row per household member of the
# SUSENAS March 2020 sample (22,488 persons), with the person's daily intake of
# 11 vitamins and minerals next to the Dietary Reference Intake (DRI) for the
# person's own sex, age and pregnancy group. Adequacy is read by comparing the
# two columns of a nutrient; no ratio or flag is stored.
# Run order: 1_build_indonesia_fct_aquatic.R and 1_build_indonesia_fct_nonaquatic.R,
# then 2_build_indonesia_fct_complete.R, then 3_build_susenas_consumption.R,
# then this script. It leaves one object in the environment: nutrient_adequacy.
#
# Name: Farid Annam
# Affiliation: Harvard T.H. Chan School of Public Health
# For: north_maluku
# Date updated: 10/2/2026
###################################################################################################################
# Inputs (all read-only):
#   2_process/blok41_fct_joined.csv                 food records with the FCT nutrients per 100 g edible portion
#   susenas_data/bv2_..._kor20ind_1.dbf             persons: member number, sex, birth date, age
#   susenas_data/bv2_..._kor20ind_2.dbf             persons: currently pregnant (R1606)
#   susenas_data/bv2_..._kor20rt.dbf                households: district, area type, members, weight
#   2_process/natl_academies_DRIs_vitamins_age_sex.csv, natl_academies_DRIs_minerals_age_sex.csv
#                                                   RDAs and AIs by life-stage group (National Academies)
#
# Method:
#  - Intake. Per food record: edible grams = quantity x grams per unit x bdd_pct / 100;
#    nutrient = edible grams x value per 100 g / 100. Summed per household, divided
#    by 7 and by the number of members: an equal per-capita share. SUSENAS records
#    food for the household, not for persons, so every member of a household
#    carries the same intake; this overstates children and understates adults.
#  - Requirement. The DRI row is chosen by sex (R405) and age in completed years
#    (R407): children 1-3 and 4-8, then males or females 9-13, 14-18, 19-30,
#    31-50, 51-70 and over 70. A woman who is currently pregnant (R1606 = 1)
#    takes the Pregnancy row of her age band. Infants under one year have no row
#    in these tables and keep blank requirements.
#  - Nutrients: the FCT panel and the DRI tables share calcium, copper, iron,
#    phosphorus, zinc, thiamin, riboflavin, niacin and vitamin C (RDAs) and
#    potassium and sodium (AIs). Copper is converted from mg to mcg to match
#    the DRI. FCT niacin is preformed niacin while the DRI is in niacin
#    equivalents, so the niacin comparison is a lower bound.
#  - Column names carry the unit: nutrient_unit_day for the intake,
#    nutrient_unit_rda or nutrient_unit_ai for the requirement.
# The script stops on any unexpected condition instead of passing it through.

# clear workspace
rm(list = ls())
graphics.off()

# libraries
library(tidyverse)
library(foreign)

# directories
susenas_dir <- "~/Desktop/susenas_data"
proc_dir    <- "~/Desktop/indonesia_fct/datasets/2_process"

# the 11 nutrients: FCT column (per 100 g), output intake column, DRI column, output
# requirement column, and the factor that brings the FCT unit to the DRI unit
nutr <- tribble(
  ~fct_col,             ~out_day,            ~dri_col,            ~out_req,            ~factor,
  "calcium_mg_100g",    "calcium_mg_day",    "Calcium_mg_RDA",    "calcium_mg_rda",    1,
  "copper_mg_100g",     "copper_mcg_day",    "Copper_mcg_RDA",    "copper_mcg_rda",    1000,
  "iron_mg_100g",       "iron_mg_day",       "Iron_mg_RDA",       "iron_mg_rda",       1,
  "phosphorus_mg_100g", "phosphorus_mg_day", "Phosphorus_mg_RDA", "phosphorus_mg_rda", 1,
  "zinc_mg_100g",       "zinc_mg_day",       "Zinc_mg_RDA",       "zinc_mg_rda",       1,
  "potassium_mg_100g",  "potassium_mg_day",  "Potassium_mg_AI",   "potassium_mg_ai",   1,
  "sodium_mg_100g",     "sodium_mg_day",     "Sodium_mg_AI",      "sodium_mg_ai",      1,
  "thiamin_mg_100g",    "thiamin_mg_day",    "Thiamin_mg_RDA",    "thiamin_mg_rda",    1,
  "riboflavin_mg_100g", "riboflavin_mg_day", "Riboflavin_mg_RDA", "riboflavin_mg_rda", 1,
  "niacin_mg_100g",     "niacin_mg_day",     "Niacin_mg_RDA",     "niacin_mg_rda",     1,
  "vitamin_c_mg_100g",  "vitamin_c_mg_day",  "VitaminC_mg_RDA",   "vitamin_c_mg_rda",  1)

###################################################################################################################
## 1. Household intake per person per day, from the food records

joined <- read_csv(file.path(proc_dir, "blok41_fct_joined.csv"), progress = FALSE,
                   col_types = cols(.default = col_double(), coicop = col_character(),
                                    susenas_item = col_character(), food_group = col_character(),
                                    unit = col_character(), sample_id = col_character(),
                                    hh_id = col_character(), primary_sampling_unit = col_character(),
                                    secondary_sampling_unit = col_character(), strata = col_character(),
                                    hh_id_data_link = col_character()))
stopifnot(nrow(joined) == 146636, all(nutr$fct_col %in% names(joined)),
          !any(is.na(joined$grams_per_unit)), !any(is.na(joined$bdd_pct)),
          !any(is.na(joined[nutr$fct_col])))

hh_intake <- joined %>%
  mutate(edible_g = volume_weekly_hh_total * grams_per_unit * bdd_pct / 100) %>%
  group_by(renum = hh_id_data_link, hh_members) %>%
  summarise(across(all_of(nutr$fct_col), ~ sum(edible_g * .x / 100)), .groups = "drop") %>%
  mutate(across(all_of(nutr$fct_col), ~ .x / 7 / hh_members))          # weekly household -> per person per day
stopifnot(nrow(hh_intake) == 5280, !any(duplicated(hh_intake$renum)))
for (i in seq_len(nrow(nutr)))
  hh_intake[[nutr$out_day[i]]] <- hh_intake[[nutr$fct_col[i]]] * nutr$factor[i]
hh_intake <- hh_intake %>% select(renum, hh_members, all_of(nutr$out_day))
cat("1. Intake: 146,636 food records ->", nrow(hh_intake), "households, per person per day\n")

###################################################################################################################
## 2. Persons: sex, birth date, age, pregnancy, and their household's district, area, size and weight

ind1 <- read.dbf(file.path(susenas_dir, "bv2_2026_09_24_11_23_04_kor20ind_1.dbf"), as.is = TRUE)
ind2 <- read.dbf(file.path(susenas_dir, "bv2_2026_09_24_11_24_01_kor20ind_2.dbf"), as.is = TRUE)
rt   <- read.dbf(file.path(susenas_dir, "bv2_2026_09_24_11_25_02_kor20rt.dbf"),    as.is = TRUE)

persons <- ind1 %>%
  transmute(renum = as.character(RENUM), member_no = R401,
            sex = case_when(R405 == 1 ~ "male", R405 == 2 ~ "female"),
            birth_day = R406A, birth_month = R406B, birth_year = R406C, age_years = R407) %>%
  left_join(ind2 %>% transmute(renum = as.character(RENUM), member_no = R401,
                               pregnant = case_when(R1606 == 1 ~ TRUE, R1606 == 5 ~ FALSE, TRUE ~ NA)),
            by = c("renum", "member_no")) %>%
  left_join(rt %>% transmute(renum = as.character(RENUM), district = R102, urban_rural = R105,
                             hh_members = R301, weight_hh = FWT),
            by = "renum")
stopifnot(nrow(persons) == 22488, !any(duplicated(persons[c("renum", "member_no")])),
          !any(is.na(persons$sex)), !any(is.na(persons$age_years)), !any(is.na(persons$weight_hh)),
          all(persons$renum %in% hh_intake$renum))
# the household size behind the food records equals the household size of the core module
stopifnot(all(hh_intake$hh_members[match(persons$renum, hh_intake$renum)] == persons$hh_members))
cat("2. Persons:", nrow(persons), "in", n_distinct(persons$renum), "households;",
    sum(persons$pregnant %in% TRUE), "currently pregnant\n")

###################################################################################################################
## 3. Life-stage group of every person (the DRI row to use)

age_band <- function(age) case_when(age < 1 ~ NA_character_,
                                    age <= 3 ~ "1-3 y",   age <= 8  ~ "4-8 y",   age <= 13 ~ "9-13 y",
                                    age <= 18 ~ "14-18 y", age <= 30 ~ "19-30 y", age <= 50 ~ "31-50 y",
                                    age <= 70 ~ "51-70 y", TRUE ~ "> 70 y")
persons <- persons %>%
  mutate(dri_age   = age_band(age_years),
         dri_group = case_when(is.na(dri_age) ~ NA_character_,
                               age_years <= 8 ~ "children",
                               pregnant %in% TRUE ~ "pregnancy",
                               sex == "male" ~ "males",
                               TRUE ~ "females"),
         life_stage_group = ifelse(is.na(dri_group), NA_character_, paste(str_to_title(dri_group), dri_age)))
# the Pregnancy rows exist for 14-18, 19-30 and 31-50 only
stopifnot(all(persons$age_years[persons$pregnant %in% TRUE] >= 14 & persons$age_years[persons$pregnant %in% TRUE] <= 50))
cat("3. Life-stage groups:", n_distinct(na.omit(persons$life_stage_group)), "groups;",
    sum(is.na(persons$life_stage_group)), "infants under one year without a DRI row\n")

###################################################################################################################
## 4. Requirements: the two DRI files on one key (group, age band)

read_dri <- function(file) {
  read_csv(file.path(proc_dir, file), show_col_types = FALSE, name_repair = "unique_quiet") %>%
    select(-any_of("...1")) %>%                                        # row numbers in the minerals file
    rename_with(~ str_replace(.x, "μg", "mcg")) %>%                 # Greek mu -> mcg
    mutate(dri_group = tolower(sex_cat), dri_age = str_replace_all(age, "–", "-")) %>%   # en dash -> hyphen
    select(dri_group, dri_age, everything(), -LifeStageGroup, -sex_cat, -age)
}
dri <- full_join(read_dri("natl_academies_DRIs_minerals_age_sex.csv"),
                 read_dri("natl_academies_DRIs_vitamins_age_sex.csv"), by = c("dri_group", "dri_age"))
stopifnot(nrow(dri) == 20, !any(duplicated(dri[c("dri_group", "dri_age")])), all(nutr$dri_col %in% names(dri)),
          all(unique(paste(persons$dri_group, persons$dri_age)[!is.na(persons$dri_group)]) %in% paste(dri$dri_group, dri$dri_age)))
requirements <- dri %>% select(dri_group, dri_age, all_of(nutr$dri_col))
names(requirements)[match(nutr$dri_col, names(requirements))] <- nutr$out_req
cat("4. Requirements: 20 life-stage rows,", nrow(nutr), "nutrients shared by the FCT panel and the DRI tables\n")

###################################################################################################################
## 5. Assemble, check, write

nutrient_adequacy <- persons %>%
  left_join(hh_intake %>% select(-hh_members), by = "renum") %>%
  left_join(requirements, by = c("dri_group", "dri_age")) %>%
  mutate(across(all_of(nutr$out_day), ~ round(.x, 2))) %>%
  select(renum, member_no, district, urban_rural, hh_members, weight_hh,
         sex, birth_day, birth_month, birth_year, age_years, pregnant, life_stage_group,
         all_of(as.vector(rbind(nutr$out_day, nutr$out_req))))      # intake next to its requirement

no_row <- is.na(nutrient_adequacy$life_stage_group)
stopifnot(nrow(nutrient_adequacy) == 22488, ncol(nutrient_adequacy) == 13 + 2 * nrow(nutr),
          !any(is.na(nutrient_adequacy[nutr$out_day])),                 # every person has an intake
          !any(is.na(nutrient_adequacy[!no_row, nutr$out_req])),        # every person aged 1+ has requirements
          all(is.na(nutrient_adequacy[no_row, nutr$out_req])),          # infants have none
          isTRUE(all.equal(sum(nutrient_adequacy$weight_hh), 1272980, tolerance = 1e-6)))   # weighted persons

write_csv(nutrient_adequacy, file.path(proc_dir, "nutrient_adequacy.csv"), na = "")
cat("5. Wrote nutrient_adequacy.csv (", nrow(nutrient_adequacy), " x ", ncol(nutrient_adequacy), ")\n", sep = "")

# a glance at the result (not stored): weighted share of persons aged 1+ whose intake reaches the requirement
glance <- map_dfr(seq_len(nrow(nutr)), function(i) {
  d <- nutrient_adequacy[!no_row, ]
  tibble(nutrient = nutr$out_day[i], median_intake = median(d[[nutr$out_day[i]]]),
         pct_reaching = round(100 * sum(d$weight_hh * (d[[nutr$out_day[i]]] >= d[[nutr$out_req[i]]])) / sum(d$weight_hh), 1))
})
print(as.data.frame(glance), row.names = FALSE)

rm(list = setdiff(ls(), "nutrient_adequacy"))
