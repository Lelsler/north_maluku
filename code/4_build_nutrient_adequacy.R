# This step builds nutrient_adequacy.csv: one row per household member of the
# SUSENAS March 2020 sample (22,488 persons), with the person's daily intake of
# 13 nutrients next to the reference intake for the person's own sex, age and
# pregnancy group. Adequacy is read by comparing the two columns of a nutrient;
# no ratio or flag is stored.
# Run order: 1_build_indonesia_fct_aquatic.R and 1_build_indonesia_fct_nonaquatic.R,
# then 2_build_indonesia_fct_complete.R, then 3_build_susenas_consumption.R,
# then this script.
#
# Name: Farid Annam
# Affiliation: Harvard T.H. Chan School of Public Health
# For: north_maluku
# Date updated: 12/8/2026
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
#  - Vitamin A is expressed as retinol activity equivalents (RAE), the unit of its
#    DRI: mcg RAE = retinol mcg + beta-carotene mcg / 12 (Food and Nutrition Board,
#    2001).
#  - Requirement. The DRI row is chosen by sex (R405) and age in completed years
#    (R407): children 1-3 and 4-8, then males or females 9-13, 14-18, 19-30,
#    31-50, 51-70 and over 70. A woman who is currently pregnant (R1606 = 1)
#    takes the Pregnancy row of her age band. Infants under one year have no row
#    in these tables and keep blank requirements.
#  - Pregnancy. R1606 is asked of ever-married women aged 10-54 only (questionnaire
#    VSEN20.K, block XVI); everyone else holds code 0 (not asked). pregnant is
#    TRUE for code 1 and FALSE for everyone else, so never-married women count as
#    not pregnant, as the survey design assumes.
#  - Nutrients: the FCT panel and the DRI tables share calcium, copper, iron,
#    phosphorus, zinc, thiamin, riboflavin, niacin, vitamin C and vitamin A (RDAs)
#    and potassium and sodium (AIs). Copper is converted from mg to mcg to match
#    the DRI. FCT niacin is preformed niacin while the DRI is in niacin
#    equivalents, so the niacin comparison is a lower bound.
#  - DHA+EPA has no DRI from the National Academies. The reference takes two
#    values only: 250 mg per day for every person who is not pregnant, infants
#    included, and 300 mg per day in pregnancy. 250 mg is the adult value of
#    FAO/WHO (2010, Fats and fatty acids in human nutrition, FAO Food and Nutrition
#    Paper 91) and the adequate intake of EFSA (2010, EFSA Journal 8(3):1461) from
#    age two; 300 mg (of which 200 mg DHA) is the FAO/WHO 2010 value for pregnancy.
#    Neither body gives an EPA+DHA value under two years; the adult value is
#    applied to every age. The FCT grams are converted to mg. Several fish-book
#    rows carry DHA+EPA above their total fat; the values are used as published.
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

# the 13 nutrients in A-Z order (the order of the file): FCT column (per 100 g), output intake column,
# reference column, output requirement column, and the factor that brings the FCT unit to the reference unit
nutr <- tribble(
  ~fct_col,             ~out_day,            ~dri_col,            ~out_req,            ~factor,
  "calcium_mg_100g",    "calcium_mg_day",    "Calcium_mg_RDA",    "calcium_mg_rda",    1,
  "copper_mg_100g",     "copper_mcg_day",    "Copper_mcg_RDA",    "copper_mcg_rda",    1000,
  "dha_epa_g_100g",     "dhaepa_mg_day",     "DHAEPA_mg_AI",      "dhaepa_mg_ai",      1000,
  "iron_mg_100g",       "iron_mg_day",       "Iron_mg_RDA",       "iron_mg_rda",       1,
  "niacin_mg_100g",     "niacin_mg_day",     "Niacin_mg_RDA",     "niacin_mg_rda",     1,
  "phosphorus_mg_100g", "phosphorus_mg_day", "Phosphorus_mg_RDA", "phosphorus_mg_rda", 1,
  "potassium_mg_100g",  "potassium_mg_day",  "Potassium_mg_AI",   "potassium_mg_ai",   1,
  "riboflavin_mg_100g", "riboflavin_mg_day", "Riboflavin_mg_RDA", "riboflavin_mg_rda", 1,
  "sodium_mg_100g",     "sodium_mg_day",     "Sodium_mg_AI",      "sodium_mg_ai",      1,
  "thiamin_mg_100g",    "thiamin_mg_day",    "Thiamin_mg_RDA",    "thiamin_mg_rda",    1,
  "vitamina_mcg_100g",  "vitamina_mcg_day",  "VitaminA_mcg_RDA",  "vitamina_mcg_rda",  1,
  "vitamin_c_mg_100g",  "vitaminc_mg_day",   "VitaminC_mg_RDA",   "vitaminc_mg_rda",   1,
  "zinc_mg_100g",       "zinc_mg_day",       "Zinc_mg_RDA",       "zinc_mg_rda",       1)
stopifnot(!is.unsorted(nutr$out_day))                                   # the list is A-Z

###################################################################################################################
## 1. Household intake per person per day, from the food records

joined <- read_csv(file.path(proc_dir, "blok41_fct_joined.csv"), progress = FALSE,
                   col_types = cols(.default = col_double(), coicop = col_character(),
                                    susenas_item = col_character(), food_group = col_character(),
                                    unit = col_character(), sample_id = col_character(),
                                    hh_id = col_character(), primary_sampling_unit = col_character(),
                                    secondary_sampling_unit = col_character(), strata = col_character(),
                                    hh_id_data_link = col_character())) %>%
  mutate(vitamina_mcg_100g = retinol_mcg_100g + beta_carotene_mcg_100g / 12)   # retinol activity equivalents
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
stopifnot(all(ind2$R1606 %in% c(0, 1, 5)))                              # 0 not asked, 1 yes, 5 no

persons <- ind1 %>%
  transmute(renum = as.character(RENUM), member_no = R401,
            sex = case_when(R405 == 1 ~ "male", R405 == 2 ~ "female"),
            birth_day = R406A, birth_month = R406B, birth_year = R406C, age_years = R407) %>%
  left_join(ind2 %>% transmute(renum = as.character(RENUM), member_no = R401, pregnant = R1606 == 1),
            by = c("renum", "member_no")) %>%
  left_join(rt %>% transmute(renum = as.character(RENUM), district = R102, urban_rural = R105,
                             hh_members = R301, weight_hh = FWT),
            by = "renum")
stopifnot(nrow(persons) == 22488, !any(duplicated(persons[c("renum", "member_no")])),
          !any(is.na(persons$sex)), !any(is.na(persons$age_years)), !any(is.na(persons$pregnant)),
          !any(is.na(persons$weight_hh)), all(persons$renum %in% hh_intake$renum),
          isTRUE(all.equal(sum(persons$weight_hh), 1272980, tolerance = 1e-6)))   # weighted persons = population
# the household size behind the food records equals the household size of the core module
stopifnot(all(hh_intake$hh_members[match(persons$renum, hh_intake$renum)] == persons$hh_members))
cat("2. Persons:", nrow(persons), "in", n_distinct(persons$renum), "households;",
    sum(persons$pregnant), "currently pregnant\n")

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
                               pregnant ~ "pregnancy",
                               sex == "male" ~ "males",
                               TRUE ~ "females"),
         life_stage_group = ifelse(is.na(dri_group), NA_character_, paste(str_to_title(dri_group), dri_age)))
# the Pregnancy rows exist for 14-18, 19-30 and 31-50 only
stopifnot(all(persons$age_years[persons$pregnant] >= 14 & persons$age_years[persons$pregnant] <= 50))
cat("3. Life-stage groups:", n_distinct(na.omit(persons$life_stage_group)), "groups;",
    sum(is.na(persons$life_stage_group)), "infants under one year without a DRI row\n")

###################################################################################################################
## 4. Requirements: the two DRI files on one key (group, age band), plus the DHA+EPA reference

read_dri <- function(file) {
  read_csv(file.path(proc_dir, file), show_col_types = FALSE, name_repair = "unique_quiet") %>%
    select(-any_of("...1")) %>%                                        # row numbers in the minerals file
    rename_with(~ str_replace(.x, "μg", "mcg")) %>%                 # Greek mu -> mcg
    mutate(dri_group = tolower(sex_cat), dri_age = str_replace_all(age, "–", "-")) %>%   # en dash -> hyphen
    select(dri_group, dri_age, everything(), -LifeStageGroup, -sex_cat, -age)
}
dri <- full_join(read_dri("natl_academies_DRIs_minerals_age_sex.csv"),
                 read_dri("natl_academies_DRIs_vitamins_age_sex.csv"), by = c("dri_group", "dri_age")) %>%
  mutate(DHAEPA_mg_AI = ifelse(dri_group == "pregnancy", 300, 250))    # FAO/WHO 2010 and EFSA 2010, see header
stopifnot(nrow(dri) == 20, !any(duplicated(dri[c("dri_group", "dri_age")])), all(nutr$dri_col %in% names(dri)),
          all(unique(paste(persons$dri_group, persons$dri_age)[!is.na(persons$dri_group)]) %in% paste(dri$dri_group, dri$dri_age)))
requirements <- dri %>% select(dri_group, dri_age, all_of(nutr$dri_col))
names(requirements)[match(nutr$dri_col, names(requirements))] <- nutr$out_req
cat("4. Requirements: 20 life-stage rows,", nrow(nutr), "nutrients\n")

###################################################################################################################
## 5. Assemble, check, write

nutrient_adequacy <- persons %>%
  left_join(hh_intake %>% select(-hh_members), by = "renum") %>%
  left_join(requirements, by = c("dri_group", "dri_age")) %>%
  mutate(across(all_of(nutr$out_day), ~ round(.x, 2)),
         dhaepa_mg_ai = ifelse(pregnant, 300, 250)) %>%          # the DHA+EPA rule applies to every person, infants included
  select(renum, member_no, district, urban_rural, hh_members,
         sex, birth_day, birth_month, birth_year, age_years, pregnant, life_stage_group,
         all_of(as.vector(rbind(nutr$out_day, nutr$out_req))))      # A-Z pairs, intake next to its requirement

no_row   <- is.na(nutrient_adequacy$life_stage_group)
dri_cols <- setdiff(nutr$out_req, "dhaepa_mg_ai")                       # the 12 requirements that come from the DRI tables
stopifnot(nrow(nutrient_adequacy) == 22488, ncol(nutrient_adequacy) == 12 + 2 * nrow(nutr),
          !any(is.na(nutrient_adequacy[nutr$out_day])),                 # every person has an intake
          !any(is.na(nutrient_adequacy[!no_row, dri_cols])),            # every person aged 1+ has the DRI requirements
          all(is.na(nutrient_adequacy[no_row, dri_cols])),              # infants have none of them
          all(nutrient_adequacy$dhaepa_mg_ai == ifelse(nutrient_adequacy$pregnant, 300, 250)))   # DHA+EPA: two values, no blanks

write_csv(nutrient_adequacy, file.path(proc_dir, "nutrient_adequacy.csv"), na = "")
cat("5. Wrote nutrient_adequacy.csv (", nrow(nutrient_adequacy), " x ", ncol(nutrient_adequacy), ")\n", sep = "")

# a glance at the result (not stored): weighted share of persons aged 1+ whose intake reaches the requirement;
# the household weight (FWT) is taken from the persons table, it is not a column of the file
glance_data <- nutrient_adequacy[!no_row, ] %>%
  left_join(persons %>% select(renum, member_no, weight_hh), by = c("renum", "member_no"))
glance <- map_dfr(seq_len(nrow(nutr)), function(i) {
  d <- glance_data
  tibble(nutrient = nutr$out_day[i], median_intake = median(d[[nutr$out_day[i]]]),
         pct_reaching = round(100 * sum(d$weight_hh * (d[[nutr$out_day[i]]] >= d[[nutr$out_req[i]]])) / sum(d$weight_hh), 1))
})
print(as.data.frame(glance), row.names = FALSE)

rm(list = setdiff(ls(), "nutrient_adequacy"))
