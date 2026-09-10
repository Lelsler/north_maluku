# Build the SUSENAS consumption pipeline, from the raw BPS file
# to the analysis tables:
#
#   step 1 - read blok 41 from the original DBF (North Maluku, province 82),
#            rename columns to readable names, and remove the category
#            subtotal rows (subgroup_code == 0) before anything else
#   step 2 - remove rokok (items 183-188), join the food composition
#            table (the fish book's 21 nutrient columns, energy through
#            bdd; per 100 g edible portion), and number the records (id)
#            (the pure join of blok 41 and indonesia_fct_complete)
#   step 3 - add the unit conversion (grams per recorded unit, from
#            food_composition_units.xlsx) and write blok41_fct_joined.csv,
#            the record-level table every later analysis starts from
#            (build_exploratory_analysis.R computes intake per person,
#            district tables and nutrient adequacy from it)
#
# Inputs:
#   ~/Desktop/susenas_data/blok41_51_94.dbf
#   2_process/food_composition_units.xlsx
#   2_process/indonesia_fct_complete.csv
#
# Conventions used throughout:
#   - category subtotal rows (subgroup_code == 0) are removed in step 1,
#     as they are BPS sums of the item rows and would count every food twice
#   - nutrient values are used as published in indonesia_fct_complete.csv
#   - identifiers are kept as text so they can never be rounded

library(readr)
library(dplyr)
library(readxl)
library(foreign)

susenas_dir  <- "~/Desktop/susenas_data"                       # raw DBF
dir_2process <- "~/Desktop/indonesia_fct/datasets/2_process"   # everything else

#### step 1: read the blok41_51_94.dbf, rename the columns, remove subtotals ####

blok_41 <- read.dbf(file.path(susenas_dir, "blok41_51_94.dbf"), as.is = TRUE)

# confirm the file arrives with the expected columns in the expected order,
# so the positional rename below can never mislabel a column
stopifnot(identical(names(blok_41), c(
  "R101", "R102", "R105", "R203", "R301", "COICOP", "KLP", "KODE",
  "B41K5", "B41K6", "B41K7", "B41K8", "B41K9", "B41K10",
  "KALORI", "PROTEIN", "LEMAK", "KARBO",
  "WERT", "WEIND", "WI1", "WI2", "PSU", "SSU", "STRATA", "RENUM"
)))
stopifnot(nrow(blok_41) == 207771)

names(blok_41) <- c("province", "district", "urban1_rural2",
                    "census_complete1_noncomplete2345",
                    "hh_members",
                    "coicop",
                    "subgroup_code",
                    "food_item_id_urut",
                    "volume_weekly_hh_purchased", "value_weekly_hh_purchased",
                    "volume_weekly_hh_subsistence", "value_weekly_hh_subsistence",
                    "volume_weekly_hh_total", "value_weekly_hh_total",
                    "kalories_weekly_hh_total", "protein_weekly_hh_total",
                    "fat_weekly_hh_total", "carbohydrates_weekly_hh_total",
                    "hh_estimate_scaling", "citizen_estimate_scaling",
                    "sample_id", "hh_id",
                    "primary_sampling_unit", "secondary_sampling_unit",
                    "strata",
                    "hh_id_data_link")

# remove the category subtotal rows (subgroup_code == 0): one row per
# household and food category holding BPS's sums of the item rows below it
# (money and BPS nutrient columns; volumes stored as zero). Summing them
# together with the item rows would count every food twice.
blok_41 <- blok_41 %>%
  filter(subgroup_code != 0)
stopifnot(nrow(blok_41) == 150895)          # 207,771 - 56,876 subtotal rows

# identifiers become text (values unchanged, digit for digit): they are
# labels, not quantities, so they can never be rounded or shown in
# scientific notation
blok_41 <- blok_41 %>%
  mutate(across(c(sample_id, hh_id, primary_sampling_unit,
                  secondary_sampling_unit, strata, hh_id_data_link),
                as.character))
cat("step 1: blok 41 read from the DBF, subtotal rows removed,",
    nrow(blok_41), "rows x", ncol(blok_41), "columns\n")

#### step 2: remove rokok/cigarette items and join the Indonesia FCT Complete ####

# remove rokok: items 184-188 (their subtotal, code 183, left in step 1)
blok41_fct_joined <- blok_41 %>%
  filter(food_item_id_urut <= 182)
stopifnot(nrow(blok_41) - nrow(blok41_fct_joined) == 4259)
stopifnot(nrow(blok41_fct_joined) == 146636)

# join the food composition table: item name, food group, and its 21
# nutrient columns (energy through bdd, the edible portion in %; dha_epa is
# the fourth). Values are per 100 g edible portion; every food item carries
# a bdd and a dha_epa.

fct <- read_csv(file.path(dir_2process, "indonesia_fct_complete.csv"),
                show_col_types = FALSE) %>%
  select(susenas_code, susenas_item, food_group, energy:bdd)
stopifnot(nrow(fct) == 182, !any(duplicated(fct$susenas_code)),
          identical(names(fct)[4:24], c(
            "energy", "protein", "fat", "dha_epa", "carbohydrate", "fiber", "ash",
            "retinol", "beta_carotene", "thiamin", "riboflavin", "niacin",
            "vitamin_c", "calcium", "copper", "iron", "phosphorus", "potassium",
            "sodium", "zinc", "bdd")),
          all(!is.na(fct$bdd[fct$susenas_item != fct$food_group])),
          all(!is.na(fct$dha_epa[fct$susenas_item != fct$food_group])))

blok41_fct_joined <- blok41_fct_joined %>%
  left_join(fct, by = c("food_item_id_urut" = "susenas_code"))

# checks: the join added columns, never rows; every row found its FCT
# label; and the full join check: the distinct (item, label, nutrient)
# rows in the joined table must be exactly the FCT rows of the items
# present, one row per item, so every record carries its item's FCT row
stopifnot(nrow(blok41_fct_joined) == 146636)
stopifnot(all(!is.na(blok41_fct_joined$susenas_item)))
joined_fct <- blok41_fct_joined %>%
  select(susenas_code = food_item_id_urut, all_of(names(fct)[-1])) %>%
  distinct() %>%
  arrange(susenas_code)
stopifnot(!any(duplicated(joined_fct$susenas_code)),
          isTRUE(all.equal(joined_fct,
                           fct %>% filter(susenas_code %in% joined_fct$susenas_code) %>%
                             arrange(susenas_code),
                           check.attributes = FALSE)))
cat("step 2 check:", nrow(joined_fct), "items present, each carrying its FCT row on every record\n")

# record number: a permanent handle for every record, 1 to 146,636 in the
# order of the source file, placed as the first column
blok41_fct_joined <- blok41_fct_joined %>%
  mutate(id = row_number()) %>%
  relocate(id)
stopifnot(identical(blok41_fct_joined$id, seq_len(146636)))

cat("step 2: blok 41 joined to the FCT,",
    nrow(blok41_fct_joined), "rows x", ncol(blok41_fct_joined), "columns\n")

#### step 3: unit conversion ####

# unit conversion: grams in one recorded unit = value x gram factor.
# "Ounce"/"ounce" are the same Indonesian ons (100 g); galon is the
# 19 L water gallon; volume units convert as water (1 ml = 1 g),
# including cooking oil
susenas_units <- read_xlsx(file.path(dir_2process, "food_composition_units.xlsx")) %>%
  mutate(
    unit = tolower(trimws(unit)),
    grams_per_unit = value * case_when(
      unit == "gram"  ~ 1,
      unit == "kg"    ~ 1000,
      unit == "ounce" ~ 100,
      unit == "ml"    ~ 1,
      unit == "liter" ~ 1000,
      unit == "galon" ~ 19000,
      TRUE            ~ NA_real_
    )
  ) %>%
  select(susenas_code, unit, grams_per_unit)
stopifnot(!any(duplicated(susenas_units$susenas_code)))

blok41_fct_joined <- blok41_fct_joined %>%
  left_join(susenas_units, by = c("food_item_id_urut" = "susenas_code"))

# the join added two columns, never rows; every food item has a unit
stopifnot(nrow(blok41_fct_joined) == 146636)
no_unit_items <- blok41_fct_joined %>%
  filter(is.na(grams_per_unit)) %>%
  distinct(food_item_id_urut) %>%
  pull(food_item_id_urut)
if (length(no_unit_items)) stop("items without a unit: ", paste(no_unit_items, collapse = ", "))

write_csv(blok41_fct_joined,
          file.path(dir_2process, "blok41_fct_joined.csv"), na = "")
cat("step 3: blok41_fct_joined.csv written,",
    nrow(blok41_fct_joined), "rows x", ncol(blok41_fct_joined), "columns",
    "(id + blok 41 + FCT + grams per unit)\n")
cat("\npipeline complete: 1 file written\n")
