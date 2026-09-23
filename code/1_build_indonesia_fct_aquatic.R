# This step builds the aquatic Indonesia food composition table
# (indonesia_fct_aquatic_food.csv) from the original fish book,
# fish_book_afcd.csv, and checks the result. It leaves one object in the
# environment: fct_aquatic, the table written to disk.
# Run order: 1_build_indonesia_fct_aquatic.R and 1_build_indonesia_fct_nonaquatic.R
# (either first), then 2_build_indonesia_fct_complete.R, then
# 3_build_susenas_consumption.R. The leading number is the step.
#
# Name: Farid Annam
# Affiliation: Harvard T.H. Chan School of Public Health
# For: north_maluku
# Date updated: 9/23/2026
###################################################################################################################
# The fish book (1_master/clean/fish_book_afcd.csv, the original file,
# READ-ONLY) has one row per linked fish, 39 rows: the IKAN header (item 16)
# plus 38 fish covering SUSENAS items 17-51. It already carries susenas_code,
# susenas_item, food_group, food_name, taxonomy and a 21-column nutrient
# panel (energy, protein, fat, dha_epa, carbohydrate, fiber, ash, retinol,
# beta_carotene, thiamin, riboflavin, niacin, vitamin_c, calcium, copper,
# iron, phosphorus, potassium, sodium, zinc, bdd). The output keeps these
# names and this order.
#
# Method:
#  - Every SUSENAS item takes the plain mean of its linked fish-book rows.
#    33 items link to one row and are that row at full precision; two pool:
#      18 Tongkol group (fresh) = mean of Tongkol, Tuna, Cakalang/dencis
#      26 Mas, nila             = mean of Mas, Nila
#  - A nutrient missing for a member is excluded from that nutrient's mean,
#    never treated as zero (no cell is missing in the current fish book).
#  - bdd is stored in the fish book as a FRACTION (0.19-0.61). TKPI stores
#    bdd as a PERCENTAGE (0-100), so bdd is multiplied by 100 here to match.
#    This is the only value transformation in the script; the source file
#    is not touched.
#  - All other values are used exactly as published.
#  - Item 16 "IKAN" is the fish category header: every nutrient blank, bdd
#    included (the same convention as the header rows of the non-aquatic table).
# The script stops on any unexpected condition instead of passing it through.
# If it stops, the intermediate objects stay in the environment for inspection.

# clear workspace
rm(list = ls())
graphics.off()

# libraries
library(tidyverse)

# directories
fish_file <- '~/Desktop/indonesia_fct/datasets/1_master/clean/fish_book_afcd.csv'
link_file <- '~/Desktop/indonesia_fct/datasets/2_process/linking_susenas_afcd.csv'
out_dir   <- '~/Desktop/indonesia_fct/datasets/2_process'

# nutrient columns: taken from the fish book itself (every column after
# 'kingdom'), so their names and order are exactly the fish book's. The same
# standard names are used in the non-aquatic and complete tables.
id_cols <- c('coicop_code','susenas_code','susenas_item','food_group')

###################################################################################################################
## 1. Read the fish book and confirm its structure

fish <- read_csv(fish_file, col_types = cols(.default = col_character()),
                 name_repair = 'unique_quiet', progress = FALSE)
names(fish)[1] <- 'row_id'                 # the unnamed first column

stopifnot(nrow(fish) == 39, 'kingdom' %in% names(fish),
          all(c(id_cols, 'food_name', 'method', 'sciname') %in% names(fish)))
nutr <- names(fish)[(which(names(fish) == 'kingdom') + 1):ncol(fish)]
stopifnot(length(nutr) == 21, all(c('bdd', 'dha_epa', 'protein') %in% nutr))
cat('Nutrient columns (from the fish book):', paste(nutr, collapse = ' '), '\n')

fish <- fish %>%
  mutate(susenas_code = as.numeric(susenas_code),
         across(all_of(nutr), as.numeric))    # every nutrient cell is numeric or blank

header16 <- fish %>% filter(susenas_code == 16)
fish     <- fish %>% filter(susenas_code != 16)
stopifnot(nrow(header16) == 1, header16$susenas_item == 'IKAN',
          all(is.na(header16[nutr])))
stopifnot(nrow(fish) == 38, all(!is.na(fish[nutr])),
          setequal(fish$susenas_code, 17:51))

n_fish <- fish %>% count(susenas_code, susenas_item)
stopifnot(nrow(n_fish) == 35,
          identical(sort(n_fish$susenas_code[n_fish$n > 1]), c(18, 26)),
          n_fish$n[n_fish$susenas_code == 18] == 3,
          n_fish$n[n_fish$susenas_code == 26] == 2)
cat('Fish book: 38 fish across 35 items; pooled items:',
    paste(n_fish$susenas_item[n_fish$n > 1], collapse = ' | '), '\n')

###################################################################################################################
## 2. Cross-check against the linking file (linking_susenas_afcd.csv)
##    The linking file is the curated record of which fish belongs to which
##    SUSENAS item. The fish book carries the same information, so the two
##    must agree before the fish book's values are used.

lk <- read_csv(link_file, col_types = cols(.default = col_character()),
               progress = FALSE) %>%
  filter(as.numeric(susenas_code) != 16) %>%
  mutate(susenas_code = as.numeric(susenas_code)) %>%
  arrange(susenas_code, food_name)
fish <- fish %>% arrange(susenas_code, food_name)
stopifnot(nrow(lk) == nrow(fish),
          identical(lk$susenas_code, fish$susenas_code),
          identical(lk$food_name,    fish$food_name),
          identical(lk$method,       fish$method),
          identical(lk$sciname,      fish$sciname))
cat('Linking file and fish book agree on code, name, method, sciname\n')

###################################################################################################################
## 3. bdd: fraction -> percentage (the only transformation)

stopifnot(all(fish$bdd > 0 & fish$bdd <= 1))     # confirms the fraction basis
fish <- fish %>% mutate(bdd = bdd * 100)
cat('bdd converted from fraction to percentage: range',
    round(min(fish$bdd), 2), '-', round(max(fish$bdd), 2), '%\n')

###################################################################################################################
## 4. Assemble - header row 16 + the plain mean of each item's linked rows,
##    4 id columns + 21 nutrients

fct_aquatic <- bind_rows(header16, fish) %>%
  distinct(coicop_code, susenas_code, susenas_item, food_group) %>%
  left_join(fish %>%
              group_by(susenas_code) %>%
              summarise(across(all_of(nutr), ~ {
                v <- .x[!is.na(.x)]
                if (length(v) == 0) NA_real_ else mean(v)
              }), .groups = 'drop'),
            by = 'susenas_code') %>%
  arrange(susenas_code) %>%                             # the IKAN header keeps every nutrient blank
  select(all_of(id_cols), all_of(nutr))

stopifnot(nrow(fct_aquatic) == 36, ncol(fct_aquatic) == 4 + length(nutr),
          all(fct_aquatic$susenas_code == 16:51),
          all(is.na(fct_aquatic[fct_aquatic$susenas_code == 16, nutr])),
          all(!is.na(fct_aquatic[fct_aquatic$susenas_code != 16, nutr])))

# hand check of the two pooled items on protein, and single-row items must
# equal their fish-book row exactly in every nutrient
stopifnot(near(fct_aquatic$protein[fct_aquatic$susenas_code == 18],
               mean(fish$protein[fish$susenas_code == 18])),
          near(fct_aquatic$protein[fct_aquatic$susenas_code == 26],
               mean(fish$protein[fish$susenas_code == 26])))
for (cn in nutr) {
  single <- n_fish$susenas_code[n_fish$n == 1]
  stopifnot(all(fct_aquatic[[cn]][match(single, fct_aquatic$susenas_code)] ==
                fish[[cn]][match(single, fish$susenas_code)]))
}

###################################################################################################################
## 5. Write, and keep only the table in the environment

stopifnot(dir.exists(out_dir))
write_csv(fct_aquatic, file.path(out_dir, 'indonesia_fct_aquatic_food.csv'), na = '')
cat('Wrote indonesia_fct_aquatic_food.csv  (', nrow(fct_aquatic), ' x ', ncol(fct_aquatic), ')\n', sep = '')

rm(list = setdiff(ls(), 'fct_aquatic'))
