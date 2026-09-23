# This file builds the complete Indonesia food composition table covering
# every SUSENAS food item 1-182, in two moves:
#   1. stack the two component tables
#        indonesia_fct_nonaquatic_food.csv  (TKPI, SMILING, ASEAN; items outside 16-51)
#        indonesia_fct_aquatic_food.csv     (fish book; items 16-51)
#   2. fill from fct_nutrients_filled.xlsx, sheet Nutrient Fill: one row per
#      cell whose value does not come from the component tables (recipes,
#      borrowed values, zero rules), placed into the stacked table
# It leaves one object in the environment: fct_complete, the table written
# to disk.
# Run order: 1_build_indonesia_fct_aquatic.R and 1_build_indonesia_fct_nonaquatic.R
# (either first), then 2_build_indonesia_fct_complete.R, then
# 3_build_susenas_consumption.R. The leading number is the step.
#
# Name: Farid Annam
# Affiliation: Harvard T.H. Chan School of Public Health
# For: north_maluku
# Date updated: 9/23/2026
###################################################################################################################
# Method:
#  - The stack performs NO arithmetic: 146 non-aquatic + 36 aquatic rows = 182,
#    each susenas_code exactly once, every cell copied verbatim.
#  - Both tables share one panel, the fish book's 21 nutrient columns in the
#    fish book's order (energy to bdd, dha_epa fourth). The complete table has
#    27 columns: six identifiers, then that panel.
#  - Fill rule. Each Nutrient Fill row names one cell (susenas_code, nutrient)
#    and its value. A row whose cause starts "cell replaced" or "cell
#    documented" overwrites the cell; every other row fills a blank cell and
#    the script stops if that cell is not blank. Header rows are never
#    touched. After the fill no food item may have a blank nutrient.
#  - Source column. An item whose 20 panel cells (all but dha_epa) come from
#    the workbook is labelled with the databases behind them, read from the
#    workbook itself: the ingredient sources of its recipe (sheet Recipes) and
#    the source tables of its borrowed cells (sheet Nutrient Fill), as
#    tkpi, afcd, usda, taco, brand (a manufacturer's specification), fao (an
#    FAO/WHO purity specification), several names separated by a comma;
#    source_codes lists the ingredient codes and source ids. Every other item
#    keeps the label of its origin table (tkpi, smiling, asean_fcd, afcd,
#    food_category). Which cells the workbook supplied is recorded in the
#    companion file indonesia_fct_gap_filling.csv (one row per filled cell:
#    value before, value after, method, source table).
#  - Category header rows (13, one per food group) keep every nutrient blank,
#    bdd included.
#  - Validation: every cell the workbook did not touch must equal its origin
#    cell; every cell it touched must equal the workbook value; and the 21
#    nutrient columns must equal sheet Final Values of the workbook cell for
#    cell. The script stops on the first failure.
# If it stops, the intermediate objects stay in the environment for inspection.

# clear workspace
rm(list = ls())
graphics.off()

# libraries
library(tidyverse)
library(readxl)

# directories: the inputs and the outputs all live in 2_process
in_dir  <- '~/Desktop/indonesia_fct/datasets/2_process'
out_dir <- '~/Desktop/indonesia_fct/datasets/2_process'

# column layout of the complete table
meta_cols <- c('coicop_code', 'susenas_code', 'susenas_item', 'food_group',
               'source', 'source_codes')
nutr_cols <- c('energy','protein','fat','dha_epa','carbohydrate','fiber','ash',
               'retinol','beta_carotene','thiamin','riboflavin','niacin',
               'vitamin_c','calcium','copper','iron','phosphorus','potassium',
               'sodium','zinc','bdd')
all_cols  <- c(meta_cols, nutr_cols)

###################################################################################################################
## 1. Read the two component tables; each must carry exactly the shared
##    layout (the aquatic table has no source or source_codes column, which
##    are added here); stack them, a pure row-stack into the 27-column layout

origin <- bind_rows(
  read_csv(file.path(in_dir, 'indonesia_fct_nonaquatic_food.csv'),
           show_col_types = FALSE, progress = FALSE) %>%
    { stopifnot(nrow(.) == 146, identical(names(.), all_cols)); . },
  read_csv(file.path(in_dir, 'indonesia_fct_aquatic_food.csv'),
           show_col_types = FALSE, progress = FALSE) %>%
    { stopifnot(nrow(.) == 36, identical(names(.), c(meta_cols[1:4], nutr_cols))); . } %>%
    mutate(source = ifelse(susenas_item == food_group, 'food_category', 'afcd'),
           source_codes = '') %>%
    select(all_of(all_cols))) %>%
  arrange(susenas_code)

is_header <- origin$source == 'food_category'
stopifnot(nrow(origin) == 182,
          all(origin$susenas_code == 1:182),             # every item once, in order
          identical(names(origin), all_cols),
          sum(is_header) == 13,
          all(is.na(origin[is_header, nutr_cols])))      # header rows: every nutrient blank
cat('Stacked: 146 non-aquatic + 36 aquatic =', nrow(origin), 'x', ncol(origin), '|',
    sum(is.na(origin[!is_header, nutr_cols])), 'blank food cells before the fill\n')

###################################################################################################################
## 2. Fill from fct_nutrients_filled.xlsx, sheet Nutrient Fill

fill <- read_excel(file.path(in_dir, 'fct_nutrients_filled.xlsx'),
                   sheet = 'Nutrient Fill') %>%
  select(susenas_code, susenas_item, nutrient, value, method, cause,
         source_table, source_id, status) %>%
  mutate(susenas_code = as.numeric(susenas_code),
         replaces = str_starts(cause, 'cell replaced|cell documented'))

# the sheet must be complete and consistent with the table: every row
# filled, a numeric value, a panel nutrient, an existing food item (never a
# header row), each cell named once, and the item label as in the table
stopifnot(all(fill$status == 'filled'),
          is.numeric(fill$value), !any(is.na(fill$value)),
          all(fill$nutrient %in% nutr_cols),
          all(fill$susenas_code %in% origin$susenas_code[!is_header]),
          !any(duplicated(fill[c('susenas_code', 'nutrient')])),
          all(fill$susenas_item ==
                origin$susenas_item[match(fill$susenas_code, origin$susenas_code)]))

# the value each named cell holds before the fill; a row that only fills
# must meet a blank cell, otherwise the two sources disagree and the script stops
fill$value_before <- map2_dbl(fill$susenas_code, fill$nutrient,
                              ~ origin[[.y]][origin$susenas_code == .x])
if (any(!fill$replaces & !is.na(fill$value_before)))
  stop('fill row(s) meet a non-blank cell: ',
       paste(fill$susenas_code, fill$nutrient)[!fill$replaces & !is.na(fill$value_before)])

# place every value
fct_complete <- origin
for (i in seq_len(nrow(fill)))
  fct_complete[[fill$nutrient[i]]][fct_complete$susenas_code == fill$susenas_code[i]] <- fill$value[i]

# after the fill: header rows untouched (all blank), and no food item has a blank nutrient
stopifnot(all(is.na(fct_complete[is_header, nutr_cols])),
          !any(is.na(fct_complete[!is_header, nutr_cols])))
cat('Filled:', nrow(fill), 'cells (', sum(fill$replaces), 'replaced,',
    sum(!fill$replaces), 'filled blank ) across', n_distinct(fill$susenas_code),
    'items | method:', paste(names(table(fill$method)), table(fill$method), collapse = ', '), '\n')

###################################################################################################################
## 3. Source label and source codes for the items built entirely from the workbook

# the databases behind an item: the ingredient sources of its recipe (sheet
# Recipes: tkpi, usda, afcd) and the source tables of its borrowed cells
# (sheet Nutrient Fill); zero rules carry no database and are skipped
recipes <- read_excel(file.path(in_dir, 'fct_nutrients_filled.xlsx'), sheet = 'Recipes') %>%
  filter(source != 'calculated')
source_word <- function(s) case_when(str_detect(s, '^TKPI')     ~ 'tkpi',
                                     str_detect(s, 'USDA')      ~ 'usda',
                                     str_detect(s, 'TACO')      ~ 'taco',
                                     str_detect(s, 'MyFCD')     ~ 'myfcd',
                                     str_detect(s, 'fish book') ~ 'afcd',
                                     str_detect(s, 'Ajinomoto') ~ 'brand',
                                     str_detect(s, 'JECFA')     ~ 'fao',
                                     TRUE                       ~ NA_character_)
label_order <- c('tkpi', 'smiling', 'asean_fcd', 'afcd', 'taco', 'usda', 'myfcd', 'brand', 'fao')

whole_items <- fill %>% filter(nutrient != 'dha_epa') %>% count(susenas_code) %>%
  filter(n == length(nutr_cols) - 1) %>% pull(susenas_code)
labels <- map_dfr(whole_items, function(code) {
  borrowed <- fill %>% filter(susenas_code == code, method != 'recipe', source_table != 'none')
  words <- c(recipes$source[recipes$susenas_code == code], source_word(borrowed$source_table))
  tibble(susenas_code = code,
         source       = paste(label_order[label_order %in% words], collapse = ', '),
         source_codes = paste(unique(c(recipes$ingredient_code[recipes$susenas_code == code], borrowed$source_id)), collapse = '; '))
})
stopifnot(all(labels$source != ''), !any(is.na(labels$source_codes)))

fct_complete <- fct_complete %>%
  mutate(source       = ifelse(susenas_code %in% labels$susenas_code, labels$source[match(susenas_code, labels$susenas_code)], source),
         source_codes = ifelse(susenas_code %in% labels$susenas_code, labels$source_codes[match(susenas_code, labels$susenas_code)], source_codes))
stopifnot(!any(fct_complete$source %in% c('no_match', 'tkpi_partial')))   # every such item is now built from the workbook
cat('Source labels:', nrow(labels), 'items built entirely from the workbook labelled by their databases;',
    'the other', 182 - nrow(labels), 'rows keep their origin label\n')

###################################################################################################################
## 4. Write the complete table and the companion file of filled cells

stopifnot(dir.exists(out_dir))
# write_csv stores full round-trip precision, so every value survives the
# file exactly as it arrived from its source
write_csv(fct_complete, file.path(out_dir, 'indonesia_fct_complete.csv'), na = '')
cat('Wrote indonesia_fct_complete.csv  (', nrow(fct_complete), ' x ', ncol(fct_complete), ')\n', sep = '')
print(table(fct_complete$source))

write_csv(fill %>%
            transmute(susenas_code, susenas_item, nutrient, value_before,
                      value_after = value, method, source_table) %>%
            arrange(susenas_code, match(nutrient, nutr_cols)),
          file.path(out_dir, 'indonesia_fct_gap_filling.csv'), na = '')
cat('Wrote indonesia_fct_gap_filling.csv  (', nrow(fill), ' x 7 )\n', sep = '')

###################################################################################################################
## 5. Validation - the script stops on the first failure

same <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) <= 1e-9)

# a. every cell the workbook did not touch equals its origin cell; every cell
#    it touched equals the workbook value; identifiers unchanged
for (cn in nutr_cols) {
  wb <- fill$value[match(paste(fct_complete$susenas_code, cn),
                         paste(fill$susenas_code, fill$nutrient))]   # NA where untouched
  ok <- ifelse(is.na(wb), same(fct_complete[[cn]], origin[[cn]]), same(fct_complete[[cn]], wb))
  if (!all(ok)) stop('value mismatch in ', cn, ' for item(s) ',
                     paste(fct_complete$susenas_code[!ok], collapse = ', '))
}
for (cn in meta_cols[1:4])
  stopifnot(identical(replace_na(as.character(fct_complete[[cn]]), ''),
                      replace_na(as.character(origin[[cn]]), '')))
keep <- !fct_complete$susenas_code %in% labels$susenas_code           # origin label kept where not relabelled
stopifnot(identical(fct_complete$source[keep], origin$source[keep]),
          identical(replace_na(fct_complete$source_codes[keep], ''), replace_na(origin$source_codes[keep], '')))

# b. the 21 nutrient columns equal sheet Final Values of the workbook, the
#    expected outcome of the fill, cell for cell
final_values <- read_excel(file.path(in_dir, 'fct_nutrients_filled.xlsx'),
                           sheet = 'Final Values')
stopifnot(nrow(final_values) == 182,
          all(final_values$susenas_code == fct_complete$susenas_code),
          identical(final_values$susenas_item, fct_complete$susenas_item),
          identical(final_values$source, fct_complete$source),           # same source labels
          sum(final_values$cells_from_this_file) == nrow(fill))
for (cn in nutr_cols) {
  ok <- same(fct_complete[[cn]], final_values[[cn]])
  if (!all(ok)) stop('differs from sheet Final Values in ', cn, ' for item(s) ',
                     paste(fct_complete$susenas_code[!ok], collapse = ', '))
}
cat('Validation passed: untouched cells match their origin table, filled cells',
    'match the workbook, and all', length(nutr_cols), 'nutrient columns and the',
    'source labels equal sheet Final Values.\n')

rm(list = setdiff(ls(), 'fct_complete'))
