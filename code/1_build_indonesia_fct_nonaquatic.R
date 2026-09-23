# This step builds the non-aquatic Indonesia food composition table
# (indonesia_fct_nonaquatic_food.csv) from the original source tables
# (TKPI 2020, SMILING 2013, ASEAN FCD 2014) and the SUSENAS linking files.
# It leaves one object in the environment: fct_nonaquatic, the table
# written to disk.
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
#  - Each SUSENAS item takes the plain mean across its code pool; missing
#    values are excluded from the mean, never treated as zero.
#  - TKPI is the primary source; SMILING covers 7 prepared-food items and
#    ASEAN FCD covers items 178 and 179 only. Every item draws on one source.
#  - The nutrient panel is the fish book's (fish_book_afcd.csv): 21 columns
#    in the fish book's names and order, energy to bdd with dha_epa fourth.
#    Source nutrients outside this panel (TKPI water and total carotene,
#    the SMILING-only vitamins) are not carried. A panel nutrient a source
#    does not carry stays blank for that source's items.
#  - ASEAN FCD notation: "-" = no value (NA), "(x)" = fallback method (used as x),
#    "0p" = presumed zero (0), "T"/"tr"/"nd" = NA.
#  - ASEAN FCD beverages are printed per 100 ml in the book; asean_fcd_2014.csv
#    already stores them per 100 g (printed value divided by the density DEN),
#    so no basis conversion is applied here. Two known cells are checked so
#    the division can never be applied twice.
#  - bdd (edible portion) is in percent in TKPI and SMILING and is used as is.
#    Three sourced items have no bdd in their source (177 from SMILING; 178
#    and 179 from ASEAN FCD). All three are packaged drinks consumed whole,
#    so bdd = 100. Category header rows keep bdd blank like every other nutrient.
#  - dha_epa: none of the three sources carries DHA + EPA. Every sourced
#    non-aquatic item is set to 0 g per 100 g (section 5). Header rows and
#    unmatched items stay blank.
#  - Category header rows keep every nutrient blank; items with no match keep
#    blank nutrients (the fill step of the complete table fills them); items
#    165 and 171 are labelled tkpi_partial.
# The script stops on any unexpected condition instead of passing it through.
# If it stops, the intermediate objects stay in the environment for inspection.

# clear workspace
rm(list = ls())
graphics.off()

# libraries
library(tidyverse)

# directories
src_dir  <- '~/Desktop/indonesia_fct/datasets/1_master/clean'   # source tables
proc_dir <- '~/Desktop/indonesia_fct/datasets/2_process'        # linking files, output

# output columns: six identifiers, then the fish book's 21 nutrient columns
# in the fish book's order. dha_epa is assigned in section 5; the other 20
# (the panel) are averaged from the sources
id_cols   <- c('coicop_code','susenas_code','susenas_item','food_group',
               'source','source_codes')
nutr_cols <- c('energy','protein','fat','dha_epa','carbohydrate','fiber','ash',
               'retinol','beta_carotene','thiamin','riboflavin','niacin',
               'vitamin_c','calcium','copper','iron','phosphorus','potassium',
               'sodium','zinc','bdd')
panel <- setdiff(nutr_cols, 'dha_epa')

# mean over the panel for a set of rows; a nutrient the source does not carry
# stays NA, and NAs are excluded from the mean
panel_mean <- function(df) {
  vapply(panel, function(n) {
    if (!n %in% names(df)) return(NA_real_)
    v <- df[[n]]
    if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
  }, numeric(1))
}

###################################################################################################################
## 1. TKPI - read and map columns

tkpi_raw <- read_csv(file.path(src_dir, 'tkpi_2020.csv'),
                     col_types = cols(.default = col_character()),
                     progress = FALSE)
stopifnot(names(tkpi_raw)[1] == 'KODE')

# TKPI column -> output column name (the 20 panel columns TKPI carries;
# AIR and KAROTEN_TOTAL are outside the panel)
tkpi_map <- c(ENERGI='energy', PROTEIN='protein', LEMAK='fat',
              KH='carbohydrate', SERAT='fiber', ABU='ash', KALSIUM='calcium',
              FOSFOR='phosphorus', BESI='iron', NATRIUM='sodium',
              KALIUM='potassium', TEMBAGA='copper', SENG='zinc',
              RETINOL='retinol', BETA_KAROTEN='beta_carotene',
              THIAMIN='thiamin', RIBOFLAVIN='riboflavin', NIASIN='niacin',
              VITAMIN_C='vitamin_c', BDD='bdd')
stopifnot(all(names(tkpi_map) %in% names(tkpi_raw)), all(tkpi_map %in% panel))

# convert every nutrient column to numeric; stop if any non-blank cell
# refuses to parse (that would be an unknown data problem, not a blank)
tkpi <- tkpi_raw['KODE']
for (rawcol in names(tkpi_map)) {
  txt <- str_trim(tkpi_raw[[rawcol]])
  num <- suppressWarnings(as.numeric(txt))
  bad <- !is.na(txt) & txt != '' & is.na(num)
  if (any(bad)) stop('TKPI column ', rawcol, ' has unparseable values: ',
                     paste(unique(txt[bad]), collapse = ' | '))
  tkpi[[tkpi_map[rawcol]]] <- num
}
stopifnot(!any(duplicated(tkpi$KODE)))
cat('TKPI parsed:', nrow(tkpi), 'rows,', length(tkpi_map), 'nutrient columns numeric\n')

###################################################################################################################
## 2. SMILING - lookup by workbook row number or food code

smil <- read_csv(file.path(src_dir, 'smiling_2013.csv'),
                 show_col_types = FALSE, progress = FALSE)

# SMILING column -> output column name (the 15 panel columns SMILING
# carries). SMILING has no phosphorus, sodium, potassium or copper, so those
# stay blank for its items. `edible` is the edible portion in percent (bdd).
smil_map <- c(enerc_kcal='energy', protcnt_g='protein', fat_g='fat',
              choavldf_g='carbohydrate', fibtg_g='fiber', ash_g='ash',
              ca_mg='calcium', fe_mg='iron', zn_mg='zinc',
              retol_mcg='retinol', cartb_mcg='beta_carotene',
              thia_mg='thiamin', ribf_mg='riboflavin', nia_mg='niacin',
              vit_c_mg='vitamin_c', edible='bdd')
stopifnot(all(names(smil_map) %in% names(smil)), all(smil_map %in% panel))
smil_std <- smil[c('smiling_row', 'food_code', 'food_name_english')]
for (rawcol in names(smil_map))
  smil_std[[smil_map[rawcol]]] <- suppressWarnings(as.numeric(smil[[rawcol]]))

lk_sm <- read_csv(file.path(proc_dir, 'linking_susenas_smiling.csv'),
                  col_types = cols(.default = col_character()), progress = FALSE)

# resolve each linking entry to one smiling_row. "(uncoded row N)" means
# workbook row N; anything else is a food_code. Two SMILING codes (IDF103,
# IDG128) are duplicated in the source, so a code lookup must prove unique.
smiling_row_for <- function(code_str, expect_name) {
  if (str_detect(code_str, 'uncoded row')) {
    r <- as.numeric(str_extract(code_str, '\\d+'))
  } else {
    hit <- smil_std$smiling_row[!is.na(smil_std$food_code) &
                                smil_std$food_code == code_str]
    if (length(hit) != 1) stop('SMILING code ', code_str, ' matched ',
                               length(hit), ' rows - expected exactly 1')
    r <- hit
  }
  # audit: the food at that row must be the food the linking file names
  found <- smil_std$food_name_english[smil_std$smiling_row == r]
  if (tolower(str_trim(found)) != tolower(str_trim(expect_name)))
    stop('SMILING row ', r, " is '", found, "' but linking expects '",
         expect_name, "'")
  r
}
lk_sm$row <- mapply(smiling_row_for, lk_sm$smiling_code,
                    lk_sm$smiling_food_name_english)
cat('SMILING: all', nrow(lk_sm), 'linking entries resolved and name-verified\n')

###################################################################################################################
## 3. ASEAN - parse printed symbols (values already per 100 g)

asean_raw <- read_csv(file.path(src_dir, 'asean_fcd_2014.csv'),
                      col_types = cols(.default = col_character()),
                      progress = FALSE)
names(asean_raw)[1] <- 'food_id'    # first header carries a BOM

# ASEAN column -> output column name (the 19 panel columns ASEAN carries;
# ASEAN has no edible portion). Sodium is printed under the INFOODS tag NA.
asean_map <- c(ENERC='energy', PROCNT='protein', FAT='fat',
               CHOAVLDF='carbohydrate', FIBTG='fiber', ASH='ash',
               CA='calcium', P='phosphorus', 'NA'='sodium', K='potassium',
               FE='iron', CU='copper', ZN='zinc', RETOL='retinol',
               CARTB='beta_carotene', THIA='thiamin',
               RIBF='riboflavin', NIA='niacin', VITC='vitamin_c')
stopifnot(all(names(asean_map) %in% names(asean_raw)), all(asean_map %in% panel))

# printed-notation parser; anything unrecognized stops the script
parse_asean <- function(txt) {
  t <- str_trim(txt)
  out <- rep(NA_real_, length(t))
  for (i in seq_along(t)) {
    z <- t[i]
    if (is.na(z) || z == '' || z == '-') next                      # no value
    if (tolower(z) %in% c('t','tr','nd')) next                     # trace / nd
    if (z == '0p') { out[i] <- 0; next }                           # presumed 0
    m <- str_match(z, '^[\\."]?\\((-?[0-9.]+)\\)$')[,2]            # (x) fallback
    if (!is.na(m)) { out[i] <- as.numeric(m); next }
    v <- suppressWarnings(as.numeric(z))
    if (!is.na(v)) { out[i] <- v; next }
    stop("ASEAN value not recognised: '", z, "'")
  }
  out
}

lk_as <- read_csv(file.path(proc_dir, 'linking_susenas_asean.csv'),
                  col_types = cols(.default = col_character()), progress = FALSE)
ase <- asean_raw %>% filter(food_id %in% lk_as$asean_code)
stopifnot(nrow(ase) == nrow(lk_as))               # every code found once
stopifnot(all(ase$row_status == 'primary'))       # no duplicate printings

# the file stores the beverages per 100 g: the printed per-100 ml value
# divided by the density DEN. Two known cells prove it (AAJ19 energy printed
# 83 kcal per 100 ml, density 1.047; AAQ15 printed 55 kcal, density 1.041),
# so a file still holding the printed values stops the script here
stopifnot(isTRUE(all.equal(parse_asean(ase$ENERC[ase$food_id == 'AAJ19']), 83 / 1.047)),
          isTRUE(all.equal(parse_asean(ase$ENERC[ase$food_id == 'AAQ15']), 55 / 1.041)))

ase_std <- ase['food_id']
for (rawcol in names(asean_map))
  ase_std[[asean_map[rawcol]]] <- parse_asean(ase[[rawcol]])
cat('ASEAN:', nrow(ase_std), 'rows parsed, values per 100 g as stored\n')

###################################################################################################################
## 4. Assemble the item table

lk_tk <- read_csv(file.path(proc_dir, 'linking_susenas_tkpi.csv'),
                  col_types = cols(.default = col_character()), progress = FALSE)

# every pooled TKPI code must exist in the TKPI table
pool_codes <- unique(na.omit(lk_tk$tkpi_code[lk_tk$tkpi_code != '']))
if (length(setdiff(pool_codes, tkpi$KODE)))
  stop('TKPI codes not found: ', paste(setdiff(pool_codes, tkpi$KODE), collapse = ', '))

# item universe = TKPI linking file (minus tobacco) + SMILING + ASEAN items
items <- bind_rows(
  lk_tk %>%
    filter(food_group != 'ROKOK DAN TEMBAKAU') %>%
    group_by(susenas_code) %>%
    summarise(coicop_code = first(coicop_code),
              susenas_item = first(susenas_item),
              food_group  = first(food_group),
              codes = paste(tkpi_code[!is.na(tkpi_code) & tkpi_code != ''],
                            collapse = '; '),
              n_codes = sum(!is.na(tkpi_code) & tkpi_code != ''),
              .groups = 'drop') %>%
    mutate(pool = 'tkpi'),
  lk_sm %>% group_by(susenas_code) %>%
    summarise(coicop_code = first(coicop_code),
              susenas_item = first(susenas_item),
              food_group  = first(food_group),
              codes = paste(smiling_code, collapse = '; '),
              n_codes = n(), .groups = 'drop') %>%
    mutate(pool = 'smiling'),
  lk_as %>% group_by(susenas_code) %>%
    summarise(coicop_code = first(coicop_code),
              susenas_item = first(susenas_item),
              food_group  = first(food_group),
              codes = paste(asean_code, collapse = '; '),
              n_codes = n(), .groups = 'drop') %>%
    mutate(pool = 'asean')) %>%
  mutate(susenas_code = as.numeric(susenas_code)) %>%
  arrange(susenas_code)
stopifnot(!any(duplicated(items$susenas_code)))   # no item in two pools
stopifnot(nrow(items) == 146)
cat('Item universe:', nrow(items), 'items\n')

partial_items <- c(165, 171)   # some pool members lack full TKPI coverage

vals <- matrix(NA_real_, nrow = nrow(items), ncol = length(panel),
               dimnames = list(NULL, panel))
src <- character(nrow(items)); src_codes <- character(nrow(items))

for (i in seq_len(nrow(items))) {
  it <- items[i, ]; key <- as.character(it$susenas_code)

  if (it$pool == 'smiling') {
    rows <- lk_sm$row[lk_sm$susenas_code == key]
    vals[i, ] <- panel_mean(smil_std[smil_std$smiling_row %in% rows, ])
    src[i] <- 'smiling'; src_codes[i] <- it$codes

  } else if (it$pool == 'asean') {
    cds <- lk_as$asean_code[lk_as$susenas_code == key]
    vals[i, ] <- panel_mean(ase_std[match(cds, ase_std$food_id), ])
    src[i] <- 'asean_fcd'; src_codes[i] <- it$codes

  } else if (it$susenas_item == it$food_group && it$n_codes == 0) {
    src[i] <- 'food_category'                     # category header row

  } else if (it$n_codes == 0) {
    src[i] <- 'no_match'                          # real item, no code pool

  } else {
    cds <- str_split(it$codes, ';\\s*')[[1]]
    vals[i, ] <- panel_mean(tkpi[match(cds, tkpi$KODE), ])
    src[i] <- if (it$susenas_code %in% partial_items) 'tkpi_partial' else 'tkpi'
    src_codes[i] <- it$codes
  }
}

fct_nonaquatic <- tibble(
  coicop_code  = ifelse(is.na(items$coicop_code), '', str_trim(items$coicop_code)),
  susenas_code = items$susenas_code,
  susenas_item = items$susenas_item,
  food_group   = items$food_group,
  source       = src,
  source_codes = src_codes) %>%
  bind_cols(as_tibble(vals))

###################################################################################################################
## 5. Edible portion and dha_epa

is_header  <- fct_nonaquatic$source == 'food_category'
is_nomatch <- fct_nonaquatic$source == 'no_match'
is_sourced <- !is_header & !is_nomatch

# bdd: three sourced items have no edible portion in their source (177 from
# SMILING; 178 and 179 from ASEAN FCD). All three are packaged drinks
# consumed whole, so a blank bdd on these items becomes 100. A blank bdd on
# any other sourced item is unexpected and stops the script. Category
# header rows and unmatched items stay blank.
fully_consumed <- c(177, 178, 179)
bdd_blank <- fct_nonaquatic$susenas_code[is_sourced & is.na(fct_nonaquatic$bdd)]
if (length(setdiff(bdd_blank, fully_consumed)))
  stop('unexpected blank bdd for sourced item(s): ',
       paste(setdiff(bdd_blank, fully_consumed), collapse = ', '))
fct_nonaquatic <- fct_nonaquatic %>%
  mutate(bdd = ifelse(is.na(bdd) & susenas_code %in% fully_consumed, 100, bdd))
cat('bdd: blank -> 100 for item(s)', paste(bdd_blank, collapse = ', '),
    '| blank for', sum(is_header), 'header rows\n')

# dha_epa: none of TKPI, SMILING or ASEAN FCD carries DHA + EPA, and these
# foods contain none of note, so every sourced non-aquatic item is set to
# 0 g per 100 g; header rows and unmatched items stay blank.
dha_epa_nonaquatic <- 0
fct_nonaquatic <- fct_nonaquatic %>%
  mutate(dha_epa = ifelse(is_sourced, dha_epa_nonaquatic, NA_real_)) %>%
  select(all_of(c(id_cols, nutr_cols)))                    # fish book column order
cat('dha_epa:', dha_epa_nonaquatic, 'for', sum(is_sourced), 'sourced items\n')

###################################################################################################################
## 6. Checks, write, and keep only the table in the environment

# layout checks: 146 rows, 6 identifiers + 21 nutrient columns in the fish
# book's order; header rows and unmatched items fully blank; every sourced
# item has at least one nutrient value, a bdd and a dha_epa
stopifnot(nrow(fct_nonaquatic) == 146, ncol(fct_nonaquatic) == 27,
          identical(names(fct_nonaquatic), c(id_cols, nutr_cols)))
stopifnot(all(is.na(fct_nonaquatic[is_header, nutr_cols])),
          all(is.na(fct_nonaquatic[is_nomatch, nutr_cols])),
          all(rowSums(!is.na(fct_nonaquatic[is_sourced, panel])) > 0),
          all(!is.na(fct_nonaquatic$bdd[is_sourced])),
          all(fct_nonaquatic$dha_epa[is_sourced] == dha_epa_nonaquatic))

stopifnot(dir.exists(proc_dir))
write_csv(fct_nonaquatic, file.path(proc_dir, 'indonesia_fct_nonaquatic_food.csv'), na = '')
cat('Wrote indonesia_fct_nonaquatic_food.csv  (', nrow(fct_nonaquatic), ' x ', ncol(fct_nonaquatic), ')\n', sep = '')
print(table(fct_nonaquatic$source))

rm(list = setdiff(ls(), 'fct_nonaquatic'))
