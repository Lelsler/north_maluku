# PLEASE READ THIS BEFORE RUN THE SCRIPT
# There will be 4 datasets built from these scripts. Run them one by one from 1 to 4. 
# Change the output directory to your directory
# 1. indonesia_fct_nonaquatic_food.csv
# 2. indonesia_fct_aquatic_food.csv
# 3. indonesia_fct_gap_filling.csv (also generates  usda_myfcd_nut_values.csv)
# 4. indonesia_fct_complete.csv (final combination from nonaquatic, aquatic, and gap filling files)
# Each script is marked by two-line hashtags, as shown below
###################################################################################################################
###################################################################################################################
#
#
# This step 1 builds the non-aquatic Indonesia food composition table
# (indonesia_fct_nonaquatic_food.csv) from the raw source tables and the
# SUSENAS linking files, and checks the result against the published table.
#
# Name: Farid Annam
# Affiliation: Harvard T.H. Chan School of Public Health
# For: north_maluku
# Date updated: 8/24/2026
###################################################################################################################
# Method:
#  - The nutrient panel is TKPI's 21 nutrients + bdd (edible portion, %).
#  - Each SUSENAS item takes the plain mean across its code pool; missing
#    values are excluded from the mean, never treated as zero.
#  - TKPI is the primary source; SMILING covers 7 prepared-food items and
#    ASEAN FCD covers items 178 and 179 only.
#  - ASEAN notation: "-" = no value (NA), "(x)" = fallback method (used as x),
#    "0p" = presumed zero (0), "T"/"tr"/"nd" = NA.
#  - ASEAN beverages are printed per 100 ml and are converted to per 100 g by
#    dividing by density (DEN, g/ml).
#  - Category header rows keep blank nutrients; items with no match keep blank
#    nutrients; items 165 and 171 are labelled tkpi_partial.
# The script stops on any unexpected condition instead of passing it through.

# clear workspace
rm(list = ls())
graphics.off()

# libraries
library(tidyverse)

# directories
wk_dir  <- '~/Documents/Github/north_maluku'
out_dir <- '~/Desktop/indonesia_fct/datasets/2_process'

# the output nutrient columns, in published order
panel <- c('water','energy','protein','fat','carbohydrate','fiber','ash',
           'calcium','phosphorus','iron','sodium','potassium','copper','zinc',
           'retinol','beta_carotene','total_carotene','thiamin','riboflavin',
           'niacin','vitamin_c','bdd')

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
## 1. TKPI - read raw, fix the one known parsing trap, map columns

tkpi_raw <- read_csv(file.path(wk_dir, 'data/tkpi_2020.csv'),
                     col_types = cols(.default = col_character()),
                     progress = FALSE)
stopifnot(names(tkpi_raw)[1] == 'KODE')

# one KAROTEN_TOTAL value is written "1,480" (thousands separator, genuinely
# 1480) - remove the comma before numeric conversion
cat('TKPI KAROTEN_TOTAL cells with a comma:',
    sum(str_detect(tkpi_raw$KAROTEN_TOTAL, ','), na.rm = TRUE), '\n')
tkpi_raw$KAROTEN_TOTAL <- str_remove_all(tkpi_raw$KAROTEN_TOTAL, ',')

# raw TKPI column -> standard column name
tkpi_map <- c(AIR='water', ENERGI='energy', PROTEIN='protein', LEMAK='fat',
              KH='carbohydrate', SERAT='fiber', ABU='ash', KALSIUM='calcium',
              FOSFOR='phosphorus', BESI='iron', NATRIUM='sodium',
              KALIUM='potassium', TEMBAGA='copper', SENG='zinc',
              RETINOL='retinol', BETA_KAROTEN='beta_carotene',
              KAROTEN_TOTAL='total_carotene', THIAMIN='thiamin',
              RIBOFLAVIN='riboflavin', NIASIN='niacin', VITAMIN_C='vitamin_c',
              BDD='bdd')
stopifnot(all(names(tkpi_map) %in% names(tkpi_raw)))

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
cat('TKPI parsed:', nrow(tkpi), 'rows, all panel columns numeric\n')

###################################################################################################################
## 2. SMILING - lookup by workbook row number or food code

smil <- read_csv(file.path(wk_dir, 'data/smiling_2013.csv'),
                 show_col_types = FALSE, progress = FALSE)

# SMILING column -> standard name. SMILING has no phosphorus, sodium,
# potassium or copper; oth_car_mcg is NOT total carotene and is not mapped;
# `edible` is the edible portion (bdd)
smil_map <- c(water_g='water', enerc_kcal='energy', protcnt_g='protein',
              fat_g='fat', choavldf_g='carbohydrate', fibtg_g='fiber',
              ash_g='ash', ca_mg='calcium', fe_mg='iron', zn_mg='zinc',
              retol_mcg='retinol', cartb_mcg='beta_carotene',
              thia_mg='thiamin', ribf_mg='riboflavin', nia_mg='niacin',
              vit_c_mg='vitamin_c', edible='bdd')
stopifnot(all(names(smil_map) %in% names(smil)))
smil_std <- smil[c('smiling_row', 'food_code', 'food_name_english')]
for (rawcol in names(smil_map))
  smil_std[[smil_map[rawcol]]] <- suppressWarnings(as.numeric(smil[[rawcol]]))

lk_sm <- read_csv(file.path(wk_dir, 'data/linking_susenas_smiling.csv'),
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
## 3. ASEAN - parse printed symbols, convert per 100 ml rows to per 100 g

asean_raw <- read_csv(file.path(wk_dir, 'data/asean_fcd_2014.csv'),
                      col_types = cols(.default = col_character()),
                      progress = FALSE)
names(asean_raw)[1] <- 'food_id'    # first header carries a BOM

asean_map <- c(WATER='water', ENERC='energy', PROCNT='protein', FAT='fat',
               CHOAVLDF='carbohydrate', FIBTG='fiber', ASH='ash',
               CA='calcium', P='phosphorus', 'NA'='sodium', K='potassium',
               FE='iron', CU='copper', ZN='zinc', RETOL='retinol',
               CARTB='beta_carotene', THIA='thiamin', RIBF='riboflavin',
               NIA='niacin', VITC='vitamin_c')
stopifnot(all(names(asean_map) %in% names(asean_raw)))

# printed-notation parser; anything unrecognised stops the script
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

lk_as <- read_csv(file.path(wk_dir, 'data/linking_susenas_asean.csv'),
                  col_types = cols(.default = col_character()), progress = FALSE)
ase <- asean_raw %>% filter(food_id %in% lk_as$asean_code)
stopifnot(nrow(ase) == nrow(lk_as))               # every code found once
stopifnot(all(ase$row_status == 'primary'))       # no duplicate printings

# every pooled code must be per 100 ml with a usable density (true of all 11;
# the guard protects against a future edit to the pools)
den <- suppressWarnings(as.numeric(ase$DEN))
if (any(is.na(den))) stop('ASEAN code(s) without density: ',
                          paste(ase$food_id[is.na(den)], collapse = ', '))

ase_std <- ase['food_id']
for (rawcol in names(asean_map))
  ase_std[[asean_map[rawcol]]] <- parse_asean(ase[[rawcol]]) / den
cat('ASEAN:', nrow(ase_std), 'rows parsed and converted per 100 ml -> per 100 g\n')

###################################################################################################################
## 4. Assemble the item table

lk_tk <- read_csv(file.path(wk_dir, 'data/linking_susenas_tkpi.csv'),
                  col_types = cols(.default = col_character()), progress = FALSE)

# every pooled TKPI code must exist in the TKPI table
pool_codes <- unique(na.omit(lk_tk$tkpi_code[lk_tk$tkpi_code != '']))
missing <- setdiff(pool_codes, tkpi$KODE)
if (length(missing)) stop('TKPI codes not found: ', paste(missing, collapse = ', '))

# item universe = TKPI linking file (minus tobacco) + SMILING + ASEAN items
items_tk <- lk_tk %>%
  filter(food_group != 'ROKOK DAN TEMBAKAU') %>%
  group_by(susenas_code) %>%
  summarise(coicop_code = first(coicop_code),
            susenas_item = first(susenas_item),
            food_group  = first(food_group),
            codes = paste(tkpi_code[!is.na(tkpi_code) & tkpi_code != ''],
                          collapse = '; '),
            n_codes = sum(!is.na(tkpi_code) & tkpi_code != ''),
            .groups = 'drop') %>%
  mutate(pool = 'tkpi')
items_sm <- lk_sm %>% group_by(susenas_code) %>%
  summarise(coicop_code = first(coicop_code),
            susenas_item = first(susenas_item),
            food_group  = first(food_group),
            codes = paste(smiling_code, collapse = '; '),
            n_codes = n(), .groups = 'drop') %>%
  mutate(pool = 'smiling')
items_as <- lk_as %>% group_by(susenas_code) %>%
  summarise(coicop_code = first(coicop_code),
            susenas_item = first(susenas_item),
            food_group  = first(food_group),
            codes = paste(asean_code, collapse = '; '),
            n_codes = n(), .groups = 'drop') %>%
  mutate(pool = 'asean')

items <- bind_rows(items_tk, items_sm, items_as) %>%
  mutate(susenas_code = as.numeric(susenas_code)) %>%
  arrange(susenas_code)
stopifnot(!any(duplicated(items$susenas_code)))   # no item in two pools
cat('Item universe:', nrow(items), 'items (expect 146)\n')

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

rebuilt <- data.frame(
  coicop_code  = ifelse(is.na(items$coicop_code), '',
                        str_trim(items$coicop_code)),
  susenas_code = items$susenas_code,
  susenas_item = items$susenas_item,
  food_group   = items$food_group,
  source       = src,
  source_codes = src_codes,
  check.names = FALSE, stringsAsFactors = FALSE)
rebuilt <- cbind(rebuilt, as.data.frame(vals))

stopifnot(dir.exists(out_dir))
out_path <- file.path(out_dir, 'indonesia_fct_nonaquatic_food.csv')
write.csv(rebuilt, out_path, row.names = FALSE, fileEncoding = 'UTF-8', na = '')
cat('\nWrote ', out_path, '  (', nrow(rebuilt), ' x ', ncol(rebuilt), ')\n', sep = '')
print(table(rebuilt$source))

###################################################################################################################
## 5. Cell-by-cell comparison against the published table

pub <- read_csv(file.path(wk_dir, 'data/indonesia_fct_nonaquatic_food.csv'),
                show_col_types = FALSE, progress = FALSE)
cat('\n---- rebuilt vs published ----\n')
cat('rows:', nrow(pub), 'published /', nrow(rebuilt), 'rebuilt;  columns identical:',
    identical(names(pub), names(rebuilt)), '\n')
stopifnot(nrow(pub) == nrow(rebuilt),
          all(pub$susenas_code == rebuilt$susenas_code))

n_bad <- 0
for (cn in names(pub)) {
  a <- pub[[cn]]; b <- rebuilt[[cn]]
  if (is.numeric(a) || is.numeric(b)) {
    a <- suppressWarnings(as.numeric(a)); b <- suppressWarnings(as.numeric(b))
    diff <- !((is.na(a) & is.na(b)) |
                (!is.na(a) & !is.na(b) & abs(a - b) <= 1e-9))
  } else {
    a <- ifelse(is.na(a), '', str_trim(a)); b <- ifelse(is.na(b), '', str_trim(b))
    diff <- a != b
  }
  if (any(diff)) {
    n_bad <- n_bad + sum(diff)
    cat(sprintf('  %-16s %3d cells differ, items: %s\n', cn, sum(diff),
                paste(head(pub$susenas_code[diff], 8), collapse = ', ')))
  }
}
if (n_bad == 0) {
  cat('  ALL ', ncol(pub), ' columns identical (numeric tolerance 1e-9,',
      ' blanks match blanks).\n', sep = '')
} else cat('  TOTAL differing cells:', n_bad, '\n')
#
###################################################################################################################
###################################################################################################################
#
# This step 2 builds the aquatic Indonesia food composition table
# (indonesia_fct_aquatic_food.csv) from the Harvard AFCD fish book and the
# SUSENAS linking file, adds the IKAN category header (SUSENAS item 16, previously not added).
# It also writes the linking file with the item 16 header row added.
#
###################################################################################################################
#
# Name: Farid Annam
# Affiliation: Harvard T.H. Chan School of Public Health
# For: north_maluku
# Date updated: 8/27/2026
###################################################################################################################
# Method:
#  - Every SUSENAS item takes the plain mean of its linked fish-book rows,
#    the same rule the non-aquatic table uses for its code pools. 33 items
#    link to exactly one row (including 33 Ikan segar/basah lainnya and 41
#    Tongkol group diawetkan, which use their dedicated fish-book entries)
#    and are that row at full precision. Two items pool several rows:
#      18 Tongkol group (fresh) = mean of Tongkol, Tuna, Cakalang/dencis
#      26 Mas, nila             = mean of Mas, Nila
#    The 2020 survey publishes no species split below province level, so the
#    members enter with equal shares; consumption is not used in this table.
#  - A nutrient missing for a member is excluded from that nutrient's mean,
#    never treated as zero.
#  - Item 16 "IKAN" is the fish category header: one row with blank
#    nutrients, mirroring the category headers of the non-aquatic table.
# The script stops on any unexpected condition instead of passing it through.

# clear workspace
rm(list = ls())
graphics.off()

# libraries
library(tidyverse)

# directories
wk_dir  <- '~/Documents/Github/north_maluku'
out_dir <- '~/Desktop/indonesia_fct/datasets/2_process'

# AFCD column -> standard column name (the 32 nutrients, in published order)
afcd_map <- c(VitaminA='vitamin_a', Thiamin='thiamin', Riboflavin='riboflavin',
              Niacin='niacin', VitaminB6='vitamin_b6', Folate='folate',
              VitaminB12='vitamin_b12', VitaminC='vitamin_c',
              VitaminD='vitamin_d', VitaminE='vitamin_e', Calcium='calcium',
              Chromium='chromium', Copper='copper', Iodine='iodine',
              Iron='iron', Magnesium='magnesium', Manganese='manganese',
              Phosphorus='phosphorus', Potassium='potassium',
              Selenium='selenium', Sodium='sodium', Zinc='zinc',
              Protein='protein', Leucine='leucine', Lysine='lysine',
              Methionine='methionine', Phenylalanine='phenylalanine',
              Threonine='threonine', Tryptophan='tryptophan', Valine='valine',
              ALA='ala', DHAEPA='dha_epa')
nutr <- unname(afcd_map)

###################################################################################################################
## 1. Linking file - 35 items, 38 linked fish, plus the item 16 header

lk <- read_csv(file.path(wk_dir, 'data/linking_susenas_afcd.csv'),
               col_types = cols(.default = col_character()), progress = FALSE)

# the linking file may or may not already carry the item 16 header row;
# separate it from the fish links either way
header16 <- lk %>% filter(as.numeric(susenas_code) == 16)
lk       <- lk %>% filter(as.numeric(susenas_code) != 16)
if (nrow(header16) == 0) {
  # the IKAN category header, blank except code, item and group
  header16 <- lk[0, ]
  header16[1, c('susenas_code', 'susenas_item', 'food_group')] <-
    list('16', 'IKAN', 'IKAN')
}
stopifnot(nrow(header16) == 1,
          header16$susenas_item == 'IKAN', header16$food_group == 'IKAN')

n_fish <- lk %>% count(susenas_code, susenas_item)
pooled <- n_fish$susenas_item[n_fish$n > 1]
cat('Linking file:', nrow(lk), 'fish across', nrow(n_fish), 'items;',
    length(pooled), 'pooled items (equal-mean):',
    paste(pooled, collapse = ' | '), '\n')

lk_out <- bind_rows(header16, lk) %>%
  arrange(as.numeric(susenas_code))

###################################################################################################################
## 2. Values - the plain mean of each item's linked fish-book rows

# the fish book's first column is an unnamed row index; name it quietly
fish <- read_csv(file.path(wk_dir, 'data/fish_book_afcd.csv'),
                 show_col_types = FALSE, progress = FALSE,
                 name_repair = 'unique_quiet')
stopifnot(all(names(afcd_map) %in% names(fish)))
names(fish)[match(names(afcd_map), names(fish))] <- nutr

# every linked fish name must match exactly one fish-book row (by local name)
n_hits <- vapply(lk$food_name,
                 function(z) sum(fish$bahasa == z, na.rm = TRUE), numeric(1))
if (any(n_hits != 1))
  stop('fish name(s) without a unique fish-book match: ',
       paste(unique(lk$food_name[n_hits != 1]), collapse = ' | '))

# mean over the linked rows; a member missing a nutrient is excluded from
# that nutrient's mean (an item linked to one row is simply that row)
vals <- lk %>%
  select(susenas_item, food_name) %>%
  left_join(fish, by = c('food_name' = 'bahasa')) %>%
  group_by(susenas_item) %>%
  summarise(across(all_of(nutr), ~ {
    v <- .x[!is.na(.x)]
    if (length(v) == 0) NA_real_ else mean(v)
  }), .groups = 'drop')
stopifnot(nrow(vals) == nrow(n_fish))
cat('Items computed:', nrow(vals), '\n')

###################################################################################################################
## 3. Assemble the table - header row 16 + 35 items

keys <- lk_out %>% distinct(coicop_code, susenas_code, susenas_item, food_group)
stopifnot(nrow(keys) == nrow(n_fish) + 1)   # 35 items + the header

aquatic <- keys %>%
  mutate(susenas_code = as.numeric(susenas_code)) %>%
  left_join(vals, by = 'susenas_item') %>%
  arrange(susenas_code)
stopifnot(ncol(aquatic) == 4 + length(nutr))

stopifnot(dir.exists(out_dir))
out_path <- file.path(out_dir, 'indonesia_fct_aquatic_food.csv')
write.csv(aquatic, out_path, row.names = FALSE, fileEncoding = 'UTF-8', na = '')
cat('Wrote ', out_path, '  (', nrow(aquatic), ' x ', ncol(aquatic), ')\n', sep = '')

###################################################################################################################
## 4. Cell-by-cell comparison against the published table
##
## Expected against the previous published version: items 18, 26 and 33
## differ (the pooled items moved from consumption-weighted district mixes
## to the rules above); items 41 and 51 differ in susenas_item only (the
## VSEN20.KP renaming); everything else is identical.

pub <- read_csv(file.path(wk_dir, 'data/indonesia_fct_aquatic_food.csv'),
                show_col_types = FALSE, progress = FALSE)
cat('\n---- rebuilt vs published ----\n')
cat('rows:', nrow(pub), 'published /', nrow(aquatic), 'rebuilt\n')
new_rows <- setdiff(aquatic$susenas_code, pub$susenas_code)
if (length(new_rows)) cat('rows not in published:',
                          paste(new_rows, collapse = ', '), '\n')

both <- inner_join(pub, aquatic, by = 'susenas_code',
                   suffix = c('_pub', '_new'))
n_bad <- 0
for (cn in setdiff(names(pub), 'susenas_code')) {
  a <- both[[paste0(cn, '_pub')]]; b <- both[[paste0(cn, '_new')]]
  if (is.numeric(a) || is.numeric(b)) {
    a <- suppressWarnings(as.numeric(a)); b <- suppressWarnings(as.numeric(b))
    diff <- !((is.na(a) & is.na(b)) |
                (!is.na(a) & !is.na(b) & abs(a - b) <= 1e-9))
  } else {
    a <- ifelse(is.na(a), '', str_trim(a)); b <- ifelse(is.na(b), '', str_trim(b))
    diff <- a != b
  }
  if (any(diff)) {
    n_bad <- n_bad + sum(diff)
    cat(sprintf('  %-14s %2d cells differ, items: %s\n', cn, sum(diff),
                paste(both$susenas_code[diff], collapse = ', ')))
  }
}
if (n_bad == 0) {
  cat('  all shared rows identical on all columns.\n')
} else {
  expected <- all(unlist(lapply(setdiff(names(pub), 'susenas_code'), function(cn) {
    a <- both[[paste0(cn, '_pub')]]; b <- both[[paste0(cn, '_new')]]
    if (is.numeric(a) || is.numeric(b)) {
      a <- suppressWarnings(as.numeric(a)); b <- suppressWarnings(as.numeric(b))
      d <- !((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) <= 1e-9))
      all(both$susenas_code[d] %in% c(18, 26, 33))
    } else {
      a <- ifelse(is.na(a), '', str_trim(a)); b <- ifelse(is.na(b), '', str_trim(b))
      all(both$susenas_code[a != b] %in% c(18, 26, 33, 41, 51))
    }
  })))
  cat('  differing cells:', n_bad,
      if (expected) '- all confined to the pooled items (18, 26, 33) and the renamed labels (41, 51), as expected.\n'
      else '- WARNING: differences outside the expected items!\n')
}
###################################################################################################################
###################################################################################################################
#
# This step 3 fill the SUSENAS nutrient gaps: 14 items with no or partial match get
# values built from ingredient-level recipes (check susenas_unmatched_gaps.xlsx)
# We are thankful for Sappho's input on how to approach those 14 unmatched items
#
# Stage 1 defines the recipes (one row per ingredient, with its share of
# 100 g of the finished dish) and the captured nutrient values: USDA and
# MyFCD (Malaysia) entries per 100 g, and bottled mineral water for every added-water
# row (composition captured from the product's nutrition facts, per
# 100 ml: calcium 0.914, sodium 3.5, potassium 0.3 mg). It writes the
# working table usda_myfcd_nut_values.csv.
#
# Stage 2 looks up the TKPI, SMILING and aquatic ingredients, then
# computes each item: value = sum(share/100 x ingredient value).
# Ingredients missing a nutrient count as zero, so the value is a minimum
# where a source is incomplete; it is blank only when no ingredient
# reports that nutrient. Item 173 'siomay, batagor' has a 'tenggiri' fish ingredient,
# so fish nutrient values is borrowed from Harvard AFCD for that specific ingredient.

library(readr)
library(dplyr)
library(tibble)

wk_dir  <- "~/Documents/Github/north_maluku"                    # inputs
out_dir <- "~/Desktop/indonesia_fct/datasets/2_process"         # outputs

nuts <- c("water","energy","protein","fat","carbohydrate","fiber","ash",
          "calcium","phosphorus","iron","sodium","potassium","copper","zinc",
          "retinol","beta_carotene","total_carotene","thiamin","riboflavin",
          "niacin","vitamin_c")

## ---- stage 1: recipes and captured nutrient values --------------------

# named nutrient vector, unnamed nutrients stay blank
val <- function(...) {
  x <- c(...)
  out <- setNames(rep(NA_real_, length(nuts)), nuts)
  out[names(x)] <- x
  out
}

# per 100 g; USDA = fdc.nal.usda.gov by FDC id, MyFCD = myfcd.moh.gov.my
# by NDB code, both captured 20-22 Aug 2026; "water" = bottled mineral
# water from the product's nutrition facts
external <- list(
  "2088673" = val(energy = 500.0, protein = 20.0, fat = 20.0, carbohydrate = 60.0, fiber = 40.0, calcium = 200.0, iron = 7.2, sodium = 100.0),
  "746775" = val(water = 0.42, energy = 0.00, protein = 0.00, fat = 0.00, carbohydrate = 0.00, fiber = 0.00, ash = 99.00, calcium = 50.00, phosphorus = 0.00, iron = 0.00, sodium = 38700.00, potassium = 2.00, copper = 0.00, zinc = 0.00, retinol = 0.00, beta_carotene = 0.00, total_carotene = 0.00, thiamin = 0.00, riboflavin = 0.00, niacin = 0.00, vitamin_c = 0.00),
  "2449440" = val(energy = 0, protein = 0, fat = 0, carbohydrate = 0, fiber = 0, sodium = 12500),
  "2032559" = val(energy = 0, protein = 0, fat = 0, carbohydrate = 0, fiber = 0, sodium = 12500),
  "2032322" = val(energy = 200.0, protein = 10.0, fat = 5.0, carbohydrate = 20.0, fiber = 10.0, calcium = 0.0, iron = 10.8, sodium = 7300.0),
  "2032236" = val(energy = 333.3, protein = 11.1, fat = 22.2, carbohydrate = 33.3, fiber = 11.1, calcium = 0.0, iron = 0.0, sodium = 4444.0),
  "2032401" = val(energy = 222.2, protein = 11.1, fat = 11.1, carbohydrate = 22.2, fiber = 0.0, calcium = 0.0, iron = 0.0, sodium = 5333.0),
  "2032198" = val(energy = 235.30, protein = 5.90, fat = 5.90, carbohydrate = 41.20, fiber = 5.90, calcium = 235.30, iron = 4.24, sodium = 6058.80),
  "2032292" = val(energy = 260.90, protein = 8.70, fat = 13.00, carbohydrate = 26.10, fiber = 17.40, calcium = 0.00, iron = 3.13, sodium = 4443.50),
  "water" = val(water = 100.000, energy = 0.000, protein = 0.000, fat = 0.000, carbohydrate = 0.000, fiber = 0.000, ash = 0.000, calcium = 0.914, phosphorus = 0.000, iron = 0.000, sodium = 3.500, potassium = 0.300, copper = 0.000, zinc = 0.000, retinol = 0.000, beta_carotene = 0.000, total_carotene = 0.000, thiamin = 0.000, riboflavin = 0.000, niacin = 0.000, vitamin_c = 0.000),
  "222016" = val(water = 52.50, energy = 243.00, protein = 18.90, fat = 12.90, carbohydrate = 12.70, fiber = 1.90, ash = 1.10, calcium = 29.00, phosphorus = 335.00, iron = 2.90, sodium = 135.00, potassium = 221.00, retinol = 84.00, total_carotene = 0.00, thiamin = 0.03, riboflavin = 0.12, niacin = 6.70, vitamin_c = 0.00),
  "222006" = val(water = 46.70, energy = 240.00, protein = 20.40, fat = 7.80, carbohydrate = 22.10, fiber = 0.30, ash = 2.70, calcium = 17.00, phosphorus = 462.00, iron = 3.90, sodium = 92.00, potassium = 202.00, retinol = 72.00, total_carotene = 0.00, thiamin = 0.13, riboflavin = 0.22, niacin = 5.30, vitamin_c = 0.00),
  "236003" = val(water = 53.10, energy = 211.00, protein = 41.70, fat = 4.90, carbohydrate = 0.00, fiber = 0.00, ash = 1.60, calcium = 44.00, phosphorus = 412.00, iron = 2.70, sodium = 113.00, potassium = 234.00, retinol = 5.00, total_carotene = 14.00, thiamin = 0.13, riboflavin = 0.19, niacin = 5.50, vitamin_c = 0.50),
  "172112" = val(water = 51.900, energy = 261.000, protein = 14.400, fat = 15.400, carbohydrate = 16.200, fiber = 0.700, ash = 2.090, calcium = 38.000, phosphorus = 213.000, iron = 1.430, sodium = 538.000, potassium = 281.000, copper = 0.139, zinc = 0.750, retinol = 16.000, beta_carotene = 0.000, thiamin = 0.106, riboflavin = 0.055, niacin = 6.740, vitamin_c = 0.000),
  "1852651" = val(energy = 158.00, protein = 5.26, fat = 2.63, carbohydrate = 22.80, fiber = 1.80, calcium = 0.00, iron = 0.63, sodium = 421.00),
  "498708" = val(energy = 42.9, protein = 0.0, fat = 0.0, carbohydrate = 10.7, fiber = 0.0, calcium = 13.0, sodium = 25.0),
  "168746" = val(water = 92.000, energy = 43.000, protein = 0.460, fat = 0.000, carbohydrate = 3.550, fiber = 0.000, ash = 0.160, calcium = 4.000, phosphorus = 14.000, iron = 0.020, sodium = 4.000, potassium = 27.000, copper = 0.005, zinc = 0.010, retinol = 0.000, beta_carotene = 0.000, total_carotene = 0.000, thiamin = 0.005, riboflavin = 0.025, niacin = 0.513, vitamin_c = 0.000),
  "173664" = val(water = 57.500, energy = 295.000, protein = 0.000, fat = 0.000, carbohydrate = 0.000, fiber = 0.000, ash = 0.010, calcium = 0.000, phosphorus = 4.000, iron = 0.040, sodium = 1.000, potassium = 2.000, copper = 0.021, zinc = 0.040, retinol = 0.000, beta_carotene = 0.000, thiamin = 0.006, riboflavin = 0.004, niacin = 0.013, vitamin_c = 0.000)
)

# one row per ingredient; proportion_pct sums to 100 within each item
recipes <- tribble(
  ~kode_susenas, ~ingredient, ~proportion_pct, ~source, ~source_code, ~note,
  "130", "Pure Brazilian ground coffee (Cafe Najjar)", 100, "usda", "2088673", "dry roasted grounds, matching the dry form recorded in SUSENAS; no FCD carries plain roasted-ground arabica or robusta, and Brazil grows both species like Indonesia. Label is per 5 g of powder (scaled x20, so rounding is coarse); potassium, phosphorus and vitamins are not declared and stay blank",
  "134", "Salt, table, iodized", 100, "usda", "746775", "proxy for Garam Cap Kapal; iodine 5,080 mcg/100 g = 50.8 ppm, inside the SNI 30-80 ppm KIO3 range; organics zero by composition",
  "141", "MSG, monosodium glutamate (Smart & Final)", 50, "usda", "2449440", "label panel only; unreported micronutrients blank",
  "141", "MSG, umami seasoning (Ajinomoto USA)", 50, "usda", "2032559", "",
  "144", "Bumbu soto daging (Bamboe Indonesia)", 20, "usda", "2032322", "Indonesian bumbu sold in the US; shallot-garlic-turmeric-galangal spice mix",
  "144", "Bumbu kare instant seasoning (Indofood)", 20, "usda", "2032236", "coconut-milk curry seasoning paste",
  "144", "Bumbu sambal goreng instant seasoning (Indofood)", 20, "usda", "2032401", "chili-coconut seasoning paste",
  "144", "Bumbu semur instant spices (Bamboe Indonesia)", 20, "usda", "2032198", "sweet soy-nutmeg braise mix",
  "144", "Bumbu rujak instant spices (Bamboe Indonesia)", 20, "usda", "2032292", "turmeric-chili-coriander mix",
  "158", "Ketoprak", 50, "tkpi", "AP060", "ketoprak and gado-gado averaged as representative; no source carries a pecel entry",
  "158", "Gado-gado", 50, "tkpi", "DP031", "",
  "162", "Lontong (rice cake)", 40.5, "smiling", "63", "",
  "162", "Santan (dengan air)", 18, "tkpi", "KP003", "coconut soup base",
  "162", "Telur ayam rebus (satu butir)", 10, "tkpi", "HR002", "one boiled egg, about 50 g edible, in a 500 g portion; no source carries a boiled-egg entry, so the fresh-egg values stand in (boiling changes them very little)",
  "162", "Labu siam, segar", 7.2, "tkpi", "DR123", "",
  "162", "Kacang panjang, segar", 3.6, "tkpi", "DR097", "",
  "162", "Nangka muda, segar", 3.6, "tkpi", "DR130", "",
  "162", "Minyak kelapa sawit", 2.25, "tkpi", "KR012", "",
  "162", "Bawang merah, segar", 2.25, "tkpi", "NR007", "bumbu",
  "162", "Bawang putih, segar", 0.9, "tkpi", "NR008", "bumbu",
  "162", "Cabai merah, segar", 0.9, "tkpi", "NR014", "bumbu",
  "162", "Kemiri", 1.35, "tkpi", "NR019", "bumbu",
  "162", "Kunyit, segar", 0.27, "tkpi", "NR022", "bumbu",
  "162", "Lengkuas, segar", 0.27, "tkpi", "NR010", "bumbu",
  "162", "Garam (iodized salt)", 0.36, "usda", "746775", "",
  "162", "Air (soup water)", 8.55, "water", "-", "mineral water, composition as item 175",
  "165", "Chicken satay (MyFCD)", 16.66666667, "myfcd", "222016", "the three satays average to the sate half",
  "165", "Beef satay (MyFCD)", 16.66666667, "myfcd", "222006", "",
  "165", "Mutton satay (MyFCD)", 16.66666667, "myfcd", "236003", "",
  "165", "Tongseng: Kambing, daging, segar", 15, "tkpi", "FR019", "",
  "165", "Tongseng: Daun kubis, segar", 5, "tkpi", "DR044", "",
  "165", "Tongseng: Tomat merah, segar", 2.5, "tkpi", "DR161", "",
  "165", "Tongseng: Santan (dengan air)", 7.5, "tkpi", "KP003", "",
  "165", "Tongseng: Kecap", 2.5, "tkpi", "NP005", "",
  "165", "Tongseng: Minyak kelapa sawit", 1.5, "tkpi", "KR012", "",
  "165", "Tongseng: Gula kelapa", 0.5, "tkpi", "MP006", "bumbu",
  "165", "Tongseng: Bawang merah, segar", 1.5, "tkpi", "NR007", "bumbu",
  "165", "Tongseng: Bawang putih, segar", 0.75, "tkpi", "NR008", "bumbu",
  "165", "Tongseng: Cabai rawit, segar", 0.5, "tkpi", "NR015", "bumbu",
  "165", "Tongseng: Jahe, segar", 0.25, "tkpi", "NR018", "bumbu",
  "165", "Tongseng: Lengkuas, segar", 0.25, "tkpi", "NR010", "bumbu",
  "165", "Tongseng: Garam (iodized salt)", 0.25, "usda", "746775", "",
  "165", "Tongseng: Air (broth)", 12, "water", "-", "mineral water, composition as item 175",
  "171", "Sapi, daging, asap", 20, "tkpi", "FP013", "",
  "171", "Sapi, daging, kornet", 20, "tkpi", "FP015", "",
  "171", "Sapi, sosis", 20, "tkpi", "FP016", "",
  "171", "Sapi, hati, sosis", 20, "tkpi", "FP017", "",
  "171", "Chicken nuggets, white meat, frozen, precooked", 20, "usda", "172112", "adds the processed-chicken side of the item",
  "172", "Beras giling, mentah", 14.25, "tkpi", "AR001", "rice-to-water 1:5 (porridge)",
  "172", "Air (cooking water)", 71.25, "water", "-", "mineral water, composition as item 175",
  "172", "Ayam, daging, segar (edible flesh)", 7, "tkpi", "FR005", "proportion refers to flesh added; bdd not reapplied",
  "172", "Minyak kelapa sawit", 1.5, "tkpi", "KR012", "",
  "172", "Kecap", 1, "tkpi", "NP005", "",
  "172", "Kerupuk udang goreng", 2, "tkpi", "BP045", "",
  "172", "Seledri, segar", 1, "tkpi", "DR147", "",
  "172", "Garam (iodized salt)", 1, "usda", "746775", "",
  "172", "Jahe, segar", 1, "tkpi", "NR018", "bumbu",
  "173", "Ikan tenggiri, segar (AFCD raw tenggiri)", 22, "aquatic", "19", "main fish component, from the aquatic source. AFCD provides protein, calcium, phosphorus, iron, sodium, potassium, copper, zinc, thiamin, riboflavin, niacin and vitamin C for this table; it carries no water, energy, fat, carbohydrate, fiber or ash, so this row contributes zero to those columns and the item averages are minimums there",
  "173", "Tepung tapioka", 18, "tkpi", "BP070", "",
  "173", "Tahu, mentah", 15, "tkpi", "CP061", "main 2",
  "173", "Kentang, segar", 10, "tkpi", "BR013", "",
  "173", "Daun kubis, segar", 6, "tkpi", "DR044", "",
  "173", "Telur ayam ras, segar", 4, "tkpi", "HR002", "binder",
  "173", "Bawang putih, segar", 1, "tkpi", "NR008", "bumbu",
  "173", "Minyak kelapa sawit (frying, batagor)", 5, "tkpi", "KR012", "",
  "173", "Peanut satay sauce (Thai Wonder)", 10, "usda", "1852651", "peanut dipping sauce",
  "173", "Garam (iodized salt)", 0.5, "usda", "746775", "",
  "173", "Air (steaming loss margin)", 8.5, "water", "-", "mineral water, composition as item 175",
  "175", "Water, bottled mineral (Le Minerale as reference brand)", 100, "water", "-", "naturally occurring minerals, captured from the product's nutrition facts (mg/L): Ca 9.14, Mg 5.87, Na 35.0, K 3.0, nitrate 1.55, bicarbonate 118.0, sulfate 3.06, chloride <0.01, TDS 177, pH 7.2-7.7. Only calcium, sodium and potassium have columns in this panel; the leading brand (Aqua) names its minerals but publishes no amounts, so the second brand's table stands in",
  "176", "Water, bottled mineral (19 L; Le Minerale as reference brand)", 100, "water", "-", "same composition as item 175",
  "181", "Es cincau: Grass jelly drink with honey (Hong Van)", 19.33333333, "usda", "498708", "no cincau entry in any FCD - a canned grass jelly drink stands in; values per 100 ml, incl. its sugar and water",
  "181", "Es cincau: Santan (dengan air)", 3.333333333, "tkpi", "KP003", "",
  "181", "Es cincau: Gula kelapa", 2.666666667, "tkpi", "MP006", "",
  "181", "Es cincau: Nangka masak pohon, segar", 1.333333333, "tkpi", "ER071", "",
  "181", "Es cincau: Air / es", 6.666666667, "water", "-", "mineral water, composition as item 175",
  "181", "Es campur: Air / es", 7, "water", "-", "mineral water, composition as item 175",
  "181", "Es campur: Kelapa muda, air, segar", 5, "tkpi", "QR001", "",
  "181", "Es campur: Kelapa muda, daging, segar", 4, "tkpi", "ER046", "",
  "181", "Es campur: Susu kental manis", 2.666666667, "tkpi", "JP007", "",
  "181", "Es campur: Sirup", 2, "tkpi", "MP014", "",
  "181", "Es campur: Mangga, segar", 3.333333333, "tkpi", "ER054", "",
  "181", "Es campur: Nanas, segar", 3.333333333, "tkpi", "ER070", "",
  "181", "Es campur: Nangka masak pohon, segar", 2, "tkpi", "ER071", "",
  "181", "Es campur: Alpukat, segar", 4, "tkpi", "ER001", "",
  "181", "Es kacang merah: Air / es", 13.33333333, "water", "-", "mineral water, composition as item 175",
  "181", "Es kacang merah: Kacang merah segar, rebus", 6.666666667, "tkpi", "CP009", "boiled fresh red beans",
  "181", "Es kacang merah: Susu kental manis", 3.333333333, "tkpi", "JP007", "",
  "181", "Es kacang merah: Kelapa muda, daging, segar", 1.666666667, "tkpi", "ER046", "",
  "181", "Es kacang merah: Kelapa muda, air, segar", 2.666666667, "tkpi", "QR001", "",
  "181", "Es kacang merah: Sirsak, segar", 2.333333333, "tkpi", "ER106", "",
  "181", "Es kacang merah: Nangka masak pohon, segar", 1.333333333, "tkpi", "ER071", "",
  "181", "Es kacang merah: Alpukat, segar", 2, "tkpi", "ER001", "",
  "182", "Beer, regular, all (Bir Bintang proxy)", 50, "usda", "168746", "",
  "182", "Distilled spirits, 100 proof (arak/sopi proxy)", 50, "usda", "173664", "50% ABV matches arak. North Maluku's own spirit is sopi (distilled aren/palm sap) - no FCD entry exists anywhere, this is its stand-in"
)

ext_tbl <- bind_rows(lapply(names(external), function(k)
  bind_cols(tibble(key = k), as_tibble(as.list(external[[k]])))))

usda_myfcd_nut_values <- recipes %>%
  mutate(key = ifelse(source == "water", "water", source_code)) %>%
  left_join(ext_tbl, by = "key") %>%      # non-external rows stay blank
  select(-key) %>%
  mutate(dha_epa = NA_real_)

write_csv(usda_myfcd_nut_values, file.path(out_dir, "usda_myfcd_nut_values.csv"),
          na = "")

## ---- stage 2: look up local sources and compute the items --------------

nonaquatic <- read_csv(file.path(wk_dir, "data/indonesia_fct_nonaquatic_food.csv"))
aquatic    <- read_csv(file.path(wk_dir, "data/indonesia_fct_aquatic_food.csv"))

# TKPI, renamed to the standard columns. One KAROTEN_TOTAL value is
# written "1,480" and needs its comma removed before it converts.
tkpi <- read_csv(file.path(wk_dir, "data/tkpi_2020.csv")) %>%
  mutate(KAROTEN_TOTAL = as.numeric(gsub(",", "", KAROTEN_TOTAL))) %>%
  select(source_code = KODE,
         water = AIR, energy = ENERGI, protein = PROTEIN, fat = LEMAK,
         carbohydrate = KH, fiber = SERAT, ash = ABU, calcium = KALSIUM,
         phosphorus = FOSFOR, iron = BESI, sodium = NATRIUM,
         potassium = KALIUM, copper = TEMBAGA, zinc = SENG,
         retinol = RETINOL, beta_carotene = BETA_KAROTEN,
         total_carotene = KAROTEN_TOTAL, thiamin = THIAMIN,
         riboflavin = RIBOFLAVIN, niacin = NIASIN, vitamin_c = VITAMIN_C)

# SMILING carries fewer nutrients; the ones it lacks stay blank.
smiling <- read_csv(file.path(wk_dir, "data/smiling_2013.csv")) %>%
  select(source_code = smiling_row,
         water = water_g, energy = enerc_kcal, protein = protcnt_g,
         fat = fat_g, carbohydrate = choavldf_g, fiber = fibtg_g,
         ash = ash_g, calcium = ca_mg, iron = fe_mg, zinc = zn_mg,
         retinol = retol_mcg, beta_carotene = cartb_mcg,
         thiamin = thia_mg, riboflavin = ribf_mg, niacin = nia_mg,
         vitamin_c = vit_c_mg) %>%
  mutate(source_code = as.character(source_code))

# the aquatic table, keyed by SUSENAS code (tenggiri = 19)
aquatic_lk <- aquatic %>%
  mutate(source_code = as.character(susenas_code)) %>%
  select(source_code, any_of(c(nuts, "dha_epa")))

# fill the blank ingredient rows from their lookup table
fill_from <- function(rows, table) {
  rows %>%
    select(-any_of(c(nuts, "dha_epa"))) %>%
    left_join(table, by = "source_code")
}

ingredients <- bind_rows(
  usda_myfcd_nut_values %>% filter(source %in% c("usda", "myfcd", "water")),
  usda_myfcd_nut_values %>% filter(source == "tkpi")    %>% fill_from(tkpi),
  usda_myfcd_nut_values %>% filter(source == "smiling") %>% fill_from(smiling),
  usda_myfcd_nut_values %>% filter(source == "aquatic") %>% fill_from(aquatic_lk)
)

# every recipe must still sum to 100
stopifnot(ingredients %>% count(kode_susenas, wt = proportion_pct) %>%
            pull(n) %>% near(100) %>% all())

# weighted average per item; missing ingredient values count as zero
# (minimums), blank only when no ingredient reports the nutrient
gap <- ingredients %>%
  group_by(susenas_code = as.numeric(kode_susenas)) %>%
  summarise(
    across(all_of(nuts),
           ~ ifelse(all(is.na(.x)), NA_real_,
                    sum(proportion_pct / 100 * coalesce(.x, 0)))),
    dha_epa = sum(proportion_pct / 100 * coalesce(dha_epa, 0)),
    source_codes = paste(unique(paste0(source, ":", source_code)[source != "water"]),
                         collapse = "; "),
    .groups = "drop"
  )

# metadata from the published table; same columns + dha_epa at the end
indonesia_fct_gap_filling <- nonaquatic %>%
  select(coicop_code, susenas_code, susenas_item, food_group, bdd) %>%
  inner_join(gap, by = "susenas_code") %>%
  mutate(source = "gap_filling") %>%
  select(all_of(names(nonaquatic)), dha_epa)

print(indonesia_fct_gap_filling %>%
        select(susenas_code, susenas_item, energy, protein, sodium, dha_epa))
write_csv(indonesia_fct_gap_filling,
          file.path(out_dir, "indonesia_fct_gap_filling.csv"), na = "")
#
###################################################################################################################
###################################################################################################################
#
# This step 4 combines the three Indonesia food composition tables into one
# complete table covering every SUSENAS food item 1-182:
#   indonesia_fct_nonaquatic_food.csv  (TKPI panel; items outside 16-51)
#   indonesia_fct_aquatic_food.csv     (AFCD panel; items 16-51)
#   indonesia_fct_gap_filling.csv      (recipe-based values for 14 items,
#                                       replacing their non-aquatic rows)
#
# Name: Farid Annam
# Affiliation: Harvard T.H. Chan School of Public Health
# For: north_maluku
# Date updated: 8/25/2026
###################################################################################################################
# Method:
#  - The combiner performs NO arithmetic: every cell is either blank or one
#    value copied verbatim from exactly one input file.
#  - Each susenas_code appears exactly once: 132 non-aquatic + 14 gap-filling
#    (which REPLACE their non-aquatic rows) + 36 aquatic = 182.
#  - Columns: 6 identification + the TKPI panel (21 nutrients + bdd) + the 20
#    AFCD-only nutrients = 48. The 12 nutrients both panels carry share one
#    column each; no row has two sources, so values never collide.
#  - AFCD vitamin_a is NOT the same quantity as TKPI retinol/carotene and
#    keeps its own column. Aquatic rows leave the TKPI-only columns blank
#    (AFCD reports no proximates or edible portion); non-aquatic rows leave
#    the AFCD-only columns blank, except dha_epa on gap-filling rows.
#  - A final validation re-reads the inputs and stops unless every non-blank
#    cell equals its origin cell and every out-of-panel cell is blank.

# clear workspace
rm(list = ls())
graphics.off()

# libraries
library(tidyverse)

# directories
wk_dir  <- '~/Documents/Github/north_maluku'
out_dir <- '~/Desktop/indonesia_fct/datasets/2_process'

# column layout of the complete table
meta_cols <- c('coicop_code', 'susenas_code', 'susenas_item', 'food_group',
               'source', 'source_codes')
tkpi_cols <- c('water','energy','protein','fat','carbohydrate','fiber','ash',
               'calcium','phosphorus','iron','sodium','potassium','copper',
               'zinc','retinol','beta_carotene','total_carotene','thiamin',
               'riboflavin','niacin','vitamin_c','bdd')
afcd_only <- c('vitamin_a','vitamin_b6','folate','vitamin_b12','vitamin_d',
               'vitamin_e','chromium','iodine','magnesium','manganese',
               'selenium','leucine','lysine','methionine','phenylalanine',
               'threonine','tryptophan','valine','ala','dha_epa')
all_cols  <- c(meta_cols, tkpi_cols, afcd_only)

# the 12 nutrients carried by both panels (single shared columns)
shared <- c('protein','thiamin','riboflavin','niacin','vitamin_c','calcium',
            'phosphorus','iron','sodium','potassium','copper','zinc')

# lay a block into the full 48-column frame; columns it lacks stay blank
as_complete <- function(df) {
  for (cn in setdiff(all_cols, names(df))) df[[cn]] <- NA_real_
  df[all_cols]
}

###################################################################################################################
## 1. Read the three tables

nonaq <- read_csv(file.path(wk_dir, 'data/indonesia_fct_nonaquatic_food.csv'),
                  show_col_types = FALSE, progress = FALSE)
aqua  <- read_csv(file.path(wk_dir, 'data/indonesia_fct_aquatic_food.csv'),
                  show_col_types = FALSE, progress = FALSE)
gap   <- read_csv(file.path(wk_dir, 'data/indonesia_fct_gap_filling.csv'),
                  show_col_types = FALSE, progress = FALSE)
stopifnot(nrow(nonaq) == 146, nrow(aqua) == 36, nrow(gap) == 14)
stopifnot(identical(names(gap), c(names(nonaq), 'dha_epa')))
cat('Read: non-aquatic', nrow(nonaq), '| aquatic', nrow(aqua),
    '| gap filling', nrow(gap), '\n')

###################################################################################################################
## 2. Non-aquatic block - drop the 14 items the gap-filling table replaces

gap_items <- sort(gap$susenas_code)
stopifnot(all(gap_items %in% nonaq$susenas_code))
nonaq_kept <- nonaq %>% filter(!susenas_code %in% gap_items)
cat('Non-aquatic rows kept:', nrow(nonaq_kept),
    '(replaced by gap filling:', length(gap_items), ')\n')

###################################################################################################################
## 3. Aquatic block - assign source labels; source_codes stay blank

aqua <- aqua %>%
  mutate(source = ifelse(susenas_item == food_group, 'food_category', 'afcd'),
         source_codes = '')
stopifnot(sum(aqua$source == 'food_category') == 1)   # the IKAN header only

###################################################################################################################
## 4. Combine - a pure row-stack into the 48-column layout

complete <- bind_rows(as_complete(nonaq_kept),
                      as_complete(gap),
                      as_complete(aqua)) %>%
  arrange(susenas_code)

# every SUSENAS item 1-182 exactly once; 48 columns in the agreed order
stopifnot(nrow(complete) == 182,
          all(sort(complete$susenas_code) == 1:182),
          identical(names(complete), all_cols))

stopifnot(dir.exists(out_dir))
out_path <- file.path(out_dir, 'indonesia_fct_complete.csv')
# write_csv stores full round-trip precision, so every value survives the
# file exactly as it arrived from its source table
write_csv(complete, out_path, na = '')
cat('Wrote ', out_path, '  (', nrow(complete), ' x ', ncol(complete), ')\n', sep = '')
print(table(complete$source))

###################################################################################################################
## 5. Validation - every cell must equal its origin cell; no leakage between
##    panels. The script stops on the first failure.

check_block <- function(rows, src, own_cols) {
  # own_cols: the columns this source is allowed to fill; all others blank
  key <- intersect(c(meta_cols, own_cols), names(src))
  out <- complete %>% filter(susenas_code %in% rows) %>% arrange(susenas_code)
  ref <- src      %>% filter(susenas_code %in% rows) %>% arrange(susenas_code)
  for (cn in setdiff(key, 'source_codes')) {
    a <- out[[cn]]; b <- ref[[cn]]
    if (is.numeric(a) || is.numeric(b)) {
      a <- suppressWarnings(as.numeric(a)); b <- suppressWarnings(as.numeric(b))
      ok <- (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
    } else ok <- ifelse(is.na(a), '', a) == ifelse(is.na(b), '', b)
    if (!all(ok)) stop('value mismatch in ', cn, ' for item(s) ',
                       paste(out$susenas_code[!ok], collapse = ', '))
  }
  for (cn in setdiff(c(tkpi_cols, afcd_only), own_cols)) {
    bad <- !is.na(out[[cn]])
    if (any(bad)) stop('unexpected value in ', cn, ' for item(s) ',
                       paste(out$susenas_code[bad], collapse = ', '))
  }
}

aq_nutr <- c(shared, afcd_only)                       # the 32 AFCD nutrients
check_block(nonaq_kept$susenas_code, nonaq, tkpi_cols)
check_block(gap$susenas_code,        gap,   c(tkpi_cols, 'dha_epa'))
check_block(aqua$susenas_code,       aqua,  aq_nutr)
cat('Validation passed: every cell matches its origin file;',
    'no value outside its source panel.\n')
