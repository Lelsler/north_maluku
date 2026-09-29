# This file is used to combine SUSENAS consumption data with IDN food composition tables 
#
# Name: Laura G. Elsler, Farid Annam
# Affiliation: Harvard T.H. Chan School of Public Health
# Email: l.elsler@outlook.com
# For: north_maluku
# Date updated: 8/6/2026
###################################################################################################################

# libraries
library(readxl)
library(tidyverse)

# directories
wk_dir      <- '~/Dropbox/Harvard/nutrition_oriented/north_maluku/north_maluku'
data_dir <- '~/Dropbox/data'

## susenas consumption data
blok_41 <- read.dbf(file.path(data_dir,'susenas/blok41_51_94.dbf'))
aquatic_fct   <- read.csv('~/Dropbox/Harvard/nutrition_oriented/git/data/fish_book_afcd.csv')
NEW_afct   <- read.csv('~/Dropbox/Harvard/nutrition_oriented/git/data/fish_book_afcd.csv')

## food composition tables
final <- read.csv(file.path(wk_dir, 'data/blok41_fct_joined.csv'))

# afcd linking file
linking_afcd    <- read.csv(file.path(wk_dir, 'data/linking_susenas_afcd.csv'))

