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
out_dir      <- '~/Dropbox/Harvard/nutrition_oriented/north_maluku/north_maluku'
data_dir <- '~/Dropbox/data'

link_file <- '~/Desktop/indonesia_fct/datasets/2_process/linking_susenas_afcd.csv'

## AFCD
fish_file   <- read.csv('~/Dropbox/Harvard/nutrition_oriented/git/data/fish_book_afcd.csv')
link_file <- read.csv(file.path(wk_dir, 'data/linking_susenas_afcd.csv')) # afcd linking file

fish_book_afcd_odd_values

## susenas consumption data
blok_41 <- read.dbf(file.path(data_dir,'susenas/blok41_51_94.dbf'))

## susenas + final food composition table 
final <- read.csv(file.path(wk_dir, 'data/blok41_fct_joined.csv'))



