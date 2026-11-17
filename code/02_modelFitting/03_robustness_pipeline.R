# 0 set up ----

## 0.1. packages ----

# data wrangling 
if(!require('tidyverse')) {
  install.packages('tidyverse', dependencies = TRUE)
}
if(!require('data.table')) {
  install.packages('data.table', dependencies = TRUE)
}
if(!require('fs')) {
  install.packages('fs', dependencies = TRUE)
}

# model fitting 
if(!require('matrixStats')) {
  install.packages('matrixStats', dependencies = TRUE)
}
if(!require('INLA')) {
  install.packages('INLA', dependencies = TRUE)
}
if(!require('sf')) {
  install.packages('sf', dependencies = TRUE)
}

# parallel
if(!require('future')) {
  install.packages('future', dependencies = TRUE)
}

## 0.2. directories ----

### 0.2.1. current ----

if(Sys.info()['sysname'] == 'Windows') {
  
  print('Running on Windows')
  code.path <- rstudioapi::getActiveDocumentContext()$path
  dir.home <- sub('(Policy_Evaluation/02_itsAnalysis).*', '\\1', code.path)
  
} else if (Sys.info()['sysname'] == 'Linux') {
  
  print('Running on Linux')
  dir.home <- '/mnt/sahsu/Projects/Policy_Evaluation/02_itsAnalysis'
  
} else {
  print('Unknown OS')
}

dir.code <- file.path(dir.home, 'code')
dir.data <- file.path(dir.home, 'data')
dir.res <- file.path(dir.home, 'results')

### 0.2.2 create ----

dir.res.main <- file.path(dir.res, '02_robustness')
fs::dir_create(dir.res.main)

dir.res.main.data <- file.path(dir.res.main, '01_data')
fs::dir_create(dir.res.main.data)

dir.res.main.mfit <- file.path(dir.res.main, '02_modelfit')
fs::dir_create(dir.res.main.mfit)

dir.res.main.res <- file.path(dir.res.main, '03_results')
fs::dir_create(dir.res.main.res)

## 0.3. imports ----

### 0.3.1. functions ----

source(file.path(dir.code, '00_functions.R'))

### 0.3.2. spatial ----

load(file.path(dir.data, 'organised', 'ENGLAND_SPATIAL_INFORMATION_LSOA_MSOA_LAD.rda'))
rm(info.lsoa)

poly.msoa <- 
  file.path(dir.data, 'spatial') %>% 
  sf::st_read(dsn = ., layer = 'ONS11_MSOA') %>%
  dplyr::filter(stringr::str_detect(MSOA11CD, '^E'))

poly.lad <- 
  file.path(dir.data, 'spatial') %>% 
  sf::st_read(dsn = ., layer = 'ONS11_LAD') %>%
  dplyr::filter(stringr::str_detect(LAD11CD, '^E'))

poly.rgn <-
  file.path(dir.data, 'spatial') %>% 
  sf::st_read(dsn = ., layer = 'ONS21_RGN') %>%
  dplyr::filter(stringr::str_detect(RGN21CD, '^E'))

lookup.lsoa.msoa <- 
  file.path(dir.data, 'spatial', 'OA11_LSOA11_MSOA11_LAD11_lookUp.csv') %>% 
  data.table::fread(file = .)

lookup.msoa.rgn <-  
  file.path(dir.data, 'spatial', 'MSOA11_LAD11_RGN11_lookUp.csv') %>% 
  data.table::fread(file = .)

# 1. data processing ----

file.path(dir.code, '03a_robustness_dataprocessing.R') %>%
  helper.cleanSource(filepath = .)

# 2. model fitting ----

file.path(dir.code, '03b_robustness_modelfitting.R') %>% 
  helper.cleanSource(filepath = .)

# 3. results generating ----

file.path(dir.code, '03c_robustness_modelresults.R') %>%
  helper.cleanSource(filepath = .)
