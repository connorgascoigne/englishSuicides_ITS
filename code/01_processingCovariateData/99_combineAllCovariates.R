# 0. set up ----

## 0.1. librarys ----

# data wrangling
library(tidyverse)
library(data.table)
library(fs)

# spatial data
library(sf)

# model fitting 
library(INLA)

# parallel
library(future)
library(future.apply)

## 0.2. directories ---- 

### 0.2.1. current ----

# retrieve directories
if (Sys.info()['sysname'] == 'Windows') {
  
  print('Running on Windows')
  dir.path <- rstudioapi::getActiveDocumentContext()$path
  dir.home <- sub('(englishSuicides_ITS).*', '\\1', dir.path)
  n.cores <- round(future::availableCores()*0.5)
  plan(multisession, workers = n.cores)
  
} else if (Sys.info()['sysname'] == 'Linux') {
  
  print('Running on Linux')
  # retrieve directories
  dir.home <- '/rds/general/user/cgascoig/home/englishSuicides_ITS' # TERMINAL ON HPC
  n.cores <- future::availableCores()
  plan(multisession, workers = n.cores)
  data.table::setDTthreads(threads = n.cores)
  
} else {
  
  print('Unknown OS')
  
}

dir.code <- file.path(dir.home, 'code')
dir.data <- file.path(dir.home, 'data')
dir.res <- file.path(dir.home, 'results')

### 0.2.2. create ----

dir.data.org.final <- file.path(dir.data, 'organised', '99_final')
fs::dir_create(dir.data.org.final)

## 0.3. functions ----

source(file.path(dir.code, '00_functions.R'))

## 0.4. spatial ----

### 0.4.1. polygon ----

poly.msoa <- 
  file.path(dir.data, 'spatial', 'ons11_MSOA') %>% 
  sf::st_read(dsn = ., layer = 'ons11_MSOA') %>% 
  dplyr::filter(str_detect(MSOA11CD, '^E'))

### 0.4.2. lookup file ----

lookup.lsoa01_lsoa11 <- 
  file.path(dir.data, 'spatial', 'ons_lookup', 'LSOA01_LSOA11_LAD11_lookUp.csv') %>% 
  data.table::fread(file = .)

lookup.lsoa11_msoa11 <- 
  file.path(dir.data, 'spatial', 'ons_lookup', 'LSOA11_MSOA11_LAD11_lookUp.csv') %>% 
  data.table::fread(file = .) %>% 
  .[, !c('OA11CD', 'ObjectId'), with = FALSE] %>% 
  unique()

# 1. data ----

## 1.1. generic ----

generic.spatiotemporal <-
  data.table::CJ(MSOA11CD = poly.msoa$MSOA11CD,
                 year = 2002:2022) %>% 
  .[order(year, MSOA11CD)]

## 1.2. import & sort ----

data.final <- 
  dir.data.org.final %>% 
  fs::dir_ls(path = .,
             recurse = TRUE) %>%
  .[!grepl(pattern = 'POPULATIONTOTAL', .)] %>% 
  lapply(X = .,
         FUN = function(x) {
           
           # 0. function arguments ----
           
           # x <- 'Z:/home/englishSuicides_ITS/data/organised/99_final/MSOA11_RAILWAYNETWORKDENSITY.csv'
           
           # 1. parameters ----
           
           name.file <- x %>% basename() %>% tools::file_path_sans_ext()
           
           message(sprintf('Opening %s...', name.file))
           
           # 1. import ----
           
           data.import <- data.table::fread(file = x)
           
           # 2. sort ----
           
           ## 2.1. merging vars ----
           
           data.import.names <- colnames(data.import)
           
           by.vars <- if ('year' %in% data.import.names) {
             c('MSOA11CD', 'year')
           } else {
             'MSOA11CD'
           }
           
           ## 2.2. merge ----
           
           data.sort <-
             data.table::copy(generic.spatiotemporal) %>% 
             merge(x = .,
                   y = data.table::copy(data.import),
                   by = by.vars,
                   all.x = TRUE)
           
           # 3. clear ----
           
           rm(name.file,
              data.import.names,
              by.vars,
              data.import)
           gc()
           
           # 4. return ----
           
           return(data.sort)
           
         }) %>% 
  Reduce(f = function(x, y) merge(x, y, by = c('MSOA11CD', 'year'), all = TRUE),
         x = .) %>% 
  data.table::as.data.table()

data.final %>% is.na() %>% sum()

## 1.3. save ----

data.table::fwrite(x = data.final,
                   file = file.path(dir.data, 'organised', 'MSOA11_COVARIATES.csv'))

