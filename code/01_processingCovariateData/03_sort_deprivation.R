# 0. set up ----

## 0.1. packages ----

# data wrangling
library(tidyverse)
library(data.table)
library(lubridate)
library(animation)

# spatial data
library(sf)

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

dir.data.org.deprivation <- file.path(dir.data, 'organised', '03_deprivation')
fs::dir_create(dir.data.org.deprivation)

dir.data.org.final <- file.path(dir.data, 'organised', '99_final')
fs::dir_create(dir.data.org.final)

dir.res.cov.imd <- file.path(dir.res, '01_covariateData', '03_deprivation')
fs::dir_create(dir.res.cov.imd)

## 0.3. functions ----

source(file.path(dir.code, '00_functions.R'))

## 0.4. shape file ----

### 0.4.1. polygon ----

poly.lsoa <- 
  file.path(dir.data, 'spatial', 'ons11_LSOA') %>% 
  sf::st_read(dsn = ., layer = 'ons11_LSOA') %>% 
  dplyr::filter(str_detect(LSOA11CD, '^E'))

poly.msoa <- 
  file.path(dir.data, 'spatial', 'ons11_MSOA') %>% 
  sf::st_read(dsn = ., layer = 'ons11_MSOA') %>% 
  dplyr::filter(str_detect(MSOA11CD, '^E'))

poly.rgn <- 
  file.path(dir.data, 'spatial', 'ons21_RGN') %>% 
  sf::st_read(dsn = ., layer = 'ons21_RGN') %>% 
  dplyr::filter(str_detect(RGN21CD, '^E'))

poly.ldn <-
  poly.rgn %>% 
  dplyr::filter(RGN21NM == 'London')

### 0.4.2. lookup file ----

lookup.lsoa01_lsoa11 <- 
  file.path(dir.data, 'spatial', 'ons_lookup', 'LSOA01_LSOA11_LAD11_lookUp.csv') %>% 
  data.table::fread(file = .)

lookup.lsoa11_lsoa21 <- 
  file.path(dir.data, 'spatial', 'ons_lookup', 'LSOA11_LSOA21_LAD22_lookUp.csv') %>% 
  data.table::fread(file = .)

lookup.lsoa11_msoa11 <- 
  file.path(dir.data, 'spatial', 'ons_lookup', 'LSOA11_MSOA11_LAD11_lookUp.csv') %>% 
  data.table::fread(file = .) %>% 
  .[, !c('OA11CD', 'ObjectId'), with = FALSE] %>% 
  unique()

### 0.4.3. neighbourhoods ----

nbs.first.lsoa <-
  file.path(dir.data, 'organised', '01_spatial', 'neighbours_first_order_lsoa.rds') %>% 
  readRDS(file = .)

nbs.second.lsoa <-
  file.path(dir.data, 'organised', '01_spatial', 'neighbours_second_order_lsoa.rds') %>% 
  readRDS(file = .)

## 0.5. plot arguments ----

# plot arguments
text.size <- 20
width <- height <- 10

# colour schemes
colours.diverging.full <- c('#00768D', '#7CB5C0', '#E9E0C8', '#FB8171', '#C13E2E')
colours.diverging <- c('#00768D', '#C13E2E')
colours.exceedance <- c('#A8E6CF', '#5CDB95', '#004D3A')
colours.sequential <- c('#E9E0C8', '#4B0082')

# 1. data ----

## 1.1. generic ----

generic.spatial <- 
  data.table::data.table(LSOA11CD = poly.lsoa$LSOA11CD)

## 1.2. labels ----

labels.imd <-
  data.frame(new = 
               c('imdScore', 'incomeScore', 'employmentScore', 
                 'healthScore', 'educationScore', 'housingScore',
                 'crimeScore', 'environmentScore'),
             imd04 = 
               c('IMD SCORE', 'INCOME SCORE', 'EMPLOYMENT SCORE', 
                 'HEALTH DEPRIVATION AND DISABILITY SCORE', 'EDUCATION SKILLS AND TRAINING SCORE', 'BARRIERS TO HOUSING AND SERVICES SCORE', 
                 'CRIME AND DISORDER SCORE', 'LIVING ENVIRONMENT SCORE'),
             imd07 = 
               c('IMD SCORE', 'INCOME SCORE', 'EMPLOYMENT SCORE', 
                 'HEALTH DEPRIVATION AND DISABILITY SCORE', 'EDUCATION SKILLS AND TRAINING SCORE', 'BARRIERS TO HOUSING AND SERVICES SCORE', 
                 'CRIME AND DISORDER SCORE', 'LIVING ENVIRONMENT SCORE'),
             imd10 = 
               c('IMD SCORE', 'INCOME SCORE', 'EMPLOYMENT SCORE',
                 'HEALTH DEPRIVATION AND DISABILITY SCORE', 'EDUCATION SKILLS AND TRAINING SCORE', 'BARRIERS TO HOUSING AND SERVICES SCORE',
                 'CRIME AND DISORDER SCORE', 'LIVING ENVIRONMENT SCORE'),
             imd15 = 
               c('Index of Multiple Deprivation (IMD) Score', 'Income Score (rate)', 'Employment Score (rate)',
                 'Health Deprivation and Disability Score', 'Education, Skills and Training Score', 'Barriers to Housing and Services Score',
                 'Crime Score', 'Living Environment Score'),
             imd19 = 
               c('Index of Multiple Deprivation (IMD) Score', 'Income Score (rate)', 'Employment Score (rate)',
                 'Health Deprivation and Disability Score', 'Education, Skills and Training Score', 'Barriers to Housing and Services Score',
                 'Crime Score', 'Living Environment Score'))

labels.lsoa01 <- c('LSOA11CD', labels.imd$new)
labels.lsoa11 <- c('LSOA11CD', labels.imd$new)
labels.lsoa21 <- c('LSOA11CD', labels.imd$new)

## 1.3. sort imd2004 ----

year <- 2004

message(sprintf('Processing IMD for %s', year))

imd.04 <-
  # import
  ## open each sheet (except notes)
  file.path(dir.data, 'deprivation', 'LSOA01_2004_IMD_SCORES.xls') %>% 
  lapply(X = readxl::excel_sheets(path = .)[-1],
         FUN = function(x, path) readxl::read_excel(path = path, sheet = x),
         path = .) %>% 
  ## join together
  purrr::reduce(.x = .,
                .f = dplyr::left_join,
                by = c('SOA', 'LA CODE', 'LA NAME', 'GOR CODE', 'GOR NAME')) %>% 
  ## format
  data.table::as.data.table() %>% 
  data.table::setnames(x = ., 
                       old = c('SOA', labels.imd$imd04), 
                       new = c('LSOA01CD', labels.imd$new)) %>% 
  # join look ups
  ## 2001 - 2011
  merge(x = ,
        y = 
          data.table::copy(lookup.lsoa01_lsoa11) %>% 
          .[, .(LSOA01CD, LSOA11CD)] %>% 
          unique(),
        by = 'LSOA01CD',
        all.x = TRUE) %>%
  # select columns
  ## LSOA21CD and columns for imd and other scores
  .[, labels.lsoa01] %>% 
  # merge to generic
  merge(x = generic.spatial,
        y = .,
        by = 'LSOA11CD',
        all.x = TRUE) %>% 
  # impute missing
  polygon.imputeNeighbours(data = .,
                           polygon.cd = 'LSOA11CD',
                           polygon.nbs = nbs.first.lsoa) %>% 
  # average of duplicated
  .[, .(
    imdScore = imdScore %>% mean(x = ., na.rm = TRUE),
    incomeScore = incomeScore %>% mean(x = ., na.rm = TRUE),
    employmentScore = employmentScore %>% mean(x = ., na.rm = TRUE),
    healthScore = healthScore %>% mean(x = ., na.rm = TRUE),
    educationScore = educationScore %>% mean(x =., na.rm = TRUE),
    housingScore = housingScore %>% mean(x = ., na.rm = TRUE),
    crimeScore = crimeScore %>% mean(x = ., na.rm = TRUE),
    environmentScore = environmentScore %>% mean(x = ., na.rm = TRUE)), 
    by = LSOA11CD] %>%  
  # add year 
  .[, date := sprintf('%s-01-01', year) %>% as.Date()] %>%
  # select columns
  .[, .SD, .SDcols = c('date', labels.lsoa01)]

message(sprintf(' Number of missing areas after formatting is: %s', imd.04 %>% is.na() %>% sum()))

data.table::fwrite(x = imd.04,
                   file = file.path(dir.data.org.deprivation, sprintf('deprivation_lsoa11_%s.csv', year)))
message(sprintf(' Saved formatted version of the %s IMD.', year))


## 1.4. sort imd2007 ----

year <- 2007

message(sprintf('Processing IMD for %s', year))

imd.07 <-
  # import
  ## open each sheet (except notes)
  file.path(dir.data, 'deprivation', 'LSOA01_2007_IMD_SCORES.xls') %>% 
  lapply(X = readxl::excel_sheets(path = .)[-1],
         FUN = function(x, path) readxl::read_excel(path = path, sheet = x),
         path = .) %>% 
  ## join together
  purrr::reduce(.x = .,
                .f = dplyr::left_join,
                by = c('LSOA', 'LA CODE', 'LA NAME', 'GOR CODE', 'GOR NAME')) %>% 
  ## format
  data.table::as.data.table() %>% 
  data.table::setnames(x = ., 
                       old = c('LSOA', labels.imd$imd07), 
                       new = c('LSOA01CD', labels.imd$new)) %>% 
  # join look ups
  ## 2001 - 2011
  merge(x = ,
        y = 
          data.table::copy(lookup.lsoa01_lsoa11) %>% 
          .[, .(LSOA01CD, LSOA11CD)] %>% 
          unique(),
        by = 'LSOA01CD',
        all.x = TRUE) %>% 
  # select columns
  ## LSOA21CD and columns for imd and other scores
  .[, labels.lsoa01] %>% 
  # merge to generic
  merge(x = generic.spatial,
        y = .,
        by = 'LSOA11CD',
        all.x = TRUE) %>% 
  # impute missing
  polygon.imputeNeighbours(data = .,
                           polygon.cd = 'LSOA11CD',
                           polygon.nbs = nbs.first.lsoa)  %>%  
  # average of duplicated
  .[, .(
    imdScore = imdScore %>% mean(x = ., na.rm = TRUE),
    incomeScore = incomeScore %>% mean(x = ., na.rm = TRUE),
    employmentScore = employmentScore %>% mean(x = ., na.rm = TRUE),
    healthScore = healthScore %>% mean(x = ., na.rm = TRUE),
    educationScore = educationScore %>% mean(x =., na.rm = TRUE),
    housingScore = housingScore %>% mean(x = ., na.rm = TRUE),
    crimeScore = crimeScore %>% mean(x = ., na.rm = TRUE),
    environmentScore = environmentScore %>% mean(x = ., na.rm = TRUE)), 
    by = LSOA11CD] %>%  
  # add year 
  .[, date := sprintf('%s-01-01', year) %>% as.Date()] %>%
  # select columns
  .[, .SD, .SDcols = c('date', labels.lsoa01)]

message(sprintf(' Number of missing areas after formatting is: %s', imd.07 %>% is.na() %>% sum()))

data.table::fwrite(x = imd.07,
                   file = file.path(dir.data.org.deprivation, sprintf('deprivation_lsoa11_%s.csv', year)))
message(sprintf(' Saved formatted version of the %s IMD.', year))

## 1.5. sort imd2010 ----

year <- 2010

message(sprintf('Processing IMD for %s', year))

imd.10 <-
  # import
  ## open each sheet (except notes)
  file.path(dir.data, 'deprivation', 'LSOA01_2010_IMD_SCORES.xls') %>% 
  lapply(X = readxl::excel_sheets(path = .)[-1],
         FUN = function(x, path) readxl::read_excel(path = path, sheet = x),
         path = .) %>% 
  ## join together
  purrr::reduce(.x = .,
                .f = dplyr::left_join,
                by = c('LSOA CODE')) %>% 
  data.table::as.data.table() %>% 
  ## format
  data.table::setnames(x = ., 
                       old = c('LSOA CODE', labels.imd$imd10), 
                       new = c('LSOA01CD', labels.imd$new)) %>% 
  # join look ups
  ## 2001 - 2011
  merge(x = ,
        y = 
          data.table::copy(lookup.lsoa01_lsoa11) %>% 
          .[, .(LSOA01CD, LSOA11CD)] %>% 
          unique(),
        by = 'LSOA01CD',
        all.x = TRUE) %>% 
  # select columns
  ## LSOA21CD and columns for imd and other scores
  .[, labels.lsoa01] %>% 
  # merge to generic
  merge(x = generic.spatial,
        y = .,
        by = 'LSOA11CD',
        all.x = TRUE) %>% 
  # impute missing
  polygon.imputeNeighbours(data = .,
                           polygon.cd = 'LSOA11CD',
                           polygon.nbs = nbs.first.lsoa) %>%  
  # average of duplicated
  .[, .(
    imdScore = imdScore %>% mean(x = ., na.rm = TRUE),
    incomeScore = incomeScore %>% mean(x = ., na.rm = TRUE),
    employmentScore = employmentScore %>% mean(x = ., na.rm = TRUE),
    healthScore = healthScore %>% mean(x = ., na.rm = TRUE),
    educationScore = educationScore %>% mean(x =., na.rm = TRUE),
    housingScore = housingScore %>% mean(x = ., na.rm = TRUE),
    crimeScore = crimeScore %>% mean(x = ., na.rm = TRUE),
    environmentScore = environmentScore %>% mean(x = ., na.rm = TRUE)), 
    by = LSOA11CD] %>%  
  # add year 
  .[, date := sprintf('%s-01-01', year) %>% as.Date()] %>%
  # select columns
  .[, .SD, .SDcols = c('date', labels.lsoa01)]

message(sprintf(' Number of missing areas after formatting is: %s', imd.10 %>% is.na() %>% sum()))

data.table::fwrite(x = imd.10,
                   file = file.path(dir.data.org.deprivation, sprintf('deprivation_lsoa11_%s.csv', year)))
message(sprintf(' Saved formatted version of the %s IMD.', year))

## 1.6. sort imd2015 ----

year <- 2015

message(sprintf('Processing IMD for %s', year))

imd.15 <- 
  # import
  file.path(dir.data, 'deprivation', 'LSOA11_2015_IMD_SCORES.xlsx') %>% 
  readxl::read_excel(path = ., sheet = 'ID2015 Scores') %>% 
  # format
  data.table::as.data.table() %>% 
  data.table::setnames(x = ., 
                       old = c('LSOA code (2011)', labels.imd$imd15), 
                       new = c('LSOA11CD', labels.imd$new)) %>% 
  # select columns
  ## LSOA11CD and columns for imd and other scores
  .[, labels.lsoa11, with = FALSE] %>% 
  # merge to generic
  merge(x = generic.spatial,
        y = .,
        by = 'LSOA11CD',
        all.x = TRUE) %>% 
  # impute missing
  polygon.imputeNeighbours(data = .,
                           polygon.cd = 'LSOA11CD',
                           polygon.nbs = nbs.first.lsoa) %>%  
  # average of duplicated
  .[, .(
    imdScore = imdScore %>% mean(x = ., na.rm = TRUE),
    incomeScore = incomeScore %>% mean(x = ., na.rm = TRUE),
    employmentScore = employmentScore %>% mean(x = ., na.rm = TRUE),
    healthScore = healthScore %>% mean(x = ., na.rm = TRUE),
    educationScore = educationScore %>% mean(x =., na.rm = TRUE),
    housingScore = housingScore %>% mean(x = ., na.rm = TRUE),
    crimeScore = crimeScore %>% mean(x = ., na.rm = TRUE),
    environmentScore = environmentScore %>% mean(x = ., na.rm = TRUE)), 
    by = LSOA11CD] %>%  
  # add year 
  .[, date := sprintf('%s-01-01', year) %>% as.Date()] %>%
  # select columns
  .[, .SD, .SDcols = c('date', labels.lsoa11)]

message(sprintf(' Number of missing areas after formatting is: %s', imd.15 %>% is.na() %>% sum()))

data.table::fwrite(x = imd.15,
                   file = file.path(dir.data.org.deprivation, sprintf('deprivation_lsoa11_%s.csv', year)))
message(sprintf(' Saved formatted version of the %s IMD.', year))

## 1.7. sort imd2019 ----

year <- 2019

message(sprintf('Processing IMD for %s', year))

imd.19 <- 
  # import
  file.path(dir.data, 'deprivation', 'LSOA11_2019_IMD_SCORES.xlsx') %>% 
  readxl::read_excel(path = ., sheet = 'IoD2019 Scores') %>% 
  # format
  data.table::as.data.table() %>% 
  data.table::setnames(x = ., 
                       old = c('LSOA code (2011)', labels.imd$imd19), 
                       new = c('LSOA11CD', labels.imd$new)) %>% 
  # select columns
  ## LSOA21CD and columns for imd and other scores
  .[, labels.lsoa11, with = FALSE] %>% 
  # merge to generic
  merge(x = generic.spatial,
        y = .,
        by = 'LSOA11CD',
        all.x = TRUE) %>% 
  # impute missing
  polygon.imputeNeighbours(data = .,
                           polygon.cd = 'LSOA11CD',
                           polygon.nbs = nbs.first.lsoa) %>%  
  # average of duplicated
  .[, .(
    imdScore = imdScore %>% mean(x = ., na.rm = TRUE),
    incomeScore = incomeScore %>% mean(x = ., na.rm = TRUE),
    employmentScore = employmentScore %>% mean(x = ., na.rm = TRUE),
    healthScore = healthScore %>% mean(x = ., na.rm = TRUE),
    educationScore = educationScore %>% mean(x =., na.rm = TRUE),
    housingScore = housingScore %>% mean(x = ., na.rm = TRUE),
    crimeScore = crimeScore %>% mean(x = ., na.rm = TRUE),
    environmentScore = environmentScore %>% mean(x = ., na.rm = TRUE)), 
    by = LSOA11CD] %>%  
  # add year 
  .[, date := sprintf('%s-01-01', year) %>% as.Date()] %>%
  # select columns
  .[, .SD, .SDcols = c('date', labels.lsoa11)]

message(sprintf(' Number of missing areas after formatting is: %s', imd.19 %>% is.na() %>% sum()))

data.table::fwrite(x = imd.19,
                   file = file.path(dir.data.org.deprivation, sprintf('deprivation_lsoa11_%s.csv', year)))
message(sprintf(' Saved formatted version of the %s IMD.', year))

## 1.8. clear ----

rm(generic.spatial,
   labels.lsoa01,
   labels.lsoa11,
   labels.lsoa21,
   year, 
   imd.04,
   imd.07, 
   imd.10,
   imd.15,
   imd.19)
gc()

# 2. sort deprivation ----

## 2.1. import imd ----

data.import.imd <- 
  dir.data.org.deprivation %>% 
  fs::dir_ls(path = .,
             recurse = TRUE) %>% 
  sort() %>% 
  lapply(X = .,
         FUN = function(x) {
           
           # 0. function argument ----
           
           # x <- 
           #   dir.data.org.deprivation %>% 
           #   fs::dir_ls(path = .,
           #              recurse = TRUE) %>% 
           #   .[1]
           
           # 1. parameters ----
           
           name.file <- x %>% basename() %>% tools::file_path_sans_ext()
           
           message(sprintf('opening %s...', name.file))
           
           # 2. open ----
           
           data.table::fread(file = x)
           
         }) %>% 
  data.table::rbindlist(l = .,
                        use.names = TRUE) %>% 
  .[, year := date %>% lubridate::year()] %>% 
  .[, date := NULL]

## 2.2. import population ----

data.import.population <-
  file.path(dir.data, 'organised', '02_population') %>% 
  fs::dir_ls(path = .,
             recurse = TRUE) %>% 
  sort() %>% 
  lapply(X = .,
         FUN = function(x) {
           
           # 0. function argument ----
           
           # x <- 
           #   file.path(dir.data, 'organised', 'population') %>% 
           #   fs::dir_ls(path = .,
           #              recurse = TRUE) %>% 
           #   .[1]
           
           # 1. parameters ----
           
           name.file <- x %>% basename() %>% tools::file_path_sans_ext()
           
           message(sprintf('opening %s...', name.file))
           
           # 2. open ----
           
           data.table::fread(file = x)
           
         }) %>% 
  data.table::rbindlist(l = .,
                        use.names = TRUE) %>% 
  .[, .(
    population = sum(population)
  ),
  by = .(LSOA11CD, year)]

## 2.3. sort deprivation ----

### 2.3.1. weight average function -----

wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

### 2.3.2. temporary ----

data.sort.tmp <-
  data.table::copy(data.import.imd) %>% 
  merge(x = .,
        y = data.table::copy(data.import.population),
        by = c('LSOA11CD', 'year'),
        all.x = TRUE) %>% 
  merge(x = .,
        y = data.table::copy(lookup.lsoa11_msoa11) %>% .[, .(LSOA11CD, MSOA11CD)] %>% unique(),
        by = 'LSOA11CD',
        all.x = TRUE) %>% 
  .[, c(
    lapply(X = .SD, FUN = wmean, w = population),
    list(population = sum(population, na.rm = TRUE))
    ),
    by = .(year, MSOA11CD),
    .SDcols = labels.imd$new
    ] %>% 
  .[, population := NULL] %>% 
  .[order(year, MSOA11CD)]

### 2.3.3. generic ----

generic.spatiotemporal <-
  data.table::CJ(MSOA11CD = poly.msoa$MSOA11CD,
                 year = 2002:2022) %>% 
  .[, yearLink := 
      dplyr::case_when(year %in% 2002:2006 ~ 2004,
                       year %in% 2007:2009 ~ 2007,
                       year %in% 2010:2014 ~ 2010,
                       year %in% 2015:2018 ~ 2015,
                       year %in% 2019:2022 ~ 2019,
                       .default = NA_integer_)]

### 2.3.4. final ----

data.sort <-
  data.table::copy(data.sort.tmp) %>% 
  .[, yearLink := year] %>% 
  .[, year := NULL] %>% 
  merge(x =data.table::copy(generic.spatiotemporal),
        y = .,
        by = c('MSOA11CD', 'yearLink'),
        all.x = TRUE) %>%
  .[, deprivation := imdScore] %>% 
  .[, `:=` (
    imdScore = NULL,
    yearLink = NULL
    )]

## 2.4. save ----

data.table::fwrite(x = data.sort,
                   file = file.path(dir.data.org.final, 'MSOA11_DEPRIVATION.csv'))

# 3. plot ----

## 3.1. prepare data ----

data.plot <- 
  file.path(dir.data.org.final, 'MSOA11_DEPRIVATION.csv') %>% 
  data.table::fread() %>% 
  .[year %in% 2002:2017] %>% 
  merge(x = .,
        y = lookup.lsoa11_msoa11 %>% .[, .(MSOA11CD, LAD11CD)] %>% unique(),
        by = 'MSOA11CD',
        all.x = TRUE) %>% 
  .[, insideLondon := 
      LAD11CD %>% 
      grepl(pattern = '^E09', x = .) %>% 
      as.integer()]

## 3.2. prepare plot layer ----

fill.ggLayer <-
  list(
    ggplot2::scale_fill_gradient(name = 'Index of Multiple Deprivation Score',
                                 low = colours.sequential[1], high = colours.sequential[2],
                                 limits = c(0, 100), breaks = c(0, 50, 100),
                                 guide = guide_colourbar(title.position = 'top'))
  )

## 3.3. make animation ----

make.animationPlot(
  data = data.plot, 
  var.frame = 'year',
  var.fill = 'deprivation',
  var.zoom = 'insideLondon',
  polygon = poly.msoa,
  polygon.cd = 'MSOA11CD',
  polygon.border = poly.rgn,
  polygon.border.zoom = poly.ldn,
  fill.ggLayer = fill.ggLayer,
  target.filename = 'MSOA11_DEPRIVATION',
  target.dir = dir.res.cov.imd
)

## 3.4. average plot ----

data.average <-
  file.path(dir.data.org.final, 'MSOA11_DEPRIVATION.csv') %>% 
  data.table::fread() %>%  
  .[year %in% 2002:2017] %>% 
  .[, .(
    deprivation = deprivation %>% mean()
  ),
  by = MSOA11CD] %>% 
  merge(x = .,
        y = lookup.lsoa11_msoa11 %>% .[, .(MSOA11CD, LAD11CD)] %>% unique(),
        by = 'MSOA11CD',
        all.x = TRUE) %>% 
  .[, insideLondon := 
      LAD11CD %>% 
      grepl(pattern = '^E09', x = .) %>% 
      as.integer()]

plot.average <-
  make.singlePlot(
    data = data.average,
    var.fill = 'deprivation',
    var.zoom = 'insideLondon',
    polygon = poly.msoa,
    polygon.cd = 'MSOA11CD',
    polygon.border = poly.rgn,
    polygon.border.zoom = poly.ldn,
    fill.ggLayer = fill.ggLayer,
    text.size = 20
  )

ggplot2::ggsave(
  filename = file.path(dir.res.cov.imd, 'MSOA11_DEPRIVATION_AVERAGE.png'),
  plot = plot.average,
  width = width,
  height = height
)
