# 0. set up ----

## 0.1. packages ----

# data wrangling
library(tidyverse)
library(data.table)
library(lubridate)

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

dir.data.org.pop <- file.path(dir.data, 'organised', '02_population')
fs::dir_create(dir.data.org.pop)

dir.data.org.final <- file.path(dir.data, 'organised', '99_final')
fs::dir_create(dir.data.org.final)

dir.res.cov.pop <- file.path(dir.res, '01_covariateData', '02_population')
fs::dir_create(dir.res.cov.pop)

## 0.3. functions ----

source(file.path(dir.code, '00_functions.R'))

## 0.4. spatial ----

### 0.4.1. polygon ----

poly.lsoa <- 
  file.path(dir.data, 'spatial', 'ons11_LSOA') %>% 
  sf::st_read(dsn = ., layer = 'ons11_LSOA') %>% 
  dplyr::filter(str_detect(LSOA11CD, '^E'))

poly.msoa <- 
  file.path(dir.data, 'spatial', 'ons11_MSOA') %>% 
  sf::st_read(dsn = ., layer = 'ons11_MSOA') %>% 
  dplyr::filter(str_detect(MSOA11CD, '^E'))

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

## 1.1. import ----

totals.02.males <- file.path(dir.data, 'population', '2002_2006_LSOA11_POPULATION_MALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2002')
totals.03.males <- file.path(dir.data, 'population', '2002_2006_LSOA11_POPULATION_MALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2003')
totals.04.males <- file.path(dir.data, 'population', '2002_2006_LSOA11_POPULATION_MALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2004')
totals.05.males <- file.path(dir.data, 'population', '2002_2006_LSOA11_POPULATION_MALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2005')
totals.06.males <- file.path(dir.data, 'population', '2002_2006_LSOA11_POPULATION_MALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2006')
totals.07.males <- file.path(dir.data, 'population', '2007_2011_LSOA11_POPULATION_MALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2007')
totals.08.males <- file.path(dir.data, 'population', '2007_2011_LSOA11_POPULATION_MALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2008')
totals.09.males <- file.path(dir.data, 'population', '2007_2011_LSOA11_POPULATION_MALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2009')
totals.10.males <- file.path(dir.data, 'population', '2007_2011_LSOA11_POPULATION_MALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2010')
totals.11.males <- file.path(dir.data, 'population', '2007_2011_LSOA11_POPULATION_MALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2011')
totals.12.males <- file.path(dir.data, 'population', '2012_LSOA11_POPULATION_TOTAL.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2012 Males', skip = 3)
totals.13.males <- file.path(dir.data, 'population', '2013_LSOA11_POPULATION_TOTAL.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2013 Males', skip = 3)
totals.14.males <- file.path(dir.data, 'population', '2014_LSOA11_POPULATION_TOTAL.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2014 Males', skip = 3)
totals.15.males <- file.path(dir.data, 'population', '2015_LSOA11_POPULATION_TOTAL.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2015 Males', skip = 3)
totals.16.males <- file.path(dir.data, 'population', '2016_LSOA11_POPULATION_TOTAL.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2016 Males', skip = 3)
totals.17.males <- file.path(dir.data, 'population', '2017_LSOA11_POPULATION_TOTAL.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2017 Males', skip = 3)
totals.18.males <- file.path(dir.data, 'population', '2018_LSOA11_POPULATION_TOTAL.xlsx') %>% readxl::read_xlsx(path = ., sheet = 'Mid-2018 Males', skip = 3)
totals.19.males <- file.path(dir.data, 'population', '2019_LSOA11_POPULATION_TOTAL.xlsx') %>% readxl::read_xlsx(path = ., sheet = 'Mid-2019 Males', skip = 3)
totals.20.males <- file.path(dir.data, 'population', '2020_LSOA11_POPULATION_TOTAL.xlsx') %>% readxl::read_xlsx(path = ., sheet = 'Mid-2020 Males', skip = 3)
totals.21.males <- file.path(dir.data, 'population', '2021_2022_LSOA21_POPULATION_TOTAL.xlsx') %>% readxl::read_xlsx(path = ., sheet = 'Mid-2021 LSOA 2021', skip = 3)
totals.22.males <- file.path(dir.data, 'population', '2021_2022_LSOA21_POPULATION_TOTAL.xlsx') %>% readxl::read_xlsx(path = ., sheet = 'Mid-2022 LSOA 2021', skip = 3)

totals.02.females <- file.path(dir.data, 'population', '2002_2006_LSOA11_POPULATION_FEMALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2002')
totals.03.females <- file.path(dir.data, 'population', '2002_2006_LSOA11_POPULATION_FEMALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2003')
totals.04.females <- file.path(dir.data, 'population', '2002_2006_LSOA11_POPULATION_FEMALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2004')
totals.05.females <- file.path(dir.data, 'population', '2002_2006_LSOA11_POPULATION_FEMALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2005')
totals.06.females <- file.path(dir.data, 'population', '2002_2006_LSOA11_POPULATION_FEMALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2006')
totals.07.females <- file.path(dir.data, 'population', '2007_2011_LSOA11_POPULATION_FEMALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2007')
totals.08.females <- file.path(dir.data, 'population', '2007_2011_LSOA11_POPULATION_FEMALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2008')
totals.09.females <- file.path(dir.data, 'population', '2007_2011_LSOA11_POPULATION_FEMALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2009')
totals.10.females <- file.path(dir.data, 'population', '2007_2011_LSOA11_POPULATION_FEMALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2010')
totals.11.females <- file.path(dir.data, 'population', '2007_2011_LSOA11_POPULATION_FEMALE.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2011')
totals.12.females <- file.path(dir.data, 'population', '2012_LSOA11_POPULATION_TOTAL.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2012 Females', skip = 3)
totals.13.females <- file.path(dir.data, 'population', '2013_LSOA11_POPULATION_TOTAL.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2013 Females', skip = 3)
totals.14.females <- file.path(dir.data, 'population', '2014_LSOA11_POPULATION_TOTAL.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2014 Females', skip = 3)
totals.15.females <- file.path(dir.data, 'population', '2015_LSOA11_POPULATION_TOTAL.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2015 Females', skip = 3)
totals.16.females <- file.path(dir.data, 'population', '2016_LSOA11_POPULATION_TOTAL.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2016 Females', skip = 3)
totals.17.females <- file.path(dir.data, 'population', '2017_LSOA11_POPULATION_TOTAL.xls') %>% readxl::read_xls(path = ., sheet = 'Mid-2017 Females', skip = 3)
totals.18.females <- file.path(dir.data, 'population', '2018_LSOA11_POPULATION_TOTAL.xlsx') %>% readxl::read_xlsx(path = ., sheet = 'Mid-2018 Females', skip = 3)
totals.19.females <- file.path(dir.data, 'population', '2019_LSOA11_POPULATION_TOTAL.xlsx') %>% readxl::read_xlsx(path = ., sheet = 'Mid-2019 Females', skip = 3)
totals.20.females <- file.path(dir.data, 'population', '2020_LSOA11_POPULATION_TOTAL.xlsx') %>% readxl::read_xlsx(path = ., sheet = 'Mid-2020 Females', skip = 3)
totals.21.females <- file.path(dir.data, 'population', '2021_2022_LSOA21_POPULATION_TOTAL.xlsx') %>% readxl::read_xlsx(path = ., sheet = 'Mid-2021 LSOA 2021', skip = 3)
totals.22.females <- file.path(dir.data, 'population', '2021_2022_LSOA21_POPULATION_TOTAL.xlsx') %>% readxl::read_xlsx(path = ., sheet = 'Mid-2022 LSOA 2021', skip = 3)

## 1.2. generic ----

data.base <- 
  data.table::CJ(LSOA11CD = poly.lsoa$LSOA11CD,
                 sex = c('male', 'female'),
                 age = c(as.character(0:89), '90plus'),
                 unique = TRUE) %>% 
  merge(x = .,
        y = lookup.lsoa11_msoa11 %>% .[, .(LSOA11CD, MSOA11CD)] %>% unique(),
        by = 'LSOA11CD',
        all.x = TRUE)

## 1.3. save yearly ----

data.list <-
  data.frame(YEAR = 2002:2022,
             MALE = paste0('totals.', sprintf('%02d', 2:22), '.males'),
             FEMALE = paste0('totals.', sprintf('%02d', 2:22), '.females'))

for(i in 1:nrow(data.list)){
  
  # i <- 20
  
  YEAR <- data.list$YEAR[i]
  cat(paste0('Year: ', YEAR, '\n'))
  
  if (YEAR %in% 2002:2011){
    
    male <-
      get(data.list[i, 'MALE']) %>%
      data.table::as.data.table() %>%
      .[, .SD, .SDcols = c('LSOA11CD', paste0('m', 0:89), 'm90plus')] %>%
      data.table::melt(id.vars = 'LSOA11CD', 
                       variable.name = 'age', 
                       value.name = 'population') %>%
      .[, `:=`(
        age = gsub('^m', '', age), 
        sex = 'male'
      )]
    
    female <-
      get(data.list[i, 'FEMALE']) %>%
      data.table::as.data.table() %>%
      .[, .SD, .SDcols = c('LSOA11CD', paste0('f', 0:89), 'f90plus')] %>%
      data.table::melt(id.vars = 'LSOA11CD', 
                       variable.name = 'age', 
                       value.name = 'population') %>%
      .[, `:=`(
        age = gsub('^f', '', age), 
        sex = 'female'
      )]
    
  } else if (YEAR %in% 2012:2018) {
    
    male <-
      get(data.list[i, 'MALE']) %>%
      data.table::as.data.table() %>%
      .[, LSOA11CD := `Area Codes`] %>%
      .[, .SD, .SDcols = c('LSOA11CD', as.character(0:89), '90+')] %>%
      data.table::melt(id.vars = 'LSOA11CD', 
                       variable.name = 'age', 
                       value.name = 'population') %>%
      .[, `:=`(
        sex = 'male',
        age = data.table::fifelse(age == '90+', '90plus', as.character(age))
      )]
    
    female <- 
      get(data.list[i, 'FEMALE']) %>%
      data.table::as.data.table() %>%
      .[, LSOA11CD := `Area Codes`] %>%
      .[, .SD, .SDcols = c('LSOA11CD', as.character(0:89), '90+')] %>%
      data.table::melt(id.vars = 'LSOA11CD', 
                       variable.name = 'age', 
                       value.name = 'population') %>%
      .[, `:=`(
        sex = 'female',
        age = data.table::fifelse(age == '90+', '90plus', as.character(age))
      )]
    
  } else if (YEAR %in% 2019:2020){
    
    male <-
      get(data.list[i, 'MALE']) %>%
      data.table::as.data.table() %>%
      .[, LSOA11CD := `LSOA Code`] %>%
      .[, .SD, .SDcols = c('LSOA11CD', as.character(0:89), '90+')] %>%
      data.table::melt(id.vars = 'LSOA11CD', 
                       variable.name = 'age', 
                       value.name = 'population') %>%
      .[, `:=`(
        sex = 'male',
        age = data.table::fifelse(age == '90+', '90plus', as.character(age))
      )]
    
    female <-
      get(data.list[i, 'FEMALE']) %>%
      data.table::as.data.table() %>%
      .[, LSOA11CD := `LSOA Code`] %>%
      .[, .SD, .SDcols = c('LSOA11CD', as.character(0:89), '90+')] %>%
      data.table::melt(id.vars = 'LSOA11CD', 
                       variable.name = 'age', 
                       value.name = 'population') %>%
      .[, `:=`(
        sex = 'female',
        age = data.table::fifelse(age == '90+', '90plus', as.character(age))
      )]
    
    
  } else if (YEAR %in% 2021:2022){
    
    male <- 
      get(data.list[i, 'MALE']) %>%
      data.table::as.data.table() %>%
      .[, LSOA21CD := `LSOA 2021 Code`] %>%
      .[, .SD, .SDcols = c('LSOA21CD', paste0('M', 0:90))] %>%
      data.table::melt(id.vars = 'LSOA21CD', 
                       variable.name = 'age', 
                       value.name = 'population') %>%
      .[, `:=`(
        age = data.table::fifelse(age == 'M90', '90plus', gsub('^M', '', age)), 
        sex = 'male'
      )]
    
    female <-
      get(data.list[i, 'FEMALE']) %>%
      data.table::as.data.table() %>%
      .[, LSOA21CD := `LSOA 2021 Code`] %>%
      .[, .SD, .SDcols = c('LSOA21CD', paste0('F', 0:90))] %>%
      data.table::melt(id.vars = 'LSOA21CD', 
                       variable.name = 'age', 
                       value.name = 'population') %>%
      .[, `:=`(
        age = data.table::fifelse(age == 'F90', '90plus', gsub('^F', '', age)), 
        sex = 'female'
      )]
    
    
  } else {
    
    stop('Warning: Year not in the correct range')
    
  }
  
  # join male and female
  both <-
    data.table::rbindlist(list(male, female)) %>% 
    .[, year := YEAR]
  
  if(YEAR %in% 2002:2020){
    
    data <-
      data.table::copy(both) %>%
      # join to base
      merge(x = data.table::copy(data.base),
            y = .,
            by = c('LSOA11CD', 'age', 'sex'),
            all.x = TRUE)
    
  } else {
    
    data <- 
      data.table::copy(both) %>% 
      # add lsoa11 code
      merge(x = .,
            y = lookup.lsoa11_lsoa21 %>% .[, .(LSOA11CD, LSOA21CD)] %>% unique(),
            by = 'LSOA21CD',
            all.x = TRUE) %>%
      # summarise 
      .[, .(
        population = sum(population)
        ),
        by = .(LSOA11CD, age, sex, year)] %>%
      # join to base
      merge(x = data.table::copy(data.base),
            y = ., 
            by = c('LSOA11CD', 'age', 'sex'), 
            all.x = TRUE)
    
  }
  
  num.missing <- data %>% is.na() %>% sum()
  cat(paste0(' Number of misisng values: ', num.missing, '\n'))
  # if(num.missing != 0) {
  #   data$population[is.na(data$population)] <-
  #     sapply(lsoa.nbs[which(is.na(data$population))],
  #            FUN = function(x) { mean(data$population[x], na.rm = TRUE) %>% round() })
  # }
  
  cat(' Saving .csv\n')
  
  data.table::fwrite(x = data,
                     file = file.path(dir.data.org.pop, sprintf('population_lsoa11_%s.csv', YEAR)))
  
}

## 1.4. clear ----

obj.total <- c(as.character(data.list$MALE),
               as.character(data.list$FEMALE))
rm(list = obj.total[obj.total %in% ls()])
gc()

rm(data.base,
   data.list,
   obj.total,
   YEAR,
   male,
   female,
   both,
   data)
gc()

# 2. sort population ----

## 2.1. load ----

data.import <-
  dir.data.org.pop %>% 
  fs::dir_ls(path = .,
             recurse = TRUE) %>% 
  sort() %>% 
  lapply(X = .,
         FUN = function(x) {
           
           # 0. function argument ----
           
           # x <- 
           #   fp.population %>% 
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
                        use.names = TRUE)

## 2.2. format data ----

data.sort <- 
  data.table::copy(data.import) %>% 
  # age into groups
  .[, age_num := data.table::fifelse(test = age == '90plus', yes = 90, no = as.numeric(age))] %>%
  .[, age :=
      cut(x = age_num,
          breaks = c(0, 5, 15, 25, 35, 45, 55, 65, 75, 85, Inf),
          right = FALSE,
          labels = c('0_4', '5_14', '15_24', '25_34', '35_44', '45_54', '55_64', '65_74', '75_84', '85')
      )] %>% 
  # aggregate up to msoa
  .[, 
    .(
      population = sum(population)
    ),
    by = .(MSOA11CD, year, sex, age)]

(data.sort %>% is.na() %>% sum()) == 0


data.sort.15_65plus <- 
  data.sort %>% 
  # remove below
  .[!(age %in% c('0_4', '5_14'))] %>% 
  # sum over 65+
  .[, age := data.table::fifelse(test = age %in% c('65_74', '75_84', '85'), yes = '65', no = as.character(age))] %>% 
  .[, 
    .(
      population = sum(population)
    ),
    by = .(MSOA11CD, year, sex, age)]

(data.sort.15_65plus %>% is.na() %>% sum()) == 0
(data.sort.15_65plus$population == 0) %>% sum() == 0

## 2.3. save ----

data.table::fwrite(x = data.sort,
                   file = file.path(dir.data.org.final, 'MSOA11_POPULATIONTOTAL.csv'))

data.table::fwrite(x = data.sort.15_65plus,
                   file = file.path(dir.data.org.final, 'MSOA11_POPULATIONTOTAL_15_65plus.csv'))
