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

dir.data.org.ethDensity <- file.path(dir.data, 'organised', '04_ethnicDensity')
fs::dir_create(dir.data.org.ethDensity)

dir.data.org.final <- file.path(dir.data, 'organised', '99_final')
fs::dir_create(dir.data.org.final)

dir.res.cov.ethDensity <- file.path(dir.res, '01_covariateData', '04_ethnicDensity')
fs::dir_create(dir.res.cov.ethDensity)

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
colours.sequential <- c('#E9E0C8', '#7D26CD')

# 1. data ----

## 1.1. generic ----

generic.spatial <- 
  data.table::data.table(LSOA11CD = poly.lsoa$LSOA11CD)

## 1.2. labels ----

labels.final <- c('LSOA11CD', 'total', 'asian', 'black', 'mixed', 'other', 'white')

## 1.3. sort census2001 ----

year <- 2001

message(sprintf('Processing year %s', year))

ed.01 <- 
  file.path(dir.data, 'ethnicDensity', '2001_lsoa_ethnicDiversity.csv') %>% 
  read.csv(file = ., skip = 7) %>% 
  dplyr::filter(row_number() <= n() - 3) %>% 
  data.table::as.data.table() %>% 
  .[, `:=` (
    # geographical identifiers
    LSOA01CD = mnemonic,
    total = All.categories..Ethnic.group %>% as.numeric(),
    ## ethnic proportions
    ### include Chinese in the Asian group
    asian = (Asian.Asian.British + Chinese.Other..Chinese),
    black = Black.Black.British,
    mixed = Mixed,
    ### only other
    other = Chinese.Other..Other,
    white = White
  )] %>% 
  .[, .(LSOA01CD, total, asian, black, mixed, other, white)] %>% 
  merge(x = ,
        y = 
          data.table::copy(lookup.lsoa01_lsoa11) %>% 
          .[, .(LSOA01CD, LSOA11CD)] %>% 
          unique(),
        by = 'LSOA01CD',
        all.x = TRUE) %>% 
  # merge to generic
  merge(x = generic.spatial,
        y = .,
        by = 'LSOA11CD',
        all.x = TRUE) %>% 
  .[, labels.final, with = FALSE] %>% 
  # impute missing
  polygon.imputeNeighbours(data = .,
                           polygon.cd = 'LSOA11CD',
                           polygon.nbs = nbs.first.lsoa) %>% 
  # average of duplicated
  .[, .(
    total = total %>% mean(x = ., na.rm = TRUE),
    asian = asian %>% mean(x = ., na.rm = TRUE),
    black = black %>% mean(x = ., na.rm = TRUE),
    mixed = mixed %>% mean(x = ., na.rm = TRUE),
    other = other %>% mean(x =., na.rm = TRUE),
    white = white %>% mean(x = ., na.rm = TRUE)
    ), 
    by = LSOA11CD] %>%  
  # add year
  .[, date := sprintf('%s-01-01', year) %>% as.Date() ] %>%
  # select columns
  .[, .SD, .SDcols = c('date', labels.final)]
  
message(sprintf(' number of missing areas: %s', ed.01 %>% is.na() %>% sum()))

data.table::fwrite(x = ed.01,
                   file = file.path(dir.data.org.ethDensity, sprintf('ethnicDensity_lsoa11_%s.csv', year)))

message(sprintf(' saved.'))

## 1.4. sort census2011 ----

year <- 2011

message(sprintf('Processing year %s', year))

ed.11 <- 
  file.path(dir.data, 'ethnicDensity', '2011_lsoa_ethnicDiversity.csv') %>% 
  data.table::fread(input = .) %>% 
  .[, `:=` (
    LSOA11CD = `geography code`,
    total = `Ethnic Group: All categories: Ethnic group; measures: Value`,
    asian = `Ethnic Group: Asian; measures: Value`,
    black = `Ethnic Group: Black; measures: Value`,
    mixed = `Ethnic Group: Mixed; measures: Value`,
    other = `Ethnic Group: Other; measures: Value`,
    white = `Ethnic Group: White; measures: Value`
  )] %>% 
  .[, .(LSOA11CD, total, asian, black, mixed, other, white)] %>% 
  # merge to generic
  merge(x = generic.spatial,
        y = .,
        by = 'LSOA11CD',
        all.x = TRUE) %>% 
  .[, labels.final, with = FALSE] %>% 
  # impute missing
  polygon.imputeNeighbours(data = .,
                           polygon.cd = 'LSOA11CD',
                           polygon.nbs = nbs.first.lsoa) %>% 
  # average of duplicated
  .[, .(
    total = total %>% mean(x = ., na.rm = TRUE),
    asian = asian %>% mean(x = ., na.rm = TRUE),
    black = black %>% mean(x = ., na.rm = TRUE),
    mixed = mixed %>% mean(x = ., na.rm = TRUE),
    other = other %>% mean(x =., na.rm = TRUE),
    white = white %>% mean(x = ., na.rm = TRUE)
  ), 
  by = LSOA11CD] %>%  
  # add year
  .[, date := sprintf('%s-01-01', year) %>% as.Date() ] %>%
  # select columns
  .[, .SD, .SDcols = c('date', labels.final)]

message(sprintf(' number of missing areas: %s', ed.11 %>% is.na() %>% sum()))

data.table::fwrite(x = ed.11,
                   file = file.path(dir.data.org.ethDensity, sprintf('ethnicDensity_lsoa11_%s.csv', year)))

message(sprintf(' saved.'))
  
## 1.5. sort census2021 ----

year <- 2021

message(sprintf('Processing year %s', year))

ed.21 <- 
  file.path(dir.data, 'ethnicDensity', '2021_lsoa_ethnicDiversity.csv') %>% 
  data.table::fread(input = .) %>% 
  .[, LSOA21CD := `Lower layer Super Output Areas Code`] %>% 
  .[`Ethnic group (20 categories) Code` > 0] %>% 
  .[, ethnicity := 
      dplyr::case_when(
        startsWith(x = `Ethnic group (20 categories)`, prefix = 'Asian') ~ 'asian',
        startsWith(x = `Ethnic group (20 categories)`, prefix = 'Black') ~ 'black',
        startsWith(x = `Ethnic group (20 categories)`, prefix = 'Mixed') ~ 'mixed',
        startsWith(x = `Ethnic group (20 categories)`, prefix = 'Other') ~ 'other',
        startsWith(x = `Ethnic group (20 categories)`, prefix = 'White') ~ 'white',
        .default = NA)
      ] %>% 
  .[, .(
    count = Observation %>% sum()
  ),
  by = .(LSOA21CD, ethnicity)] %>% 
  .[, data.table::dcast(
    data = .,
    formula = LSOA21CD ~ ethnicity,
    value.var = 'count')
    ] %>% 
  .[, total := rowSums(.SD, na.rm = TRUE),
    .SDcols = setdiff(names(.), 'LSOA21CD')] %>% 
  # merge to look up
  merge(x = .,
        y = data.table::copy(lookup.lsoa11_lsoa21) %>% .[, .(LSOA21CD, LSOA11CD)] %>% unique(),
        by = 'LSOA21CD',
        all.x = TRUE) %>% 
  # merge to generic
  merge(x = generic.spatial,
        y = .,
        by = 'LSOA11CD',
        all.x = TRUE) %>% 
  .[, labels.final, with = FALSE] %>% 
  # impute missing
  polygon.imputeNeighbours(data = .,
                           polygon.cd = 'LSOA11CD',
                           polygon.nbs = nbs.first.lsoa) %>% 
  # average of duplicated
  .[, .(
    total = total %>% mean(x = ., na.rm = TRUE),
    asian = asian %>% mean(x = ., na.rm = TRUE),
    black = black %>% mean(x = ., na.rm = TRUE),
    mixed = mixed %>% mean(x = ., na.rm = TRUE),
    other = other %>% mean(x =., na.rm = TRUE),
    white = white %>% mean(x = ., na.rm = TRUE)
  ), 
  by = LSOA11CD] %>%  
  # add year
  .[, date := sprintf('%s-01-01', year) %>% as.Date() ] %>%
  # select columns
  .[, .SD, .SDcols = c('date', labels.final)]

message(sprintf(' number of missing areas: %s', ed.21 %>% is.na() %>% sum()))

data.table::fwrite(x = ed.21,
                   file = file.path(dir.data.org.ethDensity, sprintf('ethnicDensity_lsoa11_%s.csv', year)))

message(sprintf(' saved.'))

## 1.6. clear ----

rm(generic.spatial,
   labels.final,
   year,
   ed.01,
   ed.11,
   ed.21)
gc()

# 2. sort ethnic denisty ----

## 2.1. import ----

data.import <- 
  dir.data.org.ethDensity %>% 
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

## 2.2. sort ----

### 2.2.1. temporary ----

data.sort.tmp <-
  data.table::copy(data.import) %>% 
  merge(x = .,
        y = data.table::copy(lookup.lsoa11_msoa11) %>% .[, .(LSOA11CD, MSOA11CD)] %>% unique(),
        by = 'LSOA11CD',
        all.x = TRUE) %>% 
  .[, 
    lapply(X = .SD, 
           FUN = function(x) sum(x = x, na.rm = TRUE)
           ),
    by = .(year, MSOA11CD),
    .SDcols = c('total', 'asian', 'black', 'mixed', 'other', 'white')
  ] %>% 
  .[, ethnicDensity := (total - white) / total] %>% 
  .[order(year, MSOA11CD)]

### 2.2.2. generic ----

generic.spatiotemporal <-
  data.table::CJ(MSOA11CD = poly.msoa$MSOA11CD,
                 year = 2002:2022) %>% 
  .[, yearLink := 
      dplyr::case_when(year %in% 2002:2006 ~ 2001,
                       year %in% 2007:2016 ~ 2011,
                       year %in% 2017:2022 ~ 2021,
                       .default = NA_integer_)]

### 2.2.3. final ----

data.sort <-
  data.table::copy(data.sort.tmp) %>% 
  .[, yearLink := year] %>% 
  .[, year := NULL] %>% 
  merge(x = generic.spatiotemporal,
        y = .,
        by = c('MSOA11CD', 'yearLink'),
        all.x = TRUE) %>% 
  .[, `:=` (
    total = NULL,
    yearLink = NULL
    )]

## 2.3. save ----

data.table::fwrite(x = data.sort,
                   file = file.path(dir.data.org.final, 'MSOA11_ETHNICDENSITY.csv'))

# 3. plot ----

## 3.1. prepare data ----

data.plot <- 
  file.path(dir.data.org.final, 'MSOA11_ETHNICDENSITY.csv') %>% 
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
    ggplot2::scale_fill_gradient(name = 'Ethnic Density (Non-White Population)',
                                 low = colours.sequential[1], high = colours.sequential[2],
                                 limits = c(0, 1), breaks = c(0, 0.5, 1),
                                 labels = scales::label_percent(),
                                 guide = guide_colourbar(title.position = 'top'))
  )

## 3.3. make animation ----

make.animationPlot(
  data = data.plot, 
  var.frame = 'year',
  var.fill = 'ethnicDensity',
  var.zoom = 'insideLondon',
  polygon = poly.msoa,
  polygon.cd = 'MSOA11CD',
  polygon.border = poly.rgn,
  polygon.border.zoom = poly.ldn,
  fill.ggLayer = fill.ggLayer,
  target.filename = 'MSOA11_ETHNICDENSITY',
  target.dir = dir.res.cov.ethDensity
)

## 3.4. average plot ----

data.average <-
  file.path(dir.data.org.final, 'MSOA11_ETHNICDENSITY.csv') %>% 
  data.table::fread() %>%  
  .[year %in% 2002:2017] %>% 
  .[, .(
    ethnicDensity = ethnicDensity %>% mean()
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
    var.fill = 'ethnicDensity',
    var.zoom = 'insideLondon',
    polygon = poly.msoa,
    polygon.cd = 'MSOA11CD',
    polygon.border = poly.rgn,
    polygon.border.zoom = poly.ldn,
    fill.ggLayer = fill.ggLayer,
    text.size = 20
  )

ggplot2::ggsave(
  filename = file.path(dir.res.cov.ethDensity, 'MSOA11_ETHNICDENSITY_AVERAGE.png'),
  plot = plot.average,
  width = width,
  height = height
)


