# 0. set up ----

## 0.1. librarys ----

# data wrangling
library(tidyverse)
library(data.table)
library(lubridate)
library(animation)

# spatial data
library(sf)
library(terra)
library(exactextractr)

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

dir.data.org.lightpol <- file.path(dir.data, 'organised', '06_lightPollution')
fs::dir_create(dir.data.org.lightpol)

dir.data.org.final <- file.path(dir.data, 'organised', '99_final')
fs::dir_create(dir.data.org.final)

dir.res.cov.lightpol <- file.path(dir.res, '01_covariateData', '06_lightPollution')
fs::dir_create(dir.res.cov.lightpol)


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

nbs.first.msoa <-
  file.path(dir.data, 'organised', '01_spatial', 'neighbours_first_order_msoa.rds') %>% 
  readRDS(file = .)

nbs.second.msoa <-
  file.path(dir.data, 'organised', '01_spatial', 'neighbours_second_order_msoa.rds') %>% 
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

## 1.1. import rasters ----

raster.import <- 
  file.path(dir.data, 'lightPollution') %>% 
  fs::dir_ls(path = .,
             recurse = TRUE) %>% 
  sort() %>% 
  lapply(X = .,
         FUN = function(x) {
           terra::rast(x)
           })

## 1.2. sort rasters ----

raster.sort <- 
  raster.import %>% seq_along() %>% 
  lapply(X = .,
         FUN = function(i) {
           
           # 0 . function arguments ----
           
           # i <- 1
           
           # 1. parameters ----
           
           name.fp <- names(raster.import)[i]
           name.file <- name.fp %>% basename() %>% tools::file_path_sans_ext()
           name.year <- name.file %>% stringr::str_extract(string = ., pattern = '(19|20)\\d{2}') %>% as.numeric()
           
           message(sprintf('Sorting %s:', name.file))
           
           # 2. import yearr ---
           
           import.year <- 
             raster.toPolygon(
               raster = raster.import[[i]], 
               polygon = poly.msoa,
               polygon.cd = 'MSOA11CD',
               polygon.nbs = nbs.first.msoa
             )
           
           # 3. sort year ----
           
           sort.year <- 
             data.table::copy(import.year) %>% 
             .[, `:=` (
               year = name.year, 
               lightPollution = weighted_mean
               )] %>% 
             .[, weighted_mean := NULL]
           
           # 5. save ----
           
           message(sprintf(' number missing: %s', sort.year %>% is.na() %>% sum()))
           
           data.table::fwrite(x = sort.year,
                              file = file.path(dir.data.org.lightpol, sprintf('lightPollution_msoa11_%s.csv', name.year)))
           
           message(sprintf(' saved.'))
           
           # 6. clear ----
           
           rm(import.year,
              sort.year)
           gc()
           
           # 7. return ----
           
           return(invisible(NULL))
         
           }) %>% 
  invisible()

## 1.3. clear ----

rm(raster.import,
   raster.sort)
gc()

# 2. sort light pollution ----

## 2.1. import ----

data.import <- 
  dir.data.org.lightpol %>% 
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
                        use.names = TRUE)

## 2.2. sort ----

data.sort <-
  data.table::CJ(year = 1992:2022,
                 MSOA11CD = poly.msoa$MSOA11CD) %>% 
  merge(x = .,
        y = data.table::copy(data.import),
        by = c('MSOA11CD', 'year'),
        all.x = TRUE) %>% 
  .[order(year, MSOA11CD)]

## 2.3. save ----

data.table::fwrite(x = data.sort,
                   file = file.path(dir.data.org.final, 'MSOA11_LIGHTPOLLUTION.csv'))

# 3. plot ----

## 3.1. prepare data ----

data.plot <- 
  file.path(dir.data.org.final, 'MSOA11_LIGHTPOLLUTION.csv') %>% 
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
    ggplot2::scale_fill_gradient(name = 'Light pollution (DN values)',
                                 low = colours.sequential[1], high = colours.sequential[2],
                                 limits = c(0, 64), breaks = c(0, 32, 64),
                                 guide = guide_colourbar(title.position = 'top'))
  )

## 3.3. make animation ----

make.animationPlot(
  data = data.plot, 
  var.frame = 'year',
  var.fill = 'lightPollution',
  var.zoom = 'insideLondon',
  polygon = poly.msoa,
  polygon.cd = 'MSOA11CD',
  polygon.border = poly.rgn,
  polygon.border.zoom = poly.ldn,
  fill.ggLayer = fill.ggLayer,
  target.filename = 'MSOA11_LIGHTPOLLUTION',
  target.dir = dir.res.cov.lightpol
)

## 3.4. average plot ----

data.average <-
  file.path(dir.data.org.final, 'MSOA11_LIGHTPOLLUTION.csv') %>% 
  data.table::fread() %>%  
  .[year %in% 2002:2017] %>% 
  .[, .(
    lightPollution = lightPollution %>% mean()
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
    var.fill = 'lightPollution',
    var.zoom = 'insideLondon',
    polygon = poly.msoa,
    polygon.cd = 'MSOA11CD',
    polygon.border = poly.rgn,
    polygon.border.zoom = poly.ldn,
    fill.ggLayer = fill.ggLayer,
    text.size = 20
  )

ggplot2::ggsave(
  filename = file.path(dir.res.cov.lightpol, 'MSOA11_LIGHTPOLLUTION_AVERAGE.png'),
  plot = plot.average,
  width = width,
  height = height
)