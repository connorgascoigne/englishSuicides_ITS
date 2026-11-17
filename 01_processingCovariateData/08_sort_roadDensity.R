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

dir.data.org.road <- file.path(dir.data, 'organised', '08_roadwayDensity')
fs::dir_create(dir.data.org.road)

dir.data.org.final <- file.path(dir.data, 'organised', '99_final')
fs::dir_create(dir.data.org.final)

dir.res.cov.road <- file.path(dir.res, '01_covariateData', '08_roadwayDensity')
fs::dir_create(dir.res.cov.road)


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

## 1.1. parameters ----

files.fullname <- 
  file.path(dir.data, 'roadwayDensity') %>% 
  fs::dir_ls(path = .,
             recurse = TRUE) %>% 
  basename() %>% 
  tools::file_path_sans_ext() %>% 
  unique()

files.chunk <- 
  files.fullname %>% 
  sub(pattern = '_.*', replacement = '', x = .) %>% 
  unique() %>% 
  sort()

crs.msoa <- sf::st_crs(poly.msoa)

## 1.2. import ----

import.road <- 
  files.chunk %>% seq_along() %>% 
  lapply(X = , 
         FUN = function(i) {
           
           # 0. function argument ----
           
           # i <- 1
           
           # 1. parameters ----
           
           name.chunk <- files.chunk[i]
           name.layer <- sprintf('%s_RoadLink', name.chunk)
           
           message(sprintf('Importing %s', name.chunk))
           
           # 2. import ----
           
           import.chunk <-
             sf::st_read(dsn = file.path(dir.data, 'roadwayDensity'), 
                         layer = name.layer, 
                         quiet = TRUE) %>% 
             sf::st_transform(., crs = crs.msoa)
           
           # 3. return ----
           
           return(import.chunk)
         }
         ) %>%
  dplyr::bind_rows() %>% 
  dplyr::mutate(roadType = dplyr::if_else(class %in% c('Motorway', 'A Road'), 'major', 'minor'))

## 1.3. sort ----

sort.road.a <- 
  import.road %>%
  sf::st_intersection(x = ., 
                      y = poly.msoa) %>% 
  # length of the roads in each msoa
  dplyr::mutate(Length_KM = sf::st_length(geometry) %>% units::set_units(., 'km') %>% as.numeric()) %>% 
  dplyr::select(MSOA11CD, roadType, Length_KM) %>% 
  sf::st_drop_geometry() %>%
  data.table::as.data.table()

sort.road.b1 <-
  data.table::copy(sort.road.a) %>% 
  .[, roadType := 'total'] %>% 
  .[, .(
    Length_KM = Length_KM %>% sum()
  ),
  by = .(MSOA11CD, roadType)]

sort.road.b2 <-
  data.table::copy(sort.road.a) %>% 
  .[, .(
    Length_KM = Length_KM %>% sum()
  ),
  by = .(MSOA11CD, roadType)]

sort.road <- 
  list(sort.road.b1, 
       sort.road.b2) %>% 
  data.table::rbindlist(l = .,
                        use.names = TRUE) %>% 
  merge(x = data.table::CJ(MSOA11CD = poly.msoa$MSOA11CD,
                           roadType = c('major', 'minor', 'total')),
        y = .,
        by = c('MSOA11CD', 'roadType'),
        all.x = TRUE) %>% 
  .[, Length_KM := data.table::fifelse(test = is.na(Length_KM), yes = 0, no = Length_KM)]

## 1.3. save ----

data.table::fwrite(x = sort.road,
                   file = file.path(dir.data.org.road, sprintf('roadwayDensity_msoa11.csv')))

## 1.4. clear ----

rm(files.fullname,
   files.chunk,
   crs.msoa,
   import.road,
   sort.road.a,
   sort.road.b1,
   sort.road.b2,
   sort.road)
gc()

# 2. sort roadway network ----

## 2.1. import ----

data.import <- 
  file.path(dir.data.org.road, sprintf('roadwayDensity_msoa11.csv')) %>% 
  data.table::fread(file = .)

## 2.2. sort ----

### 2.2.1. area ----

area.msoa <- 
  poly.msoa %>% 
  dplyr::mutate(Area_KM2 = sf::st_area(geometry) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
  dplyr::select(MSOA11CD, Area_KM2) %>% 
  sf::st_drop_geometry() %>% 
  data.table::as.data.table()

### 2.2.2. temporary ----

data.sort.tmp <- 
  data.import %>% 
  merge(x = .,
        y = area.msoa,
        by = 'MSOA11CD',
        all.x = TRUE) %>% 
  .[, roadwayDensity := Length_KM/Area_KM2]

### 2.2.3. final ----

data.sort <-
  data.sort.tmp %>% 
  .[roadType == 'total'] %>% 
  .[, .(MSOA11CD, roadwayDensity)]

## 2.3. save ----

data.table::fwrite(x = data.sort,
                   file = file.path(dir.data.org.final, 'MSOA11_ROADWAYDENSITY.csv'))

# 3. plot ----

## 3.1. prepare data ----

data.plot <- 
  file.path(dir.data.org.final, 'MSOA11_ROADWAYDENSITY.csv') %>% 
  data.table::fread() %>% 
  merge(x = .,
        y = lookup.lsoa11_msoa11 %>% .[, .(MSOA11CD, LAD11CD)] %>% unique(),
        by = 'MSOA11CD',
        all.x = TRUE) %>% 
  .[, insideLondon := 
      LAD11CD %>% 
      grepl(pattern = '^E09', x = .) %>% 
      as.integer()] %>% 
  .[, roadType := roadType %>% factor(x = ., levels = c('major', 'minor', 'total'), labels = c('Major', 'Minor', 'Total'))]

## 3.2. prepare plot layer ----

fill.ggLayer <-
  list(
    ggplot2::scale_fill_gradient(name = unname(latex2exp ::TeX(c('Roadway density (per $KM^{2}$)'))),
                                 low = colours.sequential[1], high = colours.sequential[2],
                                 limits = c(0, 40), breaks = c(0, 20, 40),
                                 guide = guide_colourbar(title.position = 'top'))
  )

## 3.3. make animation ----

make.animationPlot(
  data = data.plot, 
  var.frame = 'roadType',
  var.fill = 'roadwayDensity',
  var.zoom = 'insideLondon',
  polygon = poly.msoa,
  polygon.cd = 'MSOA11CD',
  polygon.border = poly.rgn,
  polygon.border.zoom = poly.ldn,
  fill.ggLayer = fill.ggLayer,
  target.filename = 'MSOA11_ROADWAYDENSITY',
  target.dir = dir.res.cov.road
)

## 3.4. average plot ----

data.average <-
  file.path(dir.data.org.final, 'MSOA11_ROADWAYDENSITY.csv') %>% 
  data.table::fread() %>% 
  .[, .(
    roadwayDensity = roadwayDensity %>% mean()
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
    var.fill = 'roadwayDensity',
    var.zoom = 'insideLondon',
    polygon = poly.msoa,
    polygon.cd = 'MSOA11CD',
    polygon.border = poly.rgn,
    polygon.border.zoom = poly.ldn,
    fill.ggLayer = fill.ggLayer,
    text.size = 20
  )

ggplot2::ggsave(
  filename = file.path(dir.res.cov.road, 'MSOA11_ROADWAYDENSITY_AVERAGE.png'),
  plot = plot.average,
  width = width,
  height = height
)

