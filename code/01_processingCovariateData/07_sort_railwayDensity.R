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

dir.data.org.rail <- file.path(dir.data, 'organised', '07_railwayDensity')
fs::dir_create(dir.data.org.rail)

dir.data.org.final <- file.path(dir.data, 'organised', '99_final')
fs::dir_create(dir.data.org.final)

dir.res.cov.rail <- file.path(dir.res, '01_covariateData', '07_railwayDensity')
fs::dir_create(dir.res.cov.rail)


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

## 1.1. import ----

import.rail <- 
  file.path(dir.data, 'railwayNetwork') %>% 
  sf::st_read(dsn = ., layer = 'GBR_rails')
  
## 1.2. sort ----

sort.rail.a <- 
  import.rail %>%
  # transform CRS to that from ONS (load ONS librarys first)
  sf::st_transform(., crs = sf::st_crs(poly.msoa)) %>% 
  dplyr::mutate(railType = dplyr::if_else(EXS_DESCRI == 'Operational', 'operational', 'unexamined')) %>% 
  sf::st_intersection(x = ., 
                      y = poly.msoa) %>% 
  # length of the rails in each msoa
  dplyr::mutate(Length_KM = sf::st_length(geometry) %>% units::set_units(., 'km') %>% as.numeric()) %>% 
  dplyr::select(MSOA11CD, railType, Length_KM) %>% 
  sf::st_drop_geometry() %>%
  data.table::as.data.table()

sort.rail.b1 <-
  data.table::copy(sort.rail.a) %>% 
  .[, railType := 'total'] %>% 
  .[, .(
    Length_KM = Length_KM %>% sum()
  ),
  by = .(MSOA11CD, railType)]

sort.rail.b2 <-
  data.table::copy(sort.rail.a) %>% 
  .[, .(
    Length_KM = Length_KM %>% sum()
  ),
  by = .(MSOA11CD, railType)]

sort.rail <- 
  list(sort.rail.b1, 
       sort.rail.b2) %>% 
  data.table::rbindlist(l = .,
                        use.names = TRUE) %>% 
  merge(x = data.table::CJ(MSOA11CD = poly.msoa$MSOA11CD,
                           railType = c('operational', 'unexamined', 'total')),
        y = .,
        by = c('MSOA11CD', 'railType'),
        all.x = TRUE) %>% 
  .[, Length_KM := data.table::fifelse(test = is.na(Length_KM), yes = 0, no = Length_KM)]
  
## 1.3. save ----

data.table::fwrite(x = sort.rail,
                   file = file.path(dir.data.org.rail, sprintf('railwayDensity_msoa11.csv')))

## 1.4. clear ----

rm(import.rail,
   sort.rail.a,
   sort.rail.b1,
   sort.rail.b2,
   sort.rail)
gc()

# 2. sort railway network ----

## 2.1. import ----

data.import <- 
  file.path(dir.data.org.rail, sprintf('railwayDensity_msoa11.csv')) %>% 
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
  .[, railwayDensity := Length_KM/Area_KM2]

### 2.2.3. final ----

data.sort <-
  data.sort.tmp %>% 
  .[railType == 'total'] %>% 
  .[, .(MSOA11CD, railwayDensity)]

## 2.3. save ----

data.table::fwrite(x = data.sort,
                   file = file.path(dir.data.org.final, 'MSOA11_RAILWAYDENSITY.csv'))

# 3. plot ----

## 3.1. prepare data ----

data.plot <- 
  file.path(dir.data.org.final, 'MSOA11_RAILWAYDENSITY.csv') %>% 
  data.table::fread() %>% 
  merge(x = .,
        y = lookup.lsoa11_msoa11 %>% .[, .(MSOA11CD, LAD11CD)] %>% unique(),
        by = 'MSOA11CD',
        all.x = TRUE) %>% 
  .[, insideLondon := 
      LAD11CD %>% 
      grepl(pattern = '^E09', x = .) %>% 
      as.integer()] %>% 
  .[, railType := railType %>% factor(x = ., levels = c('operational', 'unexamined', 'total'), labels = c('Operational', 'Unexamined', 'Total'))]

## 3.2. prepare plot layer ----

fill.ggLayer <-
  list(
    ggplot2::scale_fill_gradient(name = unname(latex2exp ::TeX(c('Railway density (per $KM^{2}$)'))),
                                 low = colours.sequential[1], high = colours.sequential[2],
                                 limits = c(0, 4), breaks = c(0, 2, 4),
                                 guide = guide_colourbar(title.position = 'top'))
  )

## 3.3. make animation ----

make.animationPlot(
  data = data.plot, 
  var.frame = 'railType',
  var.fill = 'railwayDensity',
  var.zoom = 'insideLondon',
  polygon = poly.msoa,
  polygon.cd = 'MSOA11CD',
  polygon.border = poly.rgn,
  polygon.border.zoom = poly.ldn,
  fill.ggLayer = fill.ggLayer,
  target.filename = 'MSOA11_RAILWAYDENSITY',
  target.dir = dir.res.cov.rail
)

## 3.4. average plot ----

data.average <-
  file.path(dir.data.org.final, 'MSOA11_RAILWAYDENSITY.csv') %>% 
  data.table::fread() %>% 
  .[, .(
    railwayDensity = railwayDensity %>% mean()
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
    var.fill = 'railwayDensity',
    var.zoom = 'insideLondon',
    polygon = poly.msoa,
    polygon.cd = 'MSOA11CD',
    polygon.border = poly.rgn,
    polygon.border.zoom = poly.ldn,
    fill.ggLayer = fill.ggLayer,
    text.size = 20
  )

ggplot2::ggsave(
  filename = file.path(dir.res.cov.rail, 'MSOA11_RAILWAYDENSITY_AVERAGE.png'),
  plot = plot.average,
  width = width,
  height = height
)

