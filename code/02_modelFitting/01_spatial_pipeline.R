# 0 set up ----

## 0.1. packages ----

if(!require('tidyverse')) {
  install.packages('tidyverse', dependencies = TRUE)
}
if(!require('data.table')) {
  install.packages('data.table', dep = TRUE)
}
if(!require('sf')) {
  install.packages('xtable', dependencies = TRUE)
}
## 0.2. directories ----

# retrieve directories
dir.path <- rstudioapi::getActiveDocumentContext()$path
dir.home <- sub('(Policy_Evaluation/02_itsAnalysis).*', '\\1', dir.path)
dir.code <- file.path(dir.home, 'code')
dir.data <- file.path(dir.home, 'data')
dir.data.organised <- file.path(dir.data, 'organised')
dir.data.spatial <- file.path(dir.data, 'spatial')

## 0.3. imports ----

### 0.3.1. functions ----

source(file.path(dir.code, '00_functions.R'))

### 0.3.2. shape file ----

#### 0.3.2.1. full polygons ----

poly.lsoa <- sf::st_read(dsn = dir.data.spatial, layer = 'ONS11_LSOA')
poly.msoa <- sf::st_read(dsn = dir.data.spatial, layer = 'ONS11_MSOA')
poly.lad <- sf::st_read(dsn = dir.data.spatial, layer = 'ONS11_LAD')
poly.rgn <- sf::st_read(dsn = dir.data.spatial, layer = 'ONS21_RGN')

#### 0.3.2.1. england only ----

poly.lsoa.england <- poly.lsoa %>% dplyr::filter(stringr::str_detect(LSOA11CD, '^E'))
poly.msoa.england <- poly.msoa %>% dplyr::filter(stringr::str_detect(MSOA11CD, '^E'))
poly.lad.england <- poly.lad %>% dplyr::filter(stringr::str_detect(LAD11CD, '^E'))
poly.rgn.england <- poly.rgn %>% dplyr::filter(stringr::str_detect(RGN21CD, '^E'))

# 1. organise spatial files ----

## 1.1. adjacent matrices ----

mat.lsoa.temp <- spdep::poly2nb(as(poly.lsoa.england, 'Spatial'))
mat.lsoa <- spdep::nb2mat(mat.lsoa.temp, zero.policy = TRUE)
colnames(mat.lsoa) <- rownames(mat.lsoa) <- paste0('lsoa_', 1:nrow(poly.lsoa.england))

mat.msoa.temp <- spdep::poly2nb(as(poly.msoa.england, 'Spatial'))
mat.msoa <- spdep::nb2mat(mat.msoa.temp, zero.policy = TRUE)
colnames(mat.msoa) <- rownames(mat.msoa) <- paste0('msoa_', 1:nrow(poly.msoa.england))

mat.lad.temp <- spdep::poly2nb(as(poly.lad.england, 'Spatial'))
mat.lad <- spdep::nb2mat(mat.lad.temp, zero.policy = TRUE)
colnames(mat.lad) <- rownames(mat.lad) <- paste0('lad_', 1:nrow(poly.lad.england))

mat.rgn.temp <- spdep::poly2nb(as(poly.rgn.england, 'Spatial'))
mat.rgn <- spdep::nb2mat(mat.rgn.temp, zero.policy = TRUE)
colnames(mat.rgn) <- rownames(mat.rgn) <- paste0('rgn_', 1:nrow(poly.rgn.england))

## 1.2. information data frames ----

info.lsoa <-
  data.table::data.table(LSOA11CD = poly.lsoa.england$LSOA11CD,
                         LSOA11NM = poly.lsoa.england$LSOA11NM) %>% 
  merge(x = ,
        y = 
          poly.lsoa.england %>% 
          dplyr::mutate(Area_M2 = sf::st_area(poly.lsoa.england) %>% as.numeric(),
                        Area_KM2 = sf::st_area(poly.lsoa.england) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
          sf::st_drop_geometry() %>% 
          data.table::as.data.table() %>% 
          .[, .(LSOA11CD, Area_M2, Area_KM2)],
        by = 'LSOA11CD',
        all.x = TRUE) %>% 
  data.table::as.data.table() %>% 
  .[, Internal := mat.lsoa %>% rownames()]

info.msoa <-
  data.table::data.table(MSOA11CD = poly.msoa.england$MSOA11CD,
                         MSOA11NM = poly.msoa.england$MSOA11NM) %>% 
  merge(x = ,
        y = 
          poly.msoa.england %>% 
          dplyr::mutate(Area_M2 = sf::st_area(poly.msoa.england) %>% as.numeric(),
                        Area_KM2 = sf::st_area(poly.msoa.england) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
          sf::st_drop_geometry() %>% 
          data.table::as.data.table() %>% 
          .[, .(MSOA11CD, Area_M2, Area_KM2)],
        by = 'MSOA11CD',
        all.x = TRUE) %>% 
  data.table::as.data.table() %>% 
  .[, Internal := mat.msoa %>% rownames()]

info.lad <-
  data.table::data.table(LAD11CD = poly.lad.england$LAD11CD,
                         LAD11NM = poly.lad.england$LAD11NM) %>% 
  merge(x = ,
        y = 
          poly.lad.england %>% 
          dplyr::mutate(Area_M2 = sf::st_area(poly.lad.england) %>% as.numeric(),
                        Area_KM2 = sf::st_area(poly.lad.england) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
          sf::st_drop_geometry() %>% 
          data.table::as.data.table() %>% 
          .[, .(LAD11CD, Area_M2, Area_KM2)],
        by = 'LAD11CD',
        all.x = TRUE) %>% 
  data.table::as.data.table() %>% 
  .[, Internal := mat.lad %>% rownames()]

# RGN11 is the same as RGN21
info.rgn <-
  data.table::data.table(RGN11CD = poly.rgn.england$RGN21CD,
                         RGN11NM = poly.rgn.england$RGN21CD) %>% 
  merge(x = ,
        y = 
          poly.rgn.england %>% 
          dplyr::mutate(RGN11CD = RGN21CD,
                        Area_M2 = sf::st_area(poly.rgn.england) %>% as.numeric(),
                        Area_KM2 = sf::st_area(poly.rgn.england) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
          sf::st_drop_geometry() %>% 
          data.table::as.data.table() %>% 
          .[, .(RGN11CD, Area_M2, Area_KM2)],
        by = 'RGN11CD',
        all.x = TRUE) %>% 
  data.table::as.data.table() %>% 
  .[, Internal := mat.rgn %>% rownames()]

## 1.3. save ----

# save as .rda as we want to load these in with same name and same format
save(mat.lsoa, mat.msoa, mat.lad, mat.rgn, 
     file = file.path(dir.data.organised, 'ENGLAND_AMAT_LSOA_MSOA_LAD_RGN.rda'))
save(info.lsoa, info.msoa, info.lad, info.rgn, 
     file = file.path(dir.data.organised, 'ENGLAND_SPATIAL_INFORMATION_LSOA_MSOA_LAD_RGN.rda'))