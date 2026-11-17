# 0. set up ----

## 0.1. packages ----

# data wrangling
library(tidyverse)
library(data.table)
library(fs)

# spatial data
library(sf)
library(spdep)

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

dir.data.org <- file.path(dir.data, 'organised')
fs::dir_create(dir.data.org)

dir.data.org.spa <- file.path(dir.data.org, '01_spatial')
fs::dir_create(dir.data.org.spa)

dir.res.spa <- file.path(dir.res, '01_covariateData', '01_spatial') 
fs::dir_create(dir.res.spa)

## 0.3. functions ----

source(file.path(dir.code, '00_functions.R'))

# 1. import shape files ---- 

poly.lsoa <- 
  file.path(dir.data, 'spatial', 'ons11_LSOA') %>% 
  sf::st_read(dsn = ., layer = 'ons11_LSOA') %>% 
  dplyr::filter(str_detect(LSOA11CD, '^E'))

poly.msoa <- 
  file.path(dir.data, 'spatial', 'ons11_MSOA') %>% 
  sf::st_read(dsn = ., layer = 'ons11_MSOA') %>% 
  dplyr::filter(str_detect(MSOA11CD, '^E'))

poly.lad <- 
  file.path(dir.data, 'spatial', 'ons11_LAD') %>% 
  sf::st_read(dsn = ., layer = 'ons11_LAD') %>% 
  dplyr::filter(str_detect(LAD11CD, '^E'))

poly.rgn <-
  file.path(dir.data, 'spatial', 'ons21_RGN') %>% 
  sf::st_read(dsn = ., layer = 'ons21_RGN') %>% 
  dplyr::filter(str_detect(RGN21CD, '^E')) %>% 
  dplyr::rename_with(~ stringr::str_replace(.x, '^RGN21', 'RGN11'))

poly.ldn <-
  poly.rgn %>% 
  dplyr::filter(RGN11NM == 'London')

# 2. create adjacent matrices ----

mat.lsoa.temp <- spdep::poly2nb(as(poly.lsoa, 'Spatial'))
mat.lsoa <- spdep::nb2mat(mat.lsoa.temp, zero.policy = TRUE)
colnames(mat.lsoa) <- rownames(mat.lsoa) <- paste0('lsoa_', 1:dim(mat.lsoa)[1])

mat.msoa.temp <- spdep::poly2nb(as(poly.msoa, 'Spatial'))
mat.msoa <- spdep::nb2mat(mat.msoa.temp, zero.policy = TRUE)
colnames(mat.msoa) <- rownames(mat.msoa) <- paste0('msoa_', 1:dim(mat.msoa)[1])

mat.lad.temp <- spdep::poly2nb(as(poly.lad, 'Spatial'))
mat.lad <- spdep::nb2mat(mat.lad.temp, zero.policy = TRUE)
colnames(mat.lad) <- rownames(mat.lad) <- paste0('lad_', 1:dim(mat.lad)[1])

mat.rgn.temp <- spdep::poly2nb(as(poly.rgn, 'Spatial'))
mat.rgn <- spdep::nb2mat(mat.rgn.temp, zero.policy = TRUE)
colnames(mat.rgn) <- rownames(mat.rgn) <- paste0('rgn_', 1:dim(mat.rgn)[1])

saveRDS(object = mat.lsoa,
        file = file.path(dir.data.org.spa, 'amat_lsoa.rds'))

saveRDS(object = mat.msoa,
        file = file.path(dir.data.org.spa, 'amat_msoa.rds'))

saveRDS(object = mat.lad,
        file = file.path(dir.data.org.spa, 'amat_lad.rds'))

saveRDS(object = mat.rgn,
        file = file.path(dir.data.org.spa, 'amat_rgn.rds'))

# 3. create additional information ----

info.lsoa <- 
  data.frame(LSOA11CD = poly.lsoa$LSOA11CD,
             LSOA11NM = poly.lsoa$LSOA11NM) %>%
  dplyr::left_join(., 
                   poly.lsoa %>% 
                     dplyr::mutate(Area_M2 = sf::st_area(poly.lsoa) %>% as.numeric(),
                                   Area_KM2 = sf::st_area(poly.lsoa) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
                     dplyr::select(LSOA11CD, Area_M2, Area_KM2) %>% 
                     sf::st_drop_geometry(),
                   by = 'LSOA11CD') %>% 
  dplyr::mutate(Internal = rownames(mat.lsoa))

info.msoa <- 
  data.frame(MSOA11CD = poly.msoa$MSOA11CD,
             MSOA11NM = poly.msoa$MSOA11NM) %>%
  dplyr::left_join(., 
                   poly.msoa %>% 
                     dplyr::mutate(Area_M2 = sf::st_area(poly.msoa) %>% as.numeric(),
                                   Area_KM2 = sf::st_area(poly.msoa) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
                     dplyr::select(MSOA11CD, Area_M2, Area_KM2) %>% 
                     sf::st_drop_geometry(),
                   by = 'MSOA11CD') %>% 
  dplyr::mutate(Internal = rownames(mat.msoa))

info.lad <- 
  data.frame(LAD11CD = poly.lad$LAD11CD,
             LAD11NM = poly.lad$LAD11NM) %>%
  dplyr::left_join(., 
                   poly.lad %>% 
                     dplyr::mutate(Area_M2 = sf::st_area(poly.lad) %>% as.numeric(),
                                   Area_KM2 = sf::st_area(poly.lad) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
                     dplyr::select(LAD11CD, Area_M2, Area_KM2) %>% 
                     sf::st_drop_geometry(),
                   by = 'LAD11CD') %>% 
  dplyr::mutate(Internal = rownames(mat.lad))

info.rgn <- 
  data.frame(RGN11CD = poly.rgn$RGN11CD,
             RGN11NM = poly.rgn$RGN11NM) %>%
  dplyr::left_join(., 
                   poly.rgn %>% 
                     dplyr::mutate(Area_M2 = sf::st_area(poly.rgn) %>% as.numeric(),
                                   Area_KM2 = sf::st_area(poly.rgn) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
                     dplyr::select(RGN11CD, Area_M2, Area_KM2) %>% 
                     sf::st_drop_geometry(),
                   by = 'RGN11CD') %>% 
  dplyr::mutate(Internal = rownames(mat.rgn))

data.table::fwrite(x = info.lsoa,
                   file = file.path(dir.data.org.spa, 'amat_info_lsoa.csv'))

data.table::fwrite(x = info.msoa,
                   file = file.path(dir.data.org.spa, 'amat_info_msoa.csv'))

data.table::fwrite(x = info.lad,
                   file = file.path(dir.data.org.spa, 'amat_info_lad.csv'))

data.table::fwrite(x = info.rgn,
                   file = file.path(dir.data.org.spa, 'amat_info_rgn.csv'))

# 4. neighbourhoods ----

## 4.1. functions ----

create.buffer.nbs <-
  function(polygon, 
           polygon.cd,
           polygon.crs, 
           buffer.radius.m){
    
    # 0. function arguments ----
    
    # polygon <- poly.lsoa
    # polygon.cd <- 'LSOA11CD'
    # polygon.crs <- 27700
    # buffer.radius.m <- 25000
    
    # 1. define polygon centroids ----
    
    centroids <- 
      sf::st_centroid(polygon) %>%
      sf::st_transform(polygon.crs)
    
    # distance matrix (units: meters)
    centroid.distance <- 
      sf::st_distance(centroids)
    
    # 20 km buffer radius in meters
    buffer.radius <- units::set_units(buffer.radius.m, 'm')
    
    # 2. neibourhood list ----
    
    nbs.list <- 
      centroid.distance %>% nrow() %>% seq_len() %>% 
      lapply(X = ., 
             FUN = function(i) {
               
               # 0. function arguments ----
               
               # i <- 1
               
               # 1. identify neighbours ----
               
               # all neighbors within 20 km, excluding self
               nbs.index <- 
                 which(centroid.distance[i, ] <= buffer.radius & seq_len(nrow(centroids)) != i)
               
               # 2. expanding buffer if no neighbours ----
               
               if(length(nbs.index) == 0) {
                 # if no neighbors: find nearest one (exclude self)
                 nearest.index <- which.min(replace(centroid.distance[i, ], i, Inf))
                 
                 message('Expanding buffer for LSOA ', 
                         polygon[[polygon.cd]][i], 
                         ' to include nearest neighbor at distance ',
                         centroid.distance[i, nearest.index] %>% as.numeric() %>% round(x = ., digits = 2), 
                         ' m')
                 
                 nbs.index <- nearest.index
               }
               
               # 3. neighbours as name ----
               
               nbs.name <- 
                 polygon[[ polygon.cd ]][nbs.index]
               
               # 4. return ----
               
               return(nbs.name)
               
             }) %>% 
      stats::setNames(object = .,
                      nm = polygon[[polygon.cd]])
    
    # 3. return ----
    
    return(nbs.list)
    
  }


create.border.nbs <- 
  function(polygon,
           polygon.cd,
           order = 1,
           method = c('touches', 'intersects')[1]) {
    
    # 0. function arguments ----
    
    # polygon <- poly.lad
    # polygon.cd <- 'LAD11CD'
    # order <- 3
    # method <- c('touches', 'intersects')[1]
    
    # 1. define adjaceny list ----
    
    adj <- 
      switch(
        EXPR = method,
        touches = sf::st_touches(polygon),
        intersects = sf::st_intersects(polygon)
      ) %>% 
      lapply(X = .,
             FUN = function(x) unlist(x))
    
    # 2. define neighbours ----
    
    nbs.list <-
      lapply(X = seq_along(adj),
             FUN = function(i) {
               
               # 0. function argument ----
               
               # i <- 1
               
               # 1. first order neighbours ----
               
               nbs.index <- adj[[i]]
               
               # 2. n-th order neighbours ----
               
               if(order > 1) {
                 
                 nbs.current <- nbs.index
                 
                 for (j in 2:order) {
                   
                   # 0. function argument ----
                   
                   # j <- 1
                   
                   # 1. define all new neighbours ----
                   
                   nbs.new <- adj[nbs.current] %>% unlist() %>% unique()
                   
                   # 2. remove self and known neighbours ----
                   
                   nbs.new <- setdiff(x = nbs.new, y = c(i, nbs.current))
                   
                   # 3. update list of current neighbours ----
                   
                   nbs.current <- c(nbs.current, nbs.new) %>% unique()
                   
                 }
                 
                 nbs.index <- nbs.current
                 
               }
               
               # 3. remove self ----
               
               # remove self if accidentally included
               nbs.index <- nbs.index[nbs.index != i]
               
               # 4. neighbours name ----
               
               nbs.name <- 
                 polygon[[ polygon.cd ]][nbs.index]
               
               # 5. return ----
               
               return(nbs.name)
               
             }) %>% 
      stats::setNames(object = .,
                      nm = polygon[[polygon.cd]])
    
    # 3. return ----
    
    return(nbs.list)
    
  }

## 4.2. define ----

nbs.first.lsoa <-
  create.border.nbs(polygon = poly.lsoa,
                    polygon.cd = 'LSOA11CD',
                    order = 1)

nbs.second.lsoa <-
  create.border.nbs(polygon = poly.lsoa,
                    polygon.cd = 'LSOA11CD',
                    order = 2)

nbs.first.msoa <-
  create.border.nbs(polygon = poly.msoa,
                    polygon.cd = 'MSOA11CD',
                    order = 1)

nbs.second.msoa <-
  create.border.nbs(polygon = poly.msoa,
                    polygon.cd = 'MSOA11CD',
                    order = 2)

nbs.first.lad <-
  create.border.nbs(polygon = poly.lad,
                    polygon.cd = 'LAD11CD',
                    order = 1)

nbs.second.lad <-
  create.border.nbs(polygon = poly.lad,
                    polygon.cd = 'LAD11CD',
                    order = 2)

nbs.first.rgn <-
  create.border.nbs(polygon = poly.rgn,
                    polygon.cd = 'RGN11CD',
                    order = 1)

nbs.second.rgn <-
  create.border.nbs(polygon = poly.rgn,
                    polygon.cd = 'RGN11CD',
                    order = 2)

saveRDS(object = nbs.first.lsoa,
        file = file.path(dir.data.org.spa, 'neighbours_first_order_lsoa.rds'))

saveRDS(object = nbs.second.lsoa,
        file = file.path(dir.data.org.spa, 'neighbours_second_order_lsoa.rds'))

saveRDS(object = nbs.first.msoa,
        file = file.path(dir.data.org.spa, 'neighbours_first_order_msoa.rds'))

saveRDS(object = nbs.second.msoa,
        file = file.path(dir.data.org.spa, 'neighbours_second_order_msoa.rds'))

saveRDS(object = nbs.first.lad,
        file = file.path(dir.data.org.spa, 'neighbours_first_order_lad.rds'))

saveRDS(object = nbs.second.lad,
        file = file.path(dir.data.org.spa, 'neighbours_second_order_lad.rds'))

saveRDS(object = nbs.first.rgn,
        file = file.path(dir.data.org.spa, 'neighbours_first_order_rgn.rds'))

saveRDS(object = nbs.second.rgn,
        file = file.path(dir.data.org.spa, 'neighbours_second_order_rgn.rds'))

# 5. plot ----

## 5.1. plot arguments ----

plot.text <- 20

plot.style.msoa <- list(colour = 'blue3',  linewidth = 0.3)
plot.style.lad <- list(colour = 'red3',   linewidth = 0.6)
plot.style.rgn <- list(colour = 'black',  linewidth = 1)

## 5.2. plots ----

plot.msoa <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.msoa,
                   fill = NA, 
                   colour = plot.style.msoa$colour,
                   linewidth = plot.style.msoa$linewidth) +
  my.map.theme()

plot.lad <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.lad,
                   fill = NA, 
                   colour = plot.style.lad$colour,
                   linewidth = plot.style.lad$linewidth) +
  my.map.theme()

plot.rgn <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.rgn,
                   fill = NA,
                   colour = plot.style.rgn$colour,
                   linewidth = 1) +
  ggrepel::geom_label_repel(data = poly.rgn,
                            aes(label = RGN11NM, geometry = geometry),
                            stat = 'sf_coordinates',
                            min.segment.length = 0,
                            colour = plot.style.rgn$colour,
                            fontface = 'bold') +
  my.map.theme(text = ggplot2::element_text(size = plot.text))

plot.msoa.lad <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.msoa, 
                   fill = NA,
                   color = plot.style.msoa$colour,
                   linewidth = plot.style.msoa$linewidth) +
  ggplot2::geom_sf(data = poly.lad, 
                   fill = NA,
                   color = plot.style.lad$colour,
                   linewidth = plot.style.lad$linewidth) +
  my.map.theme()

plot.msoa.lad.rgn <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.msoa, 
                   fill = NA,
                   color = plot.style.msoa$colour,
                   linewidth = plot.style.msoa$linewidth) +
  ggplot2::geom_sf(data = poly.lad, 
                   fill = NA,
                   color = plot.style.lad$colour,
                   linewidth = plot.style.lad$linewidth) +
  ggplot2::geom_sf(data = poly.rgn, 
                   fill = NA,
                   color = plot.style.rgn$colour,
                   linewidth = plot.style.rgn$linewidth) +
  my.map.theme()

## 5.3. save ----

ggplot2::ggsave(
  plot = plot.msoa,
  filename = file.path(dir.res.spa, 'geography_msoa.png'),
  width = 10,
  height = 10
)

ggplot2::ggsave(
  plot = plot.lad,
  filename = file.path(dir.res.spa, 'geography_lad.png'),
  width = 10,
  height = 10
)

ggplot2::ggsave(
  plot = plot.rgn,
  filename = file.path(dir.res.spa, 'geography_rgn.png'),
  width = 10,
  height = 10
)

ggplot2::ggsave(
  plot = plot.msoa.lad,
  filename = file.path(dir.res.spa, 'geography_msoa_lad.png'),
  width = 10,
  height = 10
)

ggplot2::ggsave(
  plot = plot.msoa.lad.rgn,
  filename = file.path(dir.res.spa, 'geography_msoa_lad_rgn.png'),
  width = 10,
  height = 10
)
