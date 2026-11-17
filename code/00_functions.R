# helper timing functions ----

helper.formatTime <- 
  function(time.end, 
           time.start) {
    
    # 1. time difference ----
    
    time.diff <- 
      difftime(time1 = time.end, time2 = time.start, units = 'secs') %>% 
      as.numeric()
    
    # 2. time lengths -----
    
    hours   <- floor(time.diff / 3600)
    minutes <- floor((time.diff %% 3600) / 60)
    seconds <- round(time.diff %% 60)
    
    # 3. return (message) ----
    
    sprintf('%02d hours %02d minutes %02d seconds', 
            hours, minutes, seconds)
    
  }

# helper load or run ----

helper.loadOrRun <- 
  function(filepath, 
           FUN, 
           message.run, 
           message.load) {
    
    # 0. function arguments ----
    
    # filepath
    # FUN
    # message.run
    # message.load
    
    # 1. load or run ----
    
    helper.nIndent <- 
      function(txt) {
        
        regmatches(
          x = txt, 
          m = regexpr(pattern = '^\\s*', text = txt)
        )
        
      }
    
    if (!file.exists(filepath)) {
      
      message(message.run)
      t0 <- Sys.time()
      obj <- FUN()
      t1 <- Sys.time()
      sprintf('%s time to run: %s', 
              helper.nIndent(message.run),
              helper.formatTime(t1, t0)) %>% 
        message()
      
      t0 <- Sys.time()
      dir.filepath <- filepath %>% dirname()
      fs::dir_create(dir.filepath)
      saveRDS(obj, filepath)
      t1 <- Sys.time()
      sprintf('%s time to save: %s', 
              helper.nIndent(message.run),
              helper.formatTime(t1, t0)) %>% 
        message()
      
    } else {
      
      message(message.load)
      t0 <- Sys.time()
      obj <- readRDS(filepath)
      t1 <- Sys.time()
      sprintf('%s time to load: %s', 
              helper.nIndent(message.load),
              helper.formatTime(t1, t0)) %>% 
        message()
      
    }
    
    # 2. return ----
    
    return(obj)
    
  }

# helper clear source ----

helper.cleanSource <-
  function(
    filepath,
    envir = .GlobalEnv,
    verbose = TRUE) {
    
    # 1. snapshot environment before ----
    
    envir.before <- ls(envir = envir, all.names = TRUE)
    
    # 2. source script ----
    
    source(filepath)
    
    # 3. snapshot environment after ----
    
    envir.after <- ls(envir = envir, all.names = TRUE)
    
    # 4. differences in environment ----
    
    envir.newobjs <- setdiff(x = envir.after, y = envir.before)
    
    if(verbose){
      if(length(envir.newobjs)){
        sprintf('Objects created by %s:\n', basename(filepath)) %>% cat()
        print(envir.newobjs)
      } else{
        sprintf('No new objects created by %s.\n', basename(filepath)) %>% cat()
      }
    }
    
    # 5. clear difference ----
    
    rm(list = envir.newobjs, envir = envir)
    gc()
    
    # 6. return ----
    
    invisible(envir.newobjs)
    
  }

# helper plot theme ----

helper.plotTheme <- function(...){
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.line = ggplot2::element_line(colour = 'black'),
                 legend.text = ggplot2::element_text(hjust = 0),
                 legend.key = ggplot2::element_rect(fill = NA, colour = NA),
                 ...)
}

# helper map theme ----

helper.mapTheme <- function(...){
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(hjust = 0),
                 legend.key = ggplot2::element_rect(fill = NA, colour = NA),
                 panel.background = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 ...)
}

# make single plot ----

make.singlePlot <-
  function(data,
           var.fill,
           var.zoom = NULL,
           polygon,
           polygon.cd,
           polygon.border = NULL,
           polygon.border.zoom = NULL,
           fill.ggLayer = NULL,
           text.size = 20) {
    
    # 0. function argument ----
    
    # data <- data.average
    # var.fill <- 'deprivation'
    # var.zoom <- 'insideLondon'
    # polygon <- poly.msoa
    # polygon.cd <- 'MSOA11CD'
    # polygon.border <- poly.rgn
    # polygon.border.zoom <- poly.ldn
    # fill.ggLayer <- fill.ggLayer
    # text.size <- 20
    
    # 1. data to sf ----
    
    ## 1.1. main ----
    
    data.sf <-
      data %>%
      merge(x = .,
            y = polygon,
            by = polygon.cd) %>%
      sf::st_as_sf()
    
    ## 1.2. zoom (optional) ----
    
    if (!is.null(var.zoom)) {
      
      border.ggLayer.zoom <-
        if (!is.null(polygon.border.zoom)) {
          ggplot2::geom_sf(data = polygon.border.zoom,
                           fill = NA,
                           colour = 'black',
                           linewidth = 1)
        } else {
          NULL
        }
      
      data.sf.sep <-
        data.sf %>%
        dplyr::filter(!!sym(var.zoom) == 1)
      
      plot.sep <-
        ggplot2::ggplot() +
        ggplot2::geom_sf(
          data = data.sf.sep,
          ggplot2::aes(fill = !!sym(var.fill)),
          colour = NA
        ) +
        border.ggLayer.zoom +
        fill.ggLayer +
        my.map.theme(legend.position = 'none')
      
      rm(data.sf.sep)
    }
    
    # 2. main plot ----
    
    border.ggLayer <-
      if (!is.null(polygon.border)) {
        ggplot2::geom_sf(data = polygon.border,
                         fill = NA,
                         colour = 'black',
                         linewidth = 1)
      } else {
        NULL
      }
    
    plot <-
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = data.sf,
                       ggplot2::aes(fill = !!sym(var.fill)),
                       colour = NA) +
      border.ggLayer +
      fill.ggLayer +
      my.map.theme(
        text = ggplot2::element_text(size = text.size),
        legend.position = 'bottom',
        legend.key.width = grid::unit(2, 'cm'),
        legend.title = ggplot2::element_text(hjust = 0.5)
      )
    
    # 3. add zoom inset (optional) ----
    
    if (!is.null(var.zoom)) {
      plot <-
        plot +
        ggplot2::geom_rect(
          ggplot2::aes(
            xmin = 490000, xmax = 560000,
            ymin = 150000, ymax = 210000
          ),
          color = 'red',
          linewidth = 1,
          fill = NA
        ) +
        ggplot2::annotation_custom(
          ggplot2::ggplotGrob(plot.sep),
          xmin = 82700, xmax = 300000,
          ymin = 160000, ymax = 450000
        )
    }
    
    # 4. clear & return ----
    
    rm(data.sf)
    gc()
    
    return(plot)
  }

# make animated plot ----

make.animationPlot <-
  function(data,
           var.frame,
           var.fill,
           var.zoom = NULL,
           polygon,
           polygon.cd,
           polygon.border = NULL,
           polygon.border.zoom = NULL,
           fill.ggLayer = NULL,
           text.size = 20,
           width = 10,
           height = 10,
           target.filename,
           target.dir = getwd()) {
    
    # 1. output file ----
    
    outfile <- file.path(target.dir, sprintf('%s.gif', target.filename))
    
    if (file.exists(outfile)) {
      message(sprintf('Animation %s already exists. Skipped.', target.filename))
      return(invisible(NULL))
    }
    
    # 2. split data ----
    
    message(sprintf('Animating %s...', target.filename))
    
    data.split <-
      split(data, by = var.frame)
    
    # 3. plot each frame ----
    
    plots <-
      lapply(X = data.split,
             FUN = function(y) {
               
               name.frame <-
                 y %>%
                 .[[var.frame]] %>%
                 unique()
               
               message(sprintf(' plotting %s...', name.frame))
               
               # call core plot function, then add frame label on top
               make.singlePlot(
                 data = y,
                 var.fill = var.fill,
                 var.zoom = var.zoom,
                 polygon = polygon,
                 polygon.cd = polygon.cd,
                 polygon.border = polygon.border,
                 polygon.border.zoom = polygon.border.zoom,
                 fill.ggLayer = fill.ggLayer,
                 text.size = text.size
               ) +
                 ggplot2::annotate('text',
                                   x = Inf, y = Inf,
                                   label = name.frame,
                                   hjust = 1.05, vjust = 1.1,
                                   color = 'grey',
                                   size = 7.5)
             })
    
    rm(data.split)
    gc()
    
    # 4. save as GIF ----
    
    message(sprintf(' saving as %s.gif...', target.filename))
    
    animation::saveGIF(
      expr = {
        for (i in seq_along(plots)) print(plots[[i]])
      },
      movie.name = outfile,
      interval   = 0.5,
      ani.width  = width * 100,
      ani.height = height * 100
    )
  }

# polygon imputing neighbours ----

polygon.imputeNeighbours <- 
  function(data, 
           polygon.cd = NULL, 
           polygon.nbs) {
    
    # 0. function arguments ----
    
    # data <- imd.04
    # polygon.cd <- 'LSOA11CD'
    # polygon.nbs <- nbs.first.lsoa
    
    # 1. set up ----
    
    ## 1.1. saftey check ----
    
    if (is.null(polygon.cd) || !(polygon.cd %in% names(data))) {
      stop(sprintf("Column '%s' not found in data. Available columns: %s",
                   polygon.cd,
                   paste(names(data), collapse = ', ')))
    }
    
    # 1.2. sort data ----
    
    value.columns <- setdiff(names(data), polygon.cd)
    imputed <- data.table::copy(data)
    
    # 2. impute ----
    
    ## 2.0. function ----
    
    imput.bufferNbs.columns <- function(x) {
      
      # 0. function arguments ----
      
      # x <- data.table::copy(data)[['imdScore']]
      
      # 1. missing index ----
      
      missing.idx <- which(is.na(x) | is.nan(x))
      
      # 2. impute using names ----
      
      if(length(missing.idx) == 0) { 
        
        imputed.column <- x
        
      } else {
        
        imputed.column <- x
        
        values.imputed <- 
          lapply(X = missing.idx, 
                 FUN = function(i) {
                   
                   # 0. function arguments ----
                   
                   # i <- missing.idx[1]
                   
                   # 1. identify row of missing lsoa ----
                   
                   selected.code <- imputed[[polygon.cd]][i]
                   
                   # 2. identify neighbors ----
                   
                   selected.nbs <- polygon.nbs[[selected.code]]
                   selected.nb.idx <- which(imputed[[polygon.cd]] %in% selected.nbs)
                   vals <- x[selected.nb.idx]
                   
                   # 3. imputing ----
                   
                   # drop NA values
                   vals <- vals[!is.na(vals)]
                   
                   # if no valid neighbors, return NA
                   if (length(vals) == 0) {
                     imputed.element <- NA_real_
                   } else {
                     imputed.element <- mean(vals)
                   }
                   
                   # 4. return ----
                   
                   return(imputed.element)
                   
                 }) %>% 
          unlist()
        
        imputed.column[missing.idx] <- values.imputed
        
      }
      
      # 3. return ----
      
      return(imputed.column)
      
    }
    
    # 2.1. run impute ----
    
    imputed <- 
      imputed %>% 
      .[, (value.columns) := lapply(X = .SD, FUN = imput.bufferNbs.columns), .SDcols = value.columns]
    
    # 3. return ----
    
    return(imputed)
    
  }

# raster to polygon ----

raster.toPolygon <- 
  function(raster, 
           polygon,
           polygon.cd,
           polygon.nbs){
    
    # 0. function arguments ----
    
    # raster <- prep.dailyNC.era(x = filepaths.nc[[1]])
    # polygon <- poly.lsoa
    # polygon.cd <- 'LSOA21CD'
    # polygon.nbs <- nbs.list.lsoa
    
    # 1. match polygon and raster crs ----
    
    polygon.matchedCRS <- 
      polygon %>% 
      sf::st_transform(x = ., crs = terra::crs(raster)) %>% 
      sf::st_make_valid()
    
    # 2. aggregate raster to polygon ----
    
    poly.agg <- 
      exactextractr::exact_extract(x = raster, 
                                   y = polygon.matchedCRS, 
                                   # weighted mean by area
                                   fun = 'weighted_mean', 
                                   weights = 'area',
                                   # add the columns of poly to new values
                                   append_cols = TRUE,
                                   progress = FALSE) %>% 
      data.table::as.data.table() %>%
      # select columns needed
      .[, c(polygon.cd, grep('^weighted_mean', names(.), value = TRUE)), with = FALSE]
    
    # 3. impute ----
    
    poly.agg.impute <-
      polygon.imputeNeighbours(data = poly.agg,
                               polygon.cd = polygon.cd,
                               polygon.nbs = polygon.nbs)
    
    # 4. clear ----
    
    rm(polygon.matchedCRS,
       poly.agg)
    gc()
    
    # 5. reuturn ----
    
    return(poly.agg.impute)
    
  }


# data processing ----

dataprocess.buildSuicides <- function(
    year.min, year.max, year.intervention,
    filter.intentional = TRUE,
    save.counterfactual = TRUE,
    output.dir,
    output.name,
    label.age = c('15_24', '25_34', '35_44', '45_54', '55_64', '65+'),
    label.sex = c('male', 'female'),
    info.msoa,
    lookup.msoa.rgn,
    lookup.lsoa.msoa,
    dir.data.org,
    dir.data
) {
  
  sprintf('Building dataset: %s | years %s-%s | intervention at %s | filtering intentional sucicides: %s. \n',
          output.name, year.min, year.max, year.intervention, filter.intentional) %>% 
    cat()
  
  # 1. generic spatial ----
  
  generic.spatial <-
    info.msoa %>%
    data.table::as.data.table() %>%
    .[, .(MSOA11CD)] %>%
    merge(
      x = .,
      y = lookup.msoa.rgn[, .(MSOA11CD, RGN11NM, RGN11CD, LAD11CD, LAD11NM)],
      by = 'MSOA11CD'
    ) %>%
    data.table::as.data.table() %>%
    .[, `:=` (
      msoa_id = MSOA11CD %>% as.factor() %>% as.numeric(),
      rgn_id = RGN11CD  %>% as.factor() %>% as.numeric(),
      lad_id = LAD11CD  %>% as.factor() %>% as.numeric()
    )]
  
  # 2. generic temporal ----
  
  generic.temporal <-
    data.table::data.table(year = year.min:year.max) %>%
    .[, `:=` (
      intervention = dplyr::if_else(year < year.intervention, 0, 1) %>%
        factor(., levels = c(0, 1), labels = c('pre', 'post')),
      period = dplyr::if_else(year < year.intervention, NA_integer_, year)
    )] %>%
    .[, `:=` (
      year_id = year %>% as.factor() %>% as.numeric(),
      intervention_id = intervention,
      period_id = dplyr::if_else(is.na(period), 0L, dplyr::dense_rank(period))
    )]
  
  # 3. generic final ----
  
  cols.spatialid <-
    generic.spatial %>%
    colnames() %>%
    grep(pattern = '_id$', x = ., value = TRUE)
  
  generic.final <-
    data.table::CJ(
      MSOA11CD = generic.spatial$MSOA11CD,
      year = year.min:year.max,
      age = label.age,
      sex = label.sex
    ) %>%
    .[, `:=` (
      age = factor(age, levels = label.age, labels = label.age),
      sex = factor(sex, levels = label.sex, labels = label.sex)
    )] %>%
    .[, agesex :=
        interaction(age, sex, sep = '-', lex.order = TRUE) %>%
        relevel(ref = '15_24-female')] %>%
    merge(
      x = ., 
      y = generic.spatial,  
      by = 'MSOA11CD', 
      all.x = TRUE
    ) %>%
    merge(
      x = ., 
      y = generic.temporal, 
      by = 'year',
      all.x = TRUE
    ) %>%
    data.table::as.data.table() %>%
    # spatiotemporal IDs
    .[,
      sprintf('%s_year_id',
              cols.spatialid %>% sub(pattern = '_id$', replacement = '', x = .)) :=
        lapply(cols.spatialid, function(x) {
          (year_id - 1L) * max(get(x)) + get(x)
        })
    ] %>%
    .[, `:=` (
      age_id    = age,
      sex_id    = sex,
      agesex_id = agesex
    )]
  
  # 4. covariates ----
  
  cols.covariates <- c(
    'deprivation', 'ethnicDensity', 'populationDensity', 'lightPollution',
    'railwayDensity', 'roadwayDensity', 'greenspace', 'mentalHealth'
  )
  
  covariates.format <-
    file.path(dir.data.org, 'MSOA11_COVARIATES.csv') %>%
    data.table::fread(file = .) %>%
    .[, setdiff(names(.), c('MSOA11CD', 'year', cols.covariates)) := NULL] %>%
    .[,
      sprintf('%s_id', cols.covariates) :=
        lapply(cols.covariates, function(x) {
          get(x) %>% scale() %>% as.numeric()
        })
    ]
  
  # 5. population ----
  
  population.format <-
    file.path(dir.data.org, 'MSOA11_POPULATIONTOTAL.csv') %>%
    data.table::fread() %>%
    .[, N := population] %>%
    .[, population := NULL] %>%
    .[!(age %in% c('0_4', '5_14'))] %>%
    .[, `:=` (
      age = data.table::fifelse(age %in% c('65_74', '75_84', '85'), '65+', age) %>%
        factor(levels = label.age, labels = label.age),
      sex = factor(sex, levels = label.sex, labels = label.sex)
    )] %>%
    .[, 
      .(N = sum(N)), 
      by = .(MSOA11CD, year, age, sex)
    ]
  
  # 6. suicide ----
  
  suicide.format <-
    file.path(dir.data, 'suicide', 'E20241210_DEATHS_CONNOR.csv') %>%
    data.table::fread() %>%
    .[, `:=` (
      LSOA11CD = LSOA11,
      year = YEAR,
      month = MONTH,
      sex = SEX,
      age = AGE_CLASS,
      intentionalSelfharm = ICD10_X60_X84,
      Y = TOTAL
    )] %>%
    .[, -c('LSOA11', 'YEAR', 'MONTH', 'SEX', 'AGE_CLASS', 'TOTAL', 'ICD10_X60_X84'),
      with = FALSE] %>%
    .[grepl('^E', LSOA11CD)] %>%
    .[!(age %in% c('0_4', '5_14'))] %>%
    # QC copies
    .[, `:=` (age.old = age, sex.old = sex)] %>%
    .[, `:=` (
      age = 
        data.table::fifelse(age %in% c('65_74', '75_84', '85'), '65+', age) %>%
        factor(levels = label.age, labels = label.age),
      sex = 
        data.table::fifelse(sex == 1, 'male', 'female') %>%
        factor(levels = label.sex, labels = label.sex)
    )] %>%
    .[, `:=` (age.old = NULL, sex.old = NULL)] %>%
    merge(
      x = .,
      y = lookup.lsoa.msoa[, .(LSOA11CD, MSOA11CD)] %>% unique(),
      by = 'LSOA11CD',
      all.x = TRUE
    ) %>%
    .[, .(Y = sum(Y)),
      by = .(LSOA11CD, MSOA11CD, year, sex, age, intentionalSelfharm)]
  
  # 7. final dataset ----
  
  # Optionally filter to intentional self-harm only
  suicide.agg <- data.table::copy(suicide.format)
  
  if (filter.intentional) {
    suicide.agg <- suicide.agg[intentionalSelfharm == 1]
  }
  
  suicide.agg <- 
    suicide.agg %>% 
    .[, 
      .(Y = sum(Y)), 
      by = .(MSOA11CD, year, sex, age)
      ]
  
  final <-
    generic.final %>% 
    merge(
      x = ., 
      y = suicide.agg,
      by = c('MSOA11CD', 'year', 'age', 'sex'), 
      all.x = TRUE) %>%
    .[, Y := tidyr::replace_na(Y, replace = 0)] %>%
    merge(
      x = ., 
      y = population.format,  
      by = c('MSOA11CD', 'year', 'age', 'sex')
    ) %>%
    merge(
      x = ., 
      y = covariates.format,  
      by = c('MSOA11CD', 'year'), 
      all.x = TRUE) %>%
    .[, year2_id := year_id] %>%
    .[, (sprintf('lad%s_id', 2:3)) := lad_id] %>%
    # column ordering: geography, time, Y, N first
    data.table::setcolorder(
      neworder = c(
        'MSOA11CD', 'RGN11CD', 'RGN11NM', 'LAD11CD', 'LAD11NM', 'year', 'Y', 'N',
        setdiff(names(.), c('MSOA11CD', 'RGN11CD', 'RGN11NM', 'LAD11CD', 'LAD11NM', 'year', 'Y', 'N'))
      )
    ) %>%
    # all _id columns at the end
    data.table::setcolorder(
      neworder = c(
        setdiff(names(.), grep('_id$', names(.), value = TRUE)),
        grep('_id$', names(.), value = TRUE)
      )
    )
  
  # 8. missing value check ----
  
  n.missing <-
    final[, -c('period', 'period_id', 'intervention', 'intervention_id')] %>%
    is.na() %>%
    sum()
  
  if (n.missing != 0) {
    stop(sprintf(
      'Stop: %s missing values found in dataset "%s".',
      n.missing, output.name
    ))
  }
  
  # 9. save ----
  
  fs::dir_create(output.dir)
  
  saveRDS(final,
          file = file.path(output.dir, sprintf('%s.rds', output.name)))
  
  message(sprintf('  Saved: %s.rds', output.name))
  
  # 10. counterfactual ----
  
  if (save.counterfactual) {
    
    final.cfact <-
      data.table::copy(final) %>%
      .[, `:=` (
        intervention = factor('pre'),
        period = NA_integer_,
        intervention_id = factor('pre'),
        period_id = 0L
      )]
    
    saveRDS(final.cfact,
            file = file.path(output.dir, sprintf('%s_counterfactual.rds', output.name)))
    
    message(sprintf('  Saved: %s_COUNTERFACTUAL.rds', output.name))
  }
  
  # 11. return ----
  
  invisible(final)
}

# model fit ----

model.fitAndSample <- function(
    formula,
    data,
    family = c('poisson', 'zeroinflatedpoisson0', 'zeroinflatedpoisson1')[3],
    filepath.model,
    filepath.samples,
    n.samples = 1000,
    filepath.samples.cheap = NULL,
    n.samples.cheap = NULL,
    ...
) {
  
  # 0. function arguments ----
  
  # formula = formula
  # data = model.data.XY
  # filepath.model = file.path(dir.res.modelfit, 'model_fit.rds')
  # filepath.samples = file.path(dir.res.modelfit, 'model_samples.rds')
  # n.samples = 1000
  # filepath.samples.cheap = file.path(dir.res.modelfit, 'model_samples_5.rds')
  # n.samples.cheap = 5
  
  # 1. model fit ----
  
  model.fit <- 
    helper.loadOrRun(
      filepath = filepath.model,
      FUN = function(...) {
        
        INLA::inla(
          formula = formula,
          data = data,
          family = family,
          control.compute = list(config = TRUE),
          control.predictor = list(link = 1),
          control.inla = list(strategy = 'auto', int.strategy = 'auto'),
          control.mode = list(restart = TRUE),
          verbose = TRUE,
          num.threads = 0.8*future::availableCores(),
          ...)
        
        
      },
      message.run = 'Fitting model...',
      message.load = 'Loading model...'
    )
  
  # 2. full posterior ----
  
  model.samples <- 
    helper.loadOrRun(
      filepath = filepath.samples,
      FUN = function() {
        
        INLA::inla.posterior.sample(
          n = n.samples, 
          result = model.fit
        )
        
      },
      message.run = sprintf('Generating %s posterior samples...', n.samples),
      message.load = sprintf('Loading %s posterior samples...', n.samples)
    )
  
  # 3. cheap posterior (optional) ----
  
  if(!is.null(filepath.samples.cheap) & !is.null(n.samples.cheap)) {
    model.samples.cheap <- 
      helper.loadOrRun(
        filepath = filepath.samples.cheap,
        FUN = function() {
          
          INLA::inla.posterior.sample(
            n = n.samples.cheap, 
            result = model.fit
          )
          
        },
        message.run = sprintf('Generating %s cheap posterior samples...', n.samples.cheap),
        message.load = sprintf('Loading %s cheap posterior samples...', n.samples.cheap)
      )
  } else {
    model.samples.cheap <- NULL
  }
  
  # 4. return ----
  
  return(list(model.fit = model.fit,
              model.samples = model.samples,
              model.samples.cheap = model.samples.cheap))
  
}

# extract sample ----

extract.samples <- function(newdata, fit, return.time = TRUE, return.cheap = FALSE, ...){
  
  time.start <- Sys.time()
  # message(sprintf('sorting...'))
  
  # 0. function arguments ----
  
  # vars.before <- ls()
  # rm(list = setdiff(ls(), vars.before))
  # newdata <- model.data.X
  # fit <- model.fit.X$model.fit
  
  # 1. names ----
  
  nms.fe <-
    fit$summary.fixed %>% 
    rownames()
  nms.re <-
    fit$summary.random %>% 
    names()
  
  # 2. coef ----
  
  coef.fe <- 
    lapply(X = nms.fe %>% seq_along(),
           FUN = function(i) {
             
             # 0. function argument ----
             
             # i <- 1
             
             # 1. parameter ----
             
             nm.fe <- nms.fe[i]
             
             # 2. data ----
             
             # data <-
             #   data.table::data.table(parameter = nm.fe,
             #                          value = fit$summary.fixed %>% as.data.frame() %>% rownames_to_column() %>% filter(rowname == nm.fe) %>% pull(`0.5quant`))
             
             data <-
               data.table::data.table(parameter = nm.fe,
                                      value = get(nm.fe))
             
             # 3. return ----
             
             return(data)
             
           }) %>% 
    rbindlist()
  
  coef.re <- 
    lapply(X = nms.re %>% seq_along(),
           FUN = function(i) {
             
             # 0. function argument ----
             
             # i <- 1
             
             # 1. parameter ----
             
             nm.re <- nms.re[i]
             
             # 2. data ----
             
             # data <-
             #   data.table::data.table(parameter = nm.re,
             #                          value = fit$summary.random[[nm.re]]$`0.5quant`) %>%
             #   .[, index := .I] %>%
             #   .[, .(parameter, index, value)]
             
             data <-
               data.table::data.table(parameter = nm.re,
                                      value = get(nm.re)) %>%
               .[, index := 1:.N] %>%
               .[, .(parameter, index, value)]
             
             # 3. return ----
             
             return(data)
             
           }) %>% 
    stats::setNames(object = .,
                    nm = nms.re)
  
  # 3. linear predictor ----
  
  ## 3.1. model matrix ----
  
  X.fe <-
    nms.fe %>%
    # remove everything after _id
    gsub(pattern = '(_id).*', replacement  = '\\1', x = .) %>% 
    # remove intercept
    .[!grepl(pattern = '(Intercept)', x = .)] %>% 
    # make unique
    unique() %>% 
    # make into formula
    paste(., collapse = ' + ') %>% 
    sprintf('~ 1 + %s', .) %>% 
    as.formula() %>%
    # define model matrix
    Matrix::sparse.model.matrix(., data = newdata)
  
  X.re <-
    newdata %>% 
    .[, nms.re, with = FALSE]
  
  ## 3.2. effect type linear predictors ----
  
  lp.fe <- 
    X.fe[, nms.fe, drop = FALSE] %*% coef.fe[parameter %in% nms.fe, value] %>% 
    as.numeric()
  
  lp.re <-
    lapply(X = nms.re %>% seq_along(),
           FUN = function(i, X.re){
             
             # 0. function arguments ----
             
             # i <- 1
             # X.re <- X.re
             
             # 1. parameters ----
             
             nm.re.sel <- nms.re[i]
             
             # 2. data ----
             
             # data <- fit$summary.random[[nm.re.sel]]$`0.5quant`[X.re[[nm.re.sel]]]
             
             data <- get(nm.re.sel)[X.re[[nm.re.sel]]]
             
             # 3. return ----
             
             return(data)
             
           },
           X.re = X.re) %>% 
    stats::setNames(object = .,
                    nm = nms.re) %>% 
    data.table::as.data.table() %>% 
    rowSums(x = ., na.rm = TRUE) %>% 
    as.numeric()
  
  ## 3.3. full predictor ----
  
  lp <- lp.fe + lp.re + log(newdata$N) # NEED A POPULATION OFFSET!
  
  rm(X.fe,
     X.re,
     lp.fe,
     lp.re)
  gc()
  
  time.end.lp <- Sys.time()
  
  # 4. hyper parameters ----
  
  nms.hypr <- 
    fit$summary.hyperpar %>% 
    rownames()
  
  hyper <- 
    data.table::data.table(parameter = nms.hypr,
                           value = theta)
  
  time.end.hyperpar <- Sys.time()
  
  # 5. observational quantities ----
  
  ## 5.1. parameters ----
  
  n.obs <- length(lp)
  family <- fit$.args$family
  
  ## 5.2. sampling ----
  
  set.seed(1234)
  
  if(family == 'poisson'){
    mu <- exp(lp)
    fitted <- stats::rpois(n = n.obs, lambda = mu)
  }
  
  if(family %in% c('zeroinflatedpoisson0', 'zeroinflatedpoisson1')) {
    
    mu <- exp(lp)
    
    idx.prob <- grep(pattern = 'zero-probability', x = nms.hypr, ignore.case = TRUE)
    prob <- theta[idx.prob]
    # prob <- fit$summary.hyperpar[idx.prob, '0.5quant']
    
    sample.zero <- stats::rbinom(n = n.obs, size = 1, prob = prob)
    sample.nonzero <- stats::rpois(n = n.obs, lambda = mu)
    
    fitted <- dplyr::if_else(sample.zero == 1, 0, sample.nonzero)
  }
  
  ## 5.3. formatting ----
  
  pred <-
    data.table::data.table(linearpredictor = lp,
                           fitted = fitted)
  
  rm(n.obs,
     family,
     mu, 
     fitted)
  gc()
  
  time.end.pred <- Sys.time()
  
  # 6. return ----
  
  ## 6.1. cheap return ----
  
  if(return.cheap){
    
    return(list(linear.predictor = lp,
                prediction = pred))
    
  }
  
  ## 6.2. full return ----
  
  time.elapsed <-
    data.table::data.table(phase = c('linear predcitor', 'hyperparameters', 'predictions', 'total'),
                           time = c(difftime(time1 = time.end.lp, time2 = time.start, units = 'secs'),
                                    difftime(time1 = time.end.hyperpar, time2 = time.end.lp, units = 'secs'),
                                    difftime(time1 = time.end.pred, time2 = time.end.hyperpar, units = 'secs'),
                                    difftime(time1 = time.end.pred, time2 = time.start, units = 'secs'))) %>% 
    .[, message := data.table::fifelse(test = phase != 'total',
                                       yes = sprintf('  time for %s: %.2f seconds.\n', phase, time),
                                       no = sprintf(' total time: %.2f seconds.', time))]
  
  if(return.time) { 
    message(time.elapsed$message) 
  }
  
  return(list(fixed.effects = coef.fe,
              random.effects = coef.re,
              hyperparameters = hyper,
              linear.predictor = lp,
              prediction = pred))
  
}

# compute posterior ----

model.computePosterior <- function(
    model.fit,
    model.samples,
    data,
    filepath.posterior = NULL,
    FUN.EVAL = extract.samples,
    return.cheap = FALSE
) {
  
  # 0. function arguments ----
  
  # model.fit
  # model.samples
  # data
  # filepath.posterior
  # n.subset = 50
  # FUN.EVAL = extract.samples
  # return.cheap = FALSE
  
  # 1. compute posterior ----
  
  posterior <- 
    helper.loadOrRun(
      filepath = filepath.posterior,
      FUN = function() {
        INLA::inla.posterior.sample.eval(
          fun = FUN.EVAL,
          samples = model.samples,
          fit = model.fit,
          newdata = data,
          return.time = FALSE,
          return.cheap = return.cheap,
          return.matrix = FALSE
        ) %>% stats::setNames(sprintf('theta:%s', seq_along(model.samples)))
      },
      message.run = 'Defining posterior...',
      message.load = 'Loading posterior...'
    )
  
  # 2. return ----
  
  return(posterior)
}

# format predictions ----

format.prediction <-
  function(posterior) {
    
    # 0. function arguments ----
    
    # posterior <- posterior.fact
    
    # 1. extract results ----
    
    res <-
      lapply(X = posterior %>% seq_along(),
             FUN = function(i) {
               
               # 0. function arguments ----
               
               # i <- 1
               
               # 1. parameters ----
               
               name.theta <- names(posterior)[i]
               sample <- posterior[[i]]
               import.prediction <- sample$prediction
               
               # 2. results ----
               
               res <- 
                 data.table::copy(import.prediction) %>% 
                 .[, id := .I] %>%
                 data.table::setnames(x = .,
                                      old = 'fitted', 
                                      new = name.theta) %>% 
                 .[, c('id', name.theta), with = FALSE]
               
               # 3. return ----
               
               return(res)
               
             }) %>% 
      Reduce(f = function(x, y) merge(x, y, by = 'id', all.x = TRUE, sort = FALSE),
             x = .) %>% 
      data.table::as.data.table()
    
    # 2. return ----
    
    return(res)
    
  }

# format fixed effects ----

format.fixedeffects <-
  function(posterior) {
    
    # 0. function arguments ----
    
    # posterior <- posterior.fact
    
    # 1. extract results ----
    
    res <-
      lapply(X = posterior %>% seq_along(),
             FUN = function(i){
               
               # 0. function arguments ----
               
               # i <- 1
               
               # 1. parameters ----
               
               name.theta <- names(posterior)[i]
               sample <- posterior[[i]]
               import.fixed <- sample$fixed.effects
               
               # 2. results ----
               
               res <- 
                 data.table::copy(import.fixed) %>% 
                 data.table::setnames(x = .,
                                      old = 'value', 
                                      new = name.theta)
               
               # 3. return ----
               
               return(res)
               
             }) %>% 
      Reduce(f = function(x, y) merge(x, y, by = 'parameter', all.x = TRUE, sort = FALSE),
             x = .) %>% 
      data.table::as.data.table()
    
  }

# format random effects ----

format.randomeffects <-
  function(posterior) {
    
    # 0. function arguments ----
    
    # posterior <- posterior.fact
    
    # 1. extract results ----
    
    res <-
      lapply(X = posterior %>% seq_along(),
             FUN = function(i){
               
               # 0. function arguments ----
               
               # i <- 1
               
               # 1. parameters ----
               
               name.theta <- names(posterior)[i]
               sample <- posterior[[i]]
               import.random <- sample$random.effects
               
               # 2. results ----
               
               res <- 
                 import.random %>% 
                 data.table::rbindlist() %>%
                 data.table::copy() %>% 
                 data.table::setnames(x = .,
                                      old = 'value',
                                      new = name.theta)
               
               # 3. return ----
               
               return(res)
               
             }) %>% 
      Reduce(f = function(x, y) merge(x, y, by = c('parameter', 'index'), all.x = TRUE, sort = FALSE),
             x = .) %>% 
      data.table::as.data.table()
    
  }

# format spatially varying effects ----

format.svceffects <-
  function(posterior) {
    
    # 0. function arguments ----
    
    # posterior <- model.posterior
    
    # 1. extract results ----
    
    res <-
      lapply(X = posterior %>% seq_along(),
             FUN = function(i){
               
               # 0. function argumnents ----
               
               # i <- 1
               
               # 1. parameters ----
               
               name.theta <- names(posterior)[i]
               sample <- posterior[[i]]
               import.fixed <- sample$fixed.effects
               import.random <- sample$random.effects
               n.lad.all <- import.random$lad_id$index %>% length()
               n.lad.needed <- n.lad.all/2 # !! NEEDED FOR USING BYM2
               
               # 2. fixed effects ----
               
               fixed.year <-
                 import.fixed %>%
                 # data.table::as.data.table() %>% 
                 .[parameter == 'year_id', value] %>% 
                 as.numeric()
               
               fixed.intervention <-
                 import.fixed %>% 
                 # data.table::as.data.table() %>% 
                 .[parameter == 'intervention_idpost', value] %>% 
                 as.numeric()
               
               fixed.period <- 
                 import.fixed %>% 
                 # data.table::as.data.table() %>% 
                 .[parameter == 'period_id', value] %>% 
                 as.numeric()
               
               # 2. random effects ----
               
               random.year <- 
                 import.random$lad_id %>% 
                 # data.table::as.data.table() %>%
                 .[index %in% 1:(n.lad.needed)]
               
               random.intervention <- 
                 import.random$lad2_id %>% 
                 # data.table::as.data.table() %>%
                 .[index %in% 1:(n.lad.needed)]
               
               random.period <- 
                 import.random$lad3_id %>% 
                 # data.table::as.data.table() %>%
                 .[index %in% 1:(n.lad.needed)]
               
               # 3. spatially varying coefficients ----
               
               svc.year <-
                 random.year %>% 
                 .[, (name.theta) := value + fixed.year] %>% 
                 .[, `:=` (
                   lad_id = index,
                   parameter = 'year_id'
                 )] %>% 
                 .[, c('parameter', 'lad_id', name.theta), with = FALSE]
               
               svc.intervention <-
                 random.intervention %>% 
                 .[, (name.theta) := value + fixed.intervention] %>% 
                 .[, `:=` (
                   lad_id = index,
                   parameter = 'intervention_idpost'
                 )] %>% 
                 .[, c('parameter', 'lad_id', name.theta), with = FALSE]
               
               svc.period <-
                 random.period %>% 
                 .[, (name.theta) := value + fixed.period] %>% 
                 .[, `:=` (
                   lad_id = index,
                   parameter = 'period_id'
                 )] %>% 
                 .[, c('parameter', 'lad_id', name.theta), with = FALSE]
               
               # 4. final results ----
               
               res <-
                 list(svc.year,
                      svc.intervention,
                      svc.period) %>% 
                 data.table::rbindlist()
               
               # 5. return ----
               
               return(res)
               
             }) %>% 
      Reduce(f = function(x, y) merge(x, y, by = c('parameter', 'lad_id'), all.x = TRUE, sort = FALSE),
             x = .) %>% 
      data.table::as.data.table()
    
  }

# format hyper parameters ----

format.hyperparameter <-
  function(posterior) {
    
    # 0. function arguments ----
    
    # posterior <- posterior.fact
    
    # 1. extract results ----
    
    res <-
      lapply(X = posterior %>% seq_along(),
             FUN = function(i){
               
               # 0. function argumnents ----
               
               # i <- 1
               
               # 1. parameters ----
               
               name.theta <- names(posterior)[i]
               sample <- posterior[[i]]
               import.hyper <- sample$hyperparameters
               
               # 2. results ----
               
               res <- 
                 data.table::copy(import.hyper) %>%
                 data.table::setnames(x = .,
                                      old = 'value', 
                                      new = name.theta)
               
               # 5. return ----
               
               return(res)
               
             }) %>% 
      Reduce(f = function(x, y) merge(x, y, by = 'parameter', all.x = TRUE, sort = FALSE),
             x = .) %>% 
      data.table::as.data.table()
    
  }

# format all results ----

format.allresults <- 
  function(
    data,
    posterior,
    scale.observation = 100000
  ) {
    
    # 0. function arguments ----
    
    # data <- XXX
    # posterior <- XXX
    # scale.observation <- 100000
    
    # 1. format data ----
    
    colnames.theta <- names(posterior)
    
    # 2. format posterior ----
    
    t0 <- Sys.time()
    sprintf('Formatting linear predictor posterior:') %>% message()
    theta.predicted <-
      posterior %>% 
      format.prediction(posterior = .) %>% 
      # join to data
      merge(x = data %>% .[, !grep(pattern = '_id$', x = names(.), value = TRUE), with = FALSE] %>% .[, id := .I],
            y = ., 
            by = 'id', 
            all.x = TRUE, 
            sort = FALSE) %>%
      data.table::as.data.table() %>% 
      .[, id := NULL]
    t1 <- Sys.time()
    sprintf(' time: %s.', helper.formatTime(t1, t0)) %>% message()
    
    t0 <- Sys.time()
    sprintf('Formatting fixed effect posterior:') %>% message()
    theta.fixed <-
      posterior %>% 
      format.fixedeffects(posterior = .)
    t1 <- Sys.time()
    sprintf(' time: %s.', helper.formatTime(t1, t0)) %>% message()
    
    t0 <- Sys.time()
    sprintf('Formatting random effects posterior:') %>% message()
    theta.random <-
      posterior %>% 
      format.randomeffects(posterior = .)
    t1 <- Sys.time()
    sprintf(' time: %s.', helper.formatTime(t1, t0)) %>% message()
    
    t0 <- Sys.time()
    sprintf('Formatting SVC effects posterior:') %>% message()
    theta.svc <-
      posterior %>% 
      format.svceffects(posterior = .)
    t1 <- Sys.time()
    sprintf(' time: %s.', helper.formatTime(t1, t0)) %>% message()
    
    t0 <- Sys.time()
    sprintf('Formatting hyperparameter posterior:') %>% message()
    theta.hyper <-
      posterior %>% 
      format.hyperparameter(posterior = .)
    t1 <- Sys.time()
    sprintf(' time: %s.', helper.formatTime(t1, t0)) %>% message()
    
    # 3. summary ----
    
    ## 3.1. year-country ----
    
    t0 <- Sys.time()
    sprintf('Summarising year-CTRY posterior:') %>% message()
    smry.yrCTRY <-
      theta.predicted %>% 
      # summarise
      .[, 
        c(.(Y = sum(Y),
            N = sum(N),
            R = sum(Y) / sum(N) * scale.observation),
          lapply(X = .SD, FUN = sum)),
        by = c('year'),
        .SDcols = colnames.theta] %>% 
      # summarise and scale
      {
        dt <- .
        m <- as.matrix(dt[, ..colnames.theta]) / dt$N * scale.observation
        dt[, `:=` (
          mean = Matrix::rowMeans(x = m),
          var = matrixStats::rowVars(x = m),
          median = matrixStats::rowMedians(x = m),
          lower = matrixStats::rowQuantiles(x = m, probs = 0.025),
          upper = matrixStats::rowQuantiles(x = m, probs = 0.975)
        )]
        dt
      } %>% 
      .[, -colnames.theta, with = FALSE]
    t1 <- Sys.time()
    sprintf(' time: %s.', helper.formatTime(t1, t0)) %>% message()
    
    ## 3.2. year-region ----
    
    t0 <- Sys.time()
    sprintf('Summarising year-RGN posterior:') %>% message()
    smry.yrRGN <-
      theta.predicted %>% 
      # summarise
      .[, 
        c(.(Y = sum(Y),
            N = sum(N),
            R = sum(Y) / sum(N) * scale.observation),
          lapply(X = .SD, FUN = sum)),
        by = c('RGN11NM', 'year'),
        .SDcols = colnames.theta] %>% 
      # summarise and scale
      {
        dt <- .
        m <- as.matrix(dt[, ..colnames.theta]) / dt$N * scale.observation
        dt[, `:=` (
          mean = Matrix::rowMeans(x = m),
          var = matrixStats::rowVars(x = m),
          median = matrixStats::rowMedians(x = m),
          lower = matrixStats::rowQuantiles(x = m, probs = 0.025),
          upper = matrixStats::rowQuantiles(x = m, probs = 0.975)
        )]
        dt
      } %>% 
      .[, -colnames.theta, with = FALSE]
    t1 <- Sys.time()
    sprintf(' time: %s.', helper.formatTime(t1, t0)) %>% message()
    
    ## 3.3. year-LAD ----
    
    t0 <- Sys.time()
    sprintf('Summarising year-LTLA posterior:') %>% message()
    smry.yrLAD <-
      theta.predicted %>% 
      # summarise
      .[, 
        c(.(Y = sum(Y),
            N = sum(N),
            R = sum(Y) / sum(N) * scale.observation),
          lapply(X = .SD, FUN = sum)),
        by = c('LAD11CD', 'LAD11NM', 'RGN11NM', 'year'),
        .SDcols = colnames.theta] %>% 
      # summarise and scale
      {
        dt <- .
        m <- as.matrix(dt[, ..colnames.theta]) / dt$N * scale.observation
        dt[, `:=` (
          mean = Matrix::rowMeans(x = m),
          var = matrixStats::rowVars(x = m),
          median = matrixStats::rowMedians(x = m),
          lower = matrixStats::rowQuantiles(x = m, probs = 0.025),
          upper = matrixStats::rowQuantiles(x = m, probs = 0.975)
        )]
        dt
      } %>% 
      .[, -colnames.theta, with = FALSE]
    t1 <- Sys.time()
    sprintf(' time: %s.', helper.formatTime(t1, t0)) %>% message()
    
    ## 3.4. year-msoa ----
    
    t0 <- Sys.time()
    sprintf('Summarising year-MSOA posterior:') %>% message()
    smry.yrMSOA <-
      theta.predicted %>% 
      # summarise
      .[, 
        c(.(Y = sum(Y),
            N = sum(N),
            R = sum(Y) / sum(N) * scale.observation),
          lapply(X = .SD, FUN = sum)),
        by = c('MSOA11CD', 'LAD11CD', 'LAD11NM', 'RGN11NM', 'year'),
        .SDcols = colnames.theta] %>% 
      # summarise and scale
      {
        dt <- .
        m <- as.matrix(dt[, ..colnames.theta]) / dt$N * scale.observation
        dt[, `:=` (
          mean = Matrix::rowMeans(x = m),
          var = matrixStats::rowVars(x = m),
          median = matrixStats::rowMedians(x = m),
          lower = matrixStats::rowQuantiles(x = m, probs = 0.025),
          upper = matrixStats::rowQuantiles(x = m, probs = 0.975)
        )]
        dt
      } %>% 
      .[, -colnames.theta, with = FALSE]
    t1 <- Sys.time()
    sprintf(' time: %s.', helper.formatTime(t1, t0)) %>% message()
    
    ## 3.5. its-country ----
    
    t0 <- Sys.time()
    sprintf('Summarising ITS-country posterior:') %>% message()
    smry.itsCtry <-
      theta.fixed %>% 
      # filter its terms
      .[parameter %in% c('year_id', 'intervention_idpost', 'period_id'), ] %>% 
      # summarise and rescale
      .[, (colnames.theta) := lapply(.SD, exp), .SDcols = colnames.theta] %>% 
      {
        dt <- .
        m <- as.matrix(dt[, ..colnames.theta])
        dt[, `:=` (
          mean = Matrix::rowMeans(x = m),
          var = matrixStats::rowVars(x = m),
          median = matrixStats::rowMedians(x = m),
          lower = matrixStats::rowQuantiles(x = m, probs = 0.025),
          upper = matrixStats::rowQuantiles(x = m, probs = 0.975)
        )]
        dt
      } %>% 
      .[, -colnames.theta, with = FALSE]
    t1 <- Sys.time()
    sprintf(' time: %s.', helper.formatTime(t1, t0)) %>% message()
    
    ## 3.6. its-lad ----
    
    t0 <- Sys.time()
    sprintf('Summarising ITS-LTLA posterior:') %>% message()
    smry.itsLAD <-
      theta.svc %>% 
      # add region
      merge(x = .,
            y = data %>% .[, .(LAD11CD, LAD11NM, lad_id, RGN11NM)] %>% unique(),
            by = 'lad_id',
            all.x = TRUE) %>%  
      # add national median
      merge(
        x = .,
        y = smry.itsCtry %>% .[, medianCTRY := median] %>% .[, .(parameter, medianCTRY)],
        by = 'parameter',
        all.x = TRUE
      ) %>% 
      # summarise and scale
      .[, (colnames.theta) := lapply(.SD, exp), .SDcols = colnames.theta] %>% 
      {
        dt <- .
        m <- as.matrix(dt[, ..colnames.theta])
        dt[, `:=` (
          mean = Matrix::rowMeans(x = m),
          var = matrixStats::rowVars(x = m),
          median = matrixStats::rowMedians(x = m),
          lower = matrixStats::rowQuantiles(x = m, probs = 0.025),
          upper = matrixStats::rowQuantiles(x = m, probs = 0.975)
        )]
        dt
      } %>% 
      # exceedance probabilties
      ## of one
      .[, exceedanceOne := 100 * Matrix::rowMeans(x = as.matrix(.SD) > 1), .SDcols = colnames.theta] %>% 
      .[, exceedanceOne_prob := cut(x = exceedanceOne, breaks = c(-Inf, 20, 80, Inf), labels = c('[0%, 20%]', '(20%, 80%]', '(80%, 100%]'))] %>%
      ## of natioanl
      .[, exceedanceCTRY := 100 * Matrix::rowMeans(x = as.matrix(.SD) > medianCTRY), .SDcols = colnames.theta] %>% 
      .[, exceedanceCTRY_prob := cut(x = exceedanceCTRY, breaks = c(-Inf, 20, 80, Inf), labels = c('[0%, 20%]', '(20%, 80%]', '(80%, 100%]'))] %>% 
      # remove uneccesary columns
      .[, -colnames.theta, with = FALSE] %>% 
      .[, -'medianCTRY', with = FALSE]
    t1 <- Sys.time()
    sprintf(' time: %s.', helper.formatTime(t1, t0)) %>% message()
    
    ## 3.7. fixed effects ----
    
    t0 <- Sys.time()
    sprintf('Summarising fixed effects posterior:') %>% message()
    smry.fxd <-
      theta.fixed %>% 
      # summarise and scale
      .[, (colnames.theta) := lapply(.SD, exp), .SDcols = colnames.theta] %>% 
      {
        dt <- .
        m <- as.matrix(dt[, ..colnames.theta])
        dt[, `:=` (
          mean = Matrix::rowMeans(x = m),
          var = matrixStats::rowVars(x = m),
          median = matrixStats::rowMedians(x = m),
          lower = matrixStats::rowQuantiles(x = m, probs = 0.025),
          upper = matrixStats::rowQuantiles(x = m, probs = 0.975)
        )]
        dt
      } %>% 
      .[, -colnames.theta, with = FALSE]
    t1 <- Sys.time()
    sprintf(' time: %s.', helper.formatTime(t1, t0)) %>% message()
    
    ## 3.8. random effects ----
    
    t0 <- Sys.time()
    sprintf('Summarising random effects posterior:') %>% message()
    smry.rdm.tmp <-
      theta.random %>% 
      # remove svc
      .[!startsWith(parameter, 'lad')] %>% 
      # summarise and scale
      .[, (colnames.theta) := lapply(X = .SD, FUN = exp), .SDcols = colnames.theta] %>% 
      {
        dt <- .
        m <- as.matrix(dt[, ..colnames.theta])
        dt[, `:=` (
          mean = Matrix::rowMeans(x = m),
          var = matrixStats::rowVars(x = m),
          median = matrixStats::rowMedians(x = m),
          lower = matrixStats::rowQuantiles(x = m, probs = 0.025),
          upper = matrixStats::rowQuantiles(x = m, probs = 0.975)
        )]
        dt
      } %>% 
      # exceedance probabilties (for spatial)
      .[, exceedance := 100 * Matrix::rowMeans(x = as.matrix(.SD) > 1), .SDcols = colnames.theta] %>% 
      .[, exceedanceProb := cut(x = exceedance, breaks = c(-Inf, 20, 80, Inf), labels = c('[0%, 20%]', '(20%, 80%]', '(80%, 100%]'))] %>% 
      .[, -colnames.theta, with = FALSE] %>% 
      split(x = .,
            by = 'parameter')
    
    smry.rdm.temporal <- 
      smry.rdm.tmp[['year2_id']] %>% 
      merge(x = ., 
            y = data %>% .[, index := year2_id] %>% .[, .(year, index)] %>% unique(),
            by = 'index',
            all.x = TRUE)
    
    smry.rdm.spatial <- 
      smry.rdm.tmp[['msoa_id']] %>% 
      .[index %in% 1:(max(index)/2)] %>% 
      merge(x = ., 
            y = data %>% .[, index := msoa_id] %>% .[, .(MSOA11CD, index)] %>% unique(),
            by = 'index',
            all.x = TRUE)
    
    smry.rdm.spatiotemporal <-
      smry.rdm.tmp[['msoa_year_id']] %>% 
      merge(x = ., 
            y = data %>% .[, index := msoa_year_id] %>% .[, .(MSOA11CD, LAD11NM, LAD11CD, RGN11NM, year, index)] %>% unique(),
            by = 'index',
            all.x = TRUE)
    
    smry.rdm <-
      list(
        temporal = smry.rdm.temporal,
        spatial = smry.rdm.spatial,
        spatiotemporal = smry.rdm.spatiotemporal
      )
    t1 <- Sys.time()
    sprintf(' time: %s.', helper.formatTime(t1, t0)) %>% message()
    
    ## 3.9. hyper priors ----
    
    t0 <- Sys.time()
    sprintf('Summarising hyperparameter posterior:') %>% message()
    smry.hyper <- 
      theta.hyper %>% 
      # summarise and scale
      {
        dt <- .
        m <- as.matrix(dt[, ..colnames.theta])
        dt[, `:=` (
          mean = Matrix::rowMeans(x = m),
          var = matrixStats::rowVars(x = m),
          median = matrixStats::rowMedians(x = m),
          lower = matrixStats::rowQuantiles(x = m, probs = 0.025),
          upper = matrixStats::rowQuantiles(x = m, probs = 0.975)
        )]
        dt
      } %>% 
      .[, -colnames.theta, with = FALSE]
    t1 <- Sys.time()
    sprintf(' time: %s.', helper.formatTime(t1, t0)) %>% message()
    
    # 4. return ----
    
    out <-
      list(
        summary.yearCTRY = smry.yrCTRY,
        summary.yearRGN = smry.yrRGN,
        summary.yearLAD = smry.yrLAD,
        summary.yearMSOA = smry.yrMSOA,
        summary.itsCTRY = smry.itsCtry,
        summary.itsLAD = smry.itsLAD,
        summary.fixedEffects = smry.fxd,
        summary.randomEffects = smry.rdm,
        summary.hyperpriors = smry.hyper
      )
    
    return(out)
    
  }

# year-country plot ----

makePlot.yrCTRY <- 
  function(data, 
           dir.target,
           filename, 
           plot.colour, 
           plot.fill,
           plot.xbreaks = seq(from = 2002, to = 2022, by = 2),
           plot.xlimits = c(2002, 2022),
           plot.scaleLabel,
           plot.text = 20,
           plot.with = 10,
           plot.height = 10) {
    
    # 0. function arguments ----
    
    # data <- data.yrCTRY
    # filename <- 'year_ctry'
    # dir.target <- dir.res.sens.modres
    # plot.colour <- colours.scenario
    # plot.fill <- fill.scenario
    # plot.scaleLabel <- scale.label
    
    # 1. parameters ----
    
    plot.range <- c(min(data$lower), max(data$upper))
    plot.limit <- c(floor(plot.range[1]/10)*10, ceiling(plot.range[2]/10)*10)
    plot.breaks <- seq(from = plot.limit[1], to = plot.limit[2], length.out = 11)
    
    # 2. plot ----
    
    plot <- 
      ggplot2::ggplot(data = data, 
                      mapping = aes(x = year, y = median, 
                                    colour = scenario, fill = scenario, group = scenario)
      ) +
      ggplot2::geom_rect(xmin = 2012, xmax = Inf,
                         ymin = -Inf, ymax = Inf,
                         fill = 'grey90', alpha = 0.1, 
                         inherit.aes = FALSE) +
      ggplot2::geom_ribbon(mapping = aes(ymin = lower, ymax = upper),
                           colour = NA, alpha = 0.5) +
      ggplot2::geom_line() +
      ggplot2::scale_colour_manual(name = '',
                                   values = plot.colour) +
      ggplot2::scale_fill_manual(name ='',
                                 values = plot.fill) +
      ggplot2::scale_x_continuous(name = '', breaks = plot.xbreaks, limits = plot.xlimits) +
      ggplot2::scale_y_continuous(name = sprintf('Suicide rate per %s people', plot.scaleLabel), 
                                  breaks = plot.breaks, 
                                  limits = plot.limit) +
      # theme
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1, 
                                                   label.position = 'left',
                                                   label.hjust = 1),
                      colour = ggplot2::guide_legend(ncol = 1, 
                                                     label.position = 'left',
                                                     label.hjust = 1)) +
      helper.plotTheme(text = ggplot2::element_text(size = plot.text),
               legend.title = ggplot2::element_blank(),
               legend.position = 'inside',
               legend.position.inside = c(0.98, 0.02),
               legend.justification = c(1, 0),
               legend.text.align = 1,
               legend.box.just = 'right',
               legend.background = ggplot2::element_blank())
    
    # 3. save ----
    
    ggplot2::ggsave(filename = file.path(dir.target, sprintf('%s.png', filename)),
                    plot = plot,
                    width = plot.width,
                    height = plot.height)
    
    data.table::fwrite(x = data,
                       file = file.path(dir.target, sprintf('%s.csv', filename)))
    
    # 4. return ----
    
    return(plot)
    
  }

# year-region plot ----

makePlot.yrRGN <-
  function(data, 
           dir.target,
           filename, 
           plot.colour, 
           plot.fill,
           plot.xbreaks = seq(from = 2002, to = 2022, by = 10),
           plot.xlimits = c(2002, 2022),
           plot.scaleLabel,
           plot.text = 20,
           plot.with = 10,
           plot.height = 10) {
    
    # 0. function arguments ----
    
    # res <- results.2012
    # plot.colour <- colours.scenario
    # plot.fill <- fill.scenario
    # plot.scaleLabel <- scale.label
    
    # 1. parameters ----
    
    plot.range <- c(min(data$lower), max(data$upper))
    plot.limit <- c(floor(plot.range[1]/10)*10, ceiling(plot.range[2]/10)*10)
    plot.breaks <- seq(from = plot.limit[1], to = plot.limit[2], length.out = 11)
    
    # 2. plot ----
    
    plot <- 
      ggplot2::ggplot(data = data, 
                      mapping = aes(x = year, y = median)) +
      # grey box
      ggplot2::geom_rect(xmin = 2012, xmax = Inf,
                         ymin = -Inf, ymax = Inf,
                         fill = 'grey90', alpha = 0.1, 
                         inherit.aes = FALSE) +
      # region
      ## fill
      ggplot2::geom_ribbon(mapping = aes(ymin = lower, ymax = upper,
                                         group = interaction(RGN11NM, scenario),
                                         fill = scenario),
                           colour = NA, alpha = 0.5) +
      ## line
      ggplot2::geom_line(mapping = aes(group = interaction(RGN11NM, scenario),
                                       colour = scenario)
      ) +
      # colour
      ## fill
      ggplot2::scale_fill_manual(name ='',
                                 values = plot.fill) +
      ## line
      ggplot2::scale_colour_manual(name = '',
                                   values = plot.colour) +
      # axis
      ggplot2::scale_x_continuous(name = '', breaks = plot.xbreaks, limits = plot.xlimits) +
      ggplot2::scale_y_continuous(name = sprintf('Suicide rate per %s people', plot.scaleLabel), 
                                  breaks = plot.breaks, 
                                  limits = plot.limit) +
      # facet
      ggplot2::facet_wrap(~ RGN11NM, ncol = 3) +
      # theme
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1, 
                                                   label.position = 'left',
                                                   label.hjust = 1),
                      colour = ggplot2::guide_legend(ncol = 1, 
                                                     label.position = 'left',
                                                     label.hjust = 1)) +
      helper.plotTheme(text = ggplot2::element_text(size = 3*plot.text),
               axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
               legend.title = ggplot2::element_blank(),
               legend.position = 'inside',
               legend.position.inside = c(0.98, 0.02),
               legend.justification = c(1, 0),
               legend.text.align = 1,
               legend.box.just = 'right',
               legend.background = ggplot2::element_blank())
    
    # 3. save ----
    
    ggplot2::ggsave(filename = file.path(dir.target, sprintf('%s.png', filename)),
                    plot = plot,
                    width = 3*plot.width,
                    height = 3*plot.height)
    
    data.table::fwrite(x = data,
                       file = file.path(dir.target, sprintf('%s.csv', filename)))
    
    # 4. return ----
    
    return(plot)
    
  }

# year-LAD plot ----

makePlot.yrLAD <-
  function(data, 
           dir.target,
           filename, 
           plot.xbreaks = seq(from = 2002, to = 2022, by = 10),
           plot.xlimits = c(2002, 2017),
           plot.scaleLabel,
           plot.text = 20,
           plot.with = 10,
           plot.height = 10) {
    
    # 0. function arguments ----
    
    # data <- data.yrLAD
    # plot.scaleLabel <- scale.label
    
    # 1. parameters ----
    
    plot.range <- c(min(data$lower), max(data$upper))
    plot.limit <- c(floor(plot.range[1]/10)*10, ceiling(plot.range[2]/10)*10)
    plot.breaks <- seq(from = plot.limit[1], to = plot.limit[2], length.out = 11)
    
    # 2. plot ----
    
    plot <- 
      ggplot2::ggplot(data = data, 
                      mapping = aes(x = year, y = median, group = LAD11NM)) +
      ggplot2::geom_rect(xmin = 2012, xmax = Inf,
                         ymin = -Inf, ymax = Inf,
                         fill = 'grey90', alpha = 0.1, 
                         inherit.aes = FALSE) +
      ggplot2::geom_line(alpha = 0.5, linewidth = 0.5, colour = 'lightskyblue2') +
      ggplot2::scale_x_continuous(name = '', breaks = plot.xbreaks, limits = plot.xlimits) +
      ggplot2::scale_y_continuous(name = sprintf('Suicide rate per %s people', plot.scaleLabel), 
                                  breaks = plot.breaks, 
                                  limits = plot.limit) +
      ggplot2::facet_wrap(~ RGN11NM, ncol = 3) +
      helper.plotTheme(text = ggplot2::element_text(size = 3*plot.text),
               legend.position = 'none',
               legend.background = ggplot2::element_blank(),
               legend.box.background = ggplot2::element_blank(),
               axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
    
    # 3. save ----
    
    ggplot2::ggsave(filename = file.path(dir.target, sprintf('%s.png', filename)),
                    plot = plot,
                    width = 3*plot.width,
                    height = 3*plot.height)
    
    # 4. return ----
    
    return(plot)
    
  }

# year-MSOA plot ----

makePlot.yrMSOA <-
  function(data, 
           dir.target,
           filename, 
           plot.xbreaks = seq(from = 2002, to = 2022, by = 10),
           plot.xlimits = c(2002, 2022),
           plot.scaleLabel,
           plot.text = 20,
           plot.with = 10,
           plot.height = 10) {
    
    # 0. function arguments ----
    
    # data <- data.yrMSOA
    # plot.scaleLabel <- scale.label
    
    # 1. parameters ----
    
    plot.range <- c(min(data$lower), max(data$upper))
    plot.limit <- c(floor(plot.range[1]/10)*10, ceiling(plot.range[2]/10)*10)
    plot.breaks <- seq(from = plot.limit[1], to = plot.limit[2], length.out = 11)
    
    # 2. plot ----
    
    plot <- 
      ggplot2::ggplot(data = data, 
                      mapping = aes(x = year, y = median, group = MSOA11CD)) +
      ggplot2::geom_rect(xmin = 2012, xmax = Inf,
                         ymin = -Inf, ymax = Inf,
                         fill = 'grey90', alpha = 0.1, 
                         inherit.aes = FALSE) +
      ggplot2::geom_line(alpha = 0.5, linewidth = 0.5, colour = 'lightskyblue2') +
      ggplot2::scale_x_continuous(name = '', breaks = plot.xbreaks, limits = plot.xlimits) +
      ggplot2::scale_y_continuous(name = sprintf('Suicide rate per %s people', plot.scaleLabel), 
                                  breaks = plot.breaks, 
                                  limits = plot.limit) +
      ggplot2::facet_wrap(~ RGN11NM, ncol = 3) +
      helper.plotTheme(text = ggplot2::element_text(size = 3*plot.text),
               legend.position = 'none',
               legend.background = ggplot2::element_blank(),
               legend.box.background = ggplot2::element_blank(),
               axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
    
    # 3. save ----
    
    ggplot2::ggsave(filename = file.path(dir.target, sprintf('%s.png', filename)),
                    plot = plot,
                    width = 3*plot.width,
                    height = 3*plot.height)
    
    # 4. return ----
    
    return(plot)
    
  }

# its-country plot ----

makePlot.itsCTRY <- 
  function(
    data, 
    plot.colour = NULL,
    dir.target,
    filename,
    plot.text = 20,
    plot.with = 10,
    plot.height = 10
  ){
    
    # 0. function parmeters ----
    
    # data <- data.itsCTRY
    # plot.colour <- colours.scenario
    
    # 1. parameters ----
    
    plot.range <- c(min(data$lower), max(data$upper))
    plot.limit <- c(floor(10*plot.range[1])/10, ceiling(10*plot.range[2])/10)
    plot.breaks <- seq(from = plot.limit[1], to = plot.limit[2], length.out = 5)
    
    # 2. plot ----
    
    if(length(unique(data$scenario)) != 1) {
      
      plot.jitter <- ggplot2::position_dodge(width = 0.5)
      
      plot <-
        ggplot2::ggplot(data = data, 
                        mapping = ggplot2::aes(x = parameter, y = median, group = scenario, colour = scenario)) +
        ggplot2::geom_hline(mapping = aes(yintercept = 1), 
                            colour = 'black', 
                            linetype = 'dashed') +
        ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = lower, ymax = upper),
                               width = 0,
                               position = plot.jitter) +
        ggplot2::geom_point(position = plot.jitter) +
        ggplot2::scale_colour_manual(name = '',
                                     values = plot.colour) +
        ggplot2::scale_x_discrete(name = '',
                                  limits = levels(data$parameter) %>% rev()) +
        ggplot2::scale_y_continuous(name = 'Relative risk', 
                                    limits = plot.limit,
                                    breaks = plot.breaks) +
        ggplot2::coord_flip() +
        # theme
        ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1, 
                                                       label.position = 'left',
                                                       label.hjust = 1)) +
        helper.plotTheme(text = ggplot2::element_text(size = plot.text),
                 legend.title = ggplot2::element_blank(),
                 legend.position = 'inside',
                 legend.position.inside = c(0.98, 0.02),
                 legend.justification = c(1, 0),
                 legend.text.align = 1,
                 legend.box.just = 'right')
      
    } else {
      
      plot <-
        ggplot2::ggplot(data = data, 
                        mapping = ggplot2::aes(x = parameter, y = median)) +
        ggplot2::geom_hline(mapping = aes(yintercept = 1), 
                            colour = 'red3', 
                            linetype = 'dashed') +
        ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = lower, ymax = upper),
                               width = 0) +
        ggplot2::geom_point() +
        ggplot2::scale_x_discrete(name = '',
                                  limits = levels(data$parameter) %>% rev()) +
        ggplot2::scale_y_continuous(name = 'Relative risk', 
                                    limits = plot.limit,
                                    breaks = plot.breaks) +
        ggplot2::coord_flip() +
        helper.plotTheme(text = ggplot2::element_text(size = plot.text))
      
    }
    
    
    
    # 3. save ----
    
    ggplot2::ggsave(filename = file.path(dir.target, sprintf('%s.png', filename)),
                    plot = plot,
                    width = plot.width,
                    height = plot.height)
    
    data.table::fwrite(x = data,
                       file = file.path(dir.target, sprintf('%s.csv', filename)))
    
    # 4. return ----
    
    return(plot)
    
  }

# its-LAD plot ----

makePlot.itsLAD <- 
  function(
    data, 
    poly.rgn,
    plot.colourMedian,
    plot.colourExceedence,
    dir.target,
    filename,
    plot.text = 20,
    plot.with = 10,
    plot.height = 10
  ){
    
    # 0. function parmeters ----
    
    # data <- data.itsLAD
    # plot.colourMedian <- colours.diverging
    # plot.colourExceedence <- colours.exceedance
    # filename <- 'its_lad_XY'
    # dir.target <- dir.res.sens.modres
    
    # 1. parameters ----
    
    label.plot <- data$parameter %>% levels()
    
    plot.range <- data$median %>% range()
    plot.range.min <- plot.range[1]
    plot.range.max <- plot.range[2]
    if(plot.range.min < 1 & plot.range.max > 1) {
      plot.breaks <- c(plot.range.min, 1, plot.range.max) %>% round(x = ., digits = 3)
      plot.limits <- c(plot.range.min, plot.range.max)
    } else if (plot.range.max <= 1){
      tmp.mean <- mean(x = c(plot.range.min, 1))
      plot.breaks <- c(plot.range.min, tmp.mean, 1) %>% round(x = ., digits = 3)
      plot.limits <- c(plot.range.min, 1)
    } else {
      tmp.mean <- mean(x = c(1, plot.range.max))
      plot.breaks <- c(1, tmp.mean, plot.range.max) %>% round(x = ., digits = 3)
      plot.limits <- c(1, plot.range.max)
    }
    plot.limit.error <- 0.001
    plot.limits.2 <- c(plot.limits[1] - plot.limit.error, plot.limits[2] + plot.limit.error)
    
    # 2. plot ----
    
    plot <- 
      lapply(
        X = seq_along(label.plot),
        FUN = function(i) {
          
          # 0. function arguments ----
          
          # i <- 2
          
          # 1. parameters ----
          
          sel.label.plot <- label.plot[i]
          sel.label.inla <- 
            sel.label.plot %>% 
            stringr::str_to_lower() %>% 
            stringr::str_replace_all(pattern = '-([a-z])', 
                                     replacement = ~ stringr::str_to_upper(stringr::str_sub(.x, 2, 2))
            )
          
          # 2. filter ----
          
          data.filter <-
            data %>% 
            dplyr::filter(parameter == sel.label.plot)
          
          # 3. plot ----
          
          plot.median <-
            ggplot2::ggplot() +
            ggplot2::geom_sf(data = data.filter, 
                             mapping = ggplot2::aes(fill = median), colour = NA) +
            ggplot2::geom_sf(data = poly.rgn,
                             colour = 'black',
                             linewidth = 1,
                             fill = NA) +
            ggplot2::scale_fill_gradient2(name = '',
                                          midpoint = 1,
                                          low = plot.colourMedian[1], mid = plot.colourMedian[2], high = plot.colourMedian[3],
                                          guide = ggplot2::guide_colourbar(title.position = 'top', label.hjust = 0.5, label.vjust = 0.5),
                                          limits = plot.limits.2,
                                          breaks = plot.breaks) +
            helper.mapTheme(text = ggplot2::element_text(size = plot.text),
                         legend.position = 'bottom',
                         legend.key.width = unit(x = 2, units = 'cm'),
                         legend.title = ggplot2::element_text(hjust = 0.5))
          
          plot.exceedance.one <-
            ggplot2::ggplot() +
            ggplot2::geom_sf(data = data.filter, 
                             mapping = ggplot2::aes(fill = exceedanceOne_prob), colour = NA, show.legend = TRUE) +
            ggplot2::geom_sf(data = poly.rgn,
                             colour = 'black',
                             linewidth = 1,
                             fill = NA) +
            ggplot2::scale_fill_manual(name = '', 
                                       values = plot.colourExceedence,
                                       drop = FALSE) +
            helper.mapTheme(text = ggplot2::element_text(size = plot.text),
                         legend.position = 'bottom',
                         legend.key.width = unit(x = 2, units = 'cm'),
                         legend.title = ggplot2::element_text(hjust = 0.5))
          
          plot.exceedance.ctry <-
            ggplot2::ggplot() +
            ggplot2::geom_sf(data = data.filter, 
                             mapping = ggplot2::aes(fill = exceedanceCTRY_prob), colour = NA, show.legend = TRUE) +
            ggplot2::geom_sf(data = poly.rgn,
                             colour = 'black',
                             linewidth = 1,
                             fill = NA) +
            ggplot2::scale_fill_manual(name = '', 
                                       values = plot.colourExceedence,
                                       drop = FALSE) +
            helper.mapTheme(text = ggplot2::element_text(size = plot.text),
                         legend.position = 'bottom',
                         legend.key.width = unit(x = 2, units = 'cm'),
                         legend.title = ggplot2::element_text(hjust = 0.5))
          
          # 4. save ----
          
          ggplot2::ggsave(filename = file.path(dir.target, sprintf('%s_median_%s.png', filename, sel.label.inla)),
                          plot = plot.median,
                          width = plot.width,
                          height = plot.height)
          
          ggplot2::ggsave(filename = file.path(dir.target, sprintf('%s_exceedance_one_%s.png', filename, sel.label.inla)),
                          plot = plot.exceedance.one,
                          width = plot.width,
                          height = plot.height)
          
          ggplot2::ggsave(filename = file.path(dir.target, sprintf('%s_exceedance_ctry_%s.png', filename, sel.label.inla)),
                          plot = plot.exceedance.ctry,
                          width = plot.width,
                          height = plot.height)
          
          # 5. return ----
          
          out <- 
            list(
              plot.median = plot.median,
              plot.exceedance.one = plot.exceedance.one,
              plot.exceedance.ctry = plot.exceedance.ctry
            )
          
          return(out)
          
          
        }
      ) %>% 
      setNames(object = .,
               nm = label.plot)
    
    # 3. save ----
    
    data %>% 
      as.data.frame() %>% 
      sf::st_drop_geometry() %>% 
      data.table::as.data.table() %>% 
      .[, .(LAD11CD, LAD11NM, lad_id, RGN11NM, scenario, parameter, mean, var, median, lower, upper, exceedanceOne, exceedanceOne_prob, exceedanceCTRY, exceedanceCTRY_prob)] %>% 
      data.table::fwrite(x = .,
                         file = file.path(dir.target, sprintf('%s.csv', filename)))
    
    # 4. return ----
    
    return(plot)
    
  }

# age-sex effects plot ----

makePlot.ageSexEffects <- 
  function(
    data, 
    plot.colour = NULL,
    dir.target,
    filename,
    plot.text = 20,
    plot.with = 10,
    plot.height = 10
  ){
    
    # 0. function parmeters ----
    
    # data <- data.fixedEffects.ageSex
    # plot.colour <- colours.scenario
    
    # 1. parameters ----
    
    plot.range <- c(min(data$lower), max(data$upper))
    plot.limit <- c(floor(10*plot.range[1])/10, ceiling(10*plot.range[2])/10)
    plot.breaks <- seq(from = plot.limit[1], to = plot.limit[2], length.out = 5)
    plot.jitter <- ggplot2::position_dodge(width = 0.5)
    
    # 2. plot ----
    
    
    if(length(unique(data$scenario)) != 1) {
      
      plot <-
        ggplot2::ggplot(data = data, 
                        mapping = ggplot2::aes(x = idx, y = median, group = interaction(scenario, sex), shape = sex, colour = scenario)) +
        # labels
        ggplot2::annotate('text',
                          y = Inf, x = 1, # top right
                          hjust = 1, vjust = 1, 
                          label = 'Ref. Female [15, 25)',
                          fontface = 'bold',
                          size = plot.text * 0.3528 * 0.75, # scaled to match and then 3/4 of the size
                          colour = 'black') +
        # null
        ggplot2::geom_hline(mapping = aes(yintercept = 1), 
                            colour = 'black', 
                            linetype = 'dashed') +
        # data
        ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = lower, ymax = upper),
                               position = plot.jitter,
                               width = 0) +
        ggplot2::geom_point(size = 2,
                            position = plot.jitter) +
        ggplot2::scale_colour_manual(name = '',
                                     values = plot.colour) +
        ggplot2::scale_x_continuous(name = '',
                                    breaks = data$idx,
                                    labels = data$age,
                                    trans = 'reverse') +
        ggplot2::scale_y_continuous(name = 'Relative risk', 
                                    limits = plot.limit,
                                    breaks = plot.breaks) +
        ggplot2::coord_flip() +
        # theme
        ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1, 
                                                       label.position = 'left',
                                                       label.hjust = 1),
                        shape = ggplot2::guide_legend(ncol = 1, 
                                                      label.position = 'left',
                                                      label.hjust = 1)) +
        helper.plotTheme(text = ggplot2::element_text(size = plot.text),
                 legend.title = ggplot2::element_blank(),
                 legend.position = 'inside',
                 legend.position.inside = c(0.98, 0.02),
                 legend.justification = c(1, 0),
                 legend.text.align = 1,
                 legend.box.just = 'right')
      
    } else {
      
      plot <-
        ggplot2::ggplot(data = data, 
                        mapping = ggplot2::aes(x = idx, y = median, group = sex, shape = sex)) +
        # labels
        ggplot2::annotate('text',
                          y = Inf, x = 1, # top right
                          hjust = 1, vjust = 1, 
                          label = 'Ref. Female [15, 25)',
                          fontface = 'bold',
                          size = plot.text * 0.3528 * 0.75, # scaled to match and then 3/4 of the size
                          colour = 'black') +
        # null
        ggplot2::geom_hline(mapping = aes(yintercept = 1), 
                            colour = 'red3', 
                            linetype = 'dashed') +
        # data
        ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = lower, ymax = upper),
                               position = plot.jitter,
                               width = 0) +
        ggplot2::geom_point(size = 2,
                            position = plot.jitter) +
        ggplot2::scale_x_continuous(name = '',
                                    breaks = data$idx,
                                    labels = data$age,
                                    trans = 'reverse') +
        ggplot2::scale_y_continuous(name = 'Relative risk', 
                                    limits = plot.limit,
                                    breaks = plot.breaks) +
        ggplot2::coord_flip() +
        # theme
        ggplot2::guides(shape = ggplot2::guide_legend(ncol = 1, 
                                                      label.position = 'left',
                                                      label.hjust = 1)) +
        helper.plotTheme(text = ggplot2::element_text(size = plot.text),
                 legend.title = ggplot2::element_blank(),
                 legend.position = 'inside',
                 legend.position.inside = c(0.98, 0.02),
                 legend.justification = c(1, 0),
                 legend.text.align = 1,
                 legend.box.just = 'right')
      
    }
    
    # 3. save ----
    
    ggplot2::ggsave(filename = file.path(dir.target, sprintf('%s.png', filename)),
                    plot = plot,
                    width = plot.width,
                    height = plot.height)
    
    data.table::fwrite(x = data,
                       file = file.path(dir.target, sprintf('%s.csv', filename)))
    
    # 4. return ----
    
    return(plot)
    
  }

# socio-environmental effects plot ----

makePlot.socioEnvEffects <- 
  function(
    data, 
    plot.colour = NULL,
    dir.target,
    filename,
    plot.text = 20,
    plot.with = 10,
    plot.height = 10
  ){
    
    # 0. function parmeters ----
    
    # data <- data.fixedEffects.socioenv
    # plot.colour <- colours.scenario
    
    # 1. parameters ----
    
    plot.range <- c(min(data$lower), max(data$upper))
    plot.limit <- c(floor(10*plot.range[1])/10, ceiling(10*plot.range[2])/10)
    plot.breaks <- seq(from = plot.limit[1], to = plot.limit[2], length.out = 5)
    
    # 2. plot ----
    
    
    if(length(unique(data$scenario)) != 1) {
      
      plot.jitter <- ggplot2::position_dodge(width = 0.5)
      
      plot <-
        ggplot2::ggplot(data = data, 
                        mapping = ggplot2::aes(x = idx, y = median, group = scenario, colour = scenario)) +
        # null
        ggplot2::geom_hline(mapping = aes(yintercept = 1), 
                            colour = 'black', 
                            linetype = 'dashed') +
        # data
        ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = lower, ymax = upper),
                               position = plot.jitter,
                               width = 0) +
        ggplot2::geom_point(size = 2,
                            position = plot.jitter) +
        ggplot2::scale_colour_manual(name = '',
                                     values = plot.colour) +
        ggplot2::scale_x_continuous(name = '',
                                    breaks = data$idx,
                                    labels = data$parameter,
                                    trans = 'reverse') +
        ggplot2::scale_y_continuous(name = 'Relative risk', 
                                    limits = plot.limit,
                                    breaks = plot.breaks) +
        ggplot2::coord_flip() +
        # theme
        ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1, 
                                                       label.position = 'left',
                                                       label.hjust = 1)) +
        helper.plotTheme(text = ggplot2::element_text(size = plot.text),
                 legend.title = ggplot2::element_blank(),
                 legend.position = 'inside',
                 legend.position.inside = c(0.98, 0.02),
                 legend.justification = c(1, 0),
                 legend.text.align = 1,
                 legend.box.just = 'right')
      
    } else {
      
      plot <-
        ggplot2::ggplot(data = data, 
                        mapping = ggplot2::aes(x = idx, y = median)) +
        # null
        ggplot2::geom_hline(mapping = aes(yintercept = 1), 
                            colour = 'red3', 
                            linetype = 'dashed') +
        # data
        ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = lower, ymax = upper),
                               width = 0) +
        ggplot2::geom_point(size = 2) +
        ggplot2::scale_x_continuous(name = '',
                                    breaks = data$idx,
                                    labels = data$parameter,
                                    trans = 'reverse') +
        ggplot2::scale_y_continuous(name = 'Relative risk', 
                                    limits = plot.limit,
                                    breaks = plot.breaks) +
        ggplot2::coord_flip() +
        helper.plotTheme(text = ggplot2::element_text(size = plot.text),
                 legend.title = ggplot2::element_blank(),
                 legend.position = 'inside',
                 legend.position.inside = c(0.98, 0.02),
                 legend.justification = c(1, 0))
      
    }
    
    # 3. save ----
    
    ggplot2::ggsave(filename = file.path(dir.target, sprintf('%s.png', filename)),
                    plot = plot,
                    width = plot.width,
                    height = plot.height)
    
    data.table::fwrite(x = data,
                       file = file.path(dir.target, sprintf('%s.csv', filename)))
    
    # 4. return ----
    
    return(plot)
    
  }

# temporal random effect plot ----

makePlot.randomEffects.temporal <- 
  function(
    data,
    dir.target,
    filename,
    plot.xbreaks = seq(from = 2002, to = 2022, by = 2),
    plot.xlimits = c(2002, 2022),
    plot.text = 20,
    plot.with = 10,
    plot.height = 10
  ){
    
    # 0. function parmeters ----
    
    # res <- results
    # filename <- 'its_lad'
    # dir.target <- dir.res.main.modres
    # polygon <- poly.msoa
    # polygon.cd <- 'MSOA11CD'
    
    # 1. parameters ----
    
    # 2. plot ----
    
    plot <- 
      ggplot2::ggplot(data = data, 
                      mapping = aes(x = year, y = median)) +
      ggplot2::geom_rect(xmin = 2012, xmax = Inf,
                         ymin = -Inf, ymax = Inf,
                         fill = 'grey90', alpha = 0.1, 
                         inherit.aes = FALSE) +
      ggplot2::geom_hline(mapping = aes(yintercept = 1), 
                          colour = 'red3', linetype = 'dashed') +
      ggplot2::geom_ribbon(mapping = aes(ymin = lower, ymax = upper),
                           colour = NA, alpha = 0.5) +
      ggplot2::geom_line() +
      ggplot2::scale_y_continuous(name = 'Relative risk') +
      ggplot2::scale_x_continuous(name = '', breaks = plot.xbreaks, limits = plot.xlimits) +
      helper.plotTheme(text = ggplot2::element_text(size = plot.text),
               legend.position = 'inside',
               legend.position.inside = c(0.02, 0.98),
               legend.justification = c(0, 1),
               legend.background = ggplot2::element_blank(),
               legend.box.background = ggplot2::element_blank())
    
    # 3. save ----
    
    ggplot2::ggsave(filename = file.path(dir.target, sprintf('%s.png', filename)),
                    plot = plot,
                    width = plot.width,
                    height = plot.height)
    
    # 4. return ----
    
    return(plot)
    
  }

# spatial random effect plot ----

makePlot.randomEffects.spatial <- 
  function(
    data, 
    poly.rgn,
    plot.colourMedian,
    plot.colourExceedence,
    dir.target,
    filename,
    plot.text = 20,
    plot.with = 10,
    plot.height = 10
  ){
    
    # 0. function parmeters ----
    
    # res <- results
    # filename <- 'its_lad'
    # dir.target <- dir.res.main.modres
    # polygon <- poly.msoa
    # polygon.cd <- 'MSOA11CD'
    
    # 1. parameters ----
    
    plot.max <- max(abs(data$median - 1), na.rm = TRUE) %>% round(., digits = 2)
    
    # 2. plot ----
    
    plot.median <-
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = data, 
                       mapping = ggplot2::aes(fill = median), colour = NA) +
      ggplot2::geom_sf(data = poly.rgn,
                       colour = 'black',
                       linewidth = 1,
                       fill = NA) +
      ggplot2::scale_fill_gradient2(name = '',
                                    midpoint = 1,
                                    low = plot.colourMedian[1], mid = plot.colourMedian[2], high = plot.colourMedian[3],
                                    guide = ggplot2::guide_colourbar(title.position = 'top', label.hjust = 0.5, label.vjust = 0.5),
                                    limits = c(1-plot.max, 1+plot.max),
                                    breaks = c(1-plot.max, 1, 1+plot.max)) +
      helper.mapTheme(text = ggplot2::element_text(size = plot.text),
                   legend.position = 'bottom',
                   legend.key.width = unit(x = 2, units = 'cm'),
                   legend.title = ggplot2::element_text(hjust = 0.5))
    
    plot.exceedence <- 
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = data, 
                       mapping = ggplot2::aes(fill = exceedanceProb), colour = NA, show.legend = TRUE) +
      ggplot2::geom_sf(data = poly.rgn,
                       colour = 'black',
                       linewidth = 1,
                       fill = NA) +
      ggplot2::scale_fill_manual(name = '', 
                                 values = plot.colourExceedence,
                                 drop = FALSE) +
      helper.mapTheme(text = ggplot2::element_text(size = plot.text),
                   legend.position = 'bottom',
                   legend.key.width = unit(x = 2, units = 'cm'),
                   legend.title = ggplot2::element_text(hjust = 0.5))
    
    # 3. save ----
    
    ggplot2::ggsave(filename = file.path(dir.target, sprintf('%s_median.png', filename)),
                    plot = plot.median,
                    width = plot.width,
                    height = plot.height)
    
    ggplot2::ggsave(filename = file.path(dir.target, sprintf('%s_exceedance.png', filename)),
                    plot = plot.exceedence,
                    width = plot.width,
                    height = plot.height)
    
    
    # 4. return ----
    
    out <- 
      list(
        plot.median = plot.median,
        plot.exceedence = plot.exceedence
      )
    
    return(out)
    
  }

# spatio-temporal random effect plot ----

makePlot.randomEffects.spatiotemporal <- 
  function(
    data,
    plot.colourMedian,
    plot.colourExceedence,
    dir.target,
    filename,
    plot.xbreaks = seq(from = 2002, to = 2022, by = 2),
    plot.xlimits = c(2001.5, 2022.5),
    plot.text = 20,
    plot.with = 10,
    plot.height = 10
  ){
    
    # 0. function parmeters ----
    
    # data <- data.randomEffects.spatiotemporal
    # plot.colourMedian <- colours.diverging
    # plot.colourExceedence <- colours.exceedance
    # plot.xbreaks = seq(from <- 2002, to = 2022, by = 2)
    # plot.xlimits <- c(2001.5, 2022.5)
    # plot.text <- 20
    # plot.with <- 10
    # plot.height <- 10
    
    # 1. parameters ----
    
    plot.max <- max(abs(data$median - 1), na.rm = TRUE) # %>% round(., digits = 2)
    
    plot.order.st.rgn <-
      data %>% 
      .[, .(
        median = median(x = median)
      ),
      by = 'RGN11NM'] %>% 
      .[, idx.rgn := rank(median, ties.method = 'first')]
    
    plot.order.st.rgn.msoa <-
      data %>% 
      .[, .(
        median = median(x = median)
      ),
      by = c('MSOA11CD', 'RGN11NM')] %>% 
      merge(x = .,
            y = plot.order.st.rgn %>% .[, .(RGN11NM, idx.rgn)],
            by = 'RGN11NM',
            all.x = TRUE) %>% 
      .[, idx.msoa.rgn := rank(x = median, ties.method = 'first'), by = idx.rgn] %>% 
      data.table::setorder(x = .,
                           idx.rgn, idx.msoa.rgn) %>% 
      .[, idx := .I]
    
    plot.breaks.st.rgn.msoa <-
      data.table::copy(plot.order.st.rgn.msoa) %>%
      .[, c('idx.min', 'idx.max', 'idx.mid') := {
        .(idx[which.min(idx.msoa.rgn)] - 0.5,
          idx[which.max(idx.msoa.rgn)] + 0.5,
          idx[idx.msoa.rgn == round(median(idx.msoa.rgn))][1L])
      },
      by = idx.rgn] %>% 
      .[, .(RGN11NM, idx.min, idx.max, idx.mid)] %>% 
      unique()
    
    data.2 <-
      data %>% 
      data.table::copy() %>%
      merge(x = .,
            y = plot.order.st.rgn.msoa %>% .[, .(MSOA11CD, RGN11NM, idx.rgn, idx.msoa.rgn, idx)],
            by = c('MSOA11CD', 'RGN11NM'),
            all.x = TRUE)
    
    # 2. plot ----
    
    plot.median <- 
      ggplot2::ggplot(data = data.2) +
      ggplot2::geom_tile(mapping = ggplot2::aes(x = year, y = idx, fill = median)) +
      ggplot2::scale_fill_gradient2(name = '',
                                    midpoint = 1,
                                    low = plot.colourMedian[1], mid = plot.colourMedian[2], high = plot.colourMedian[3],
                                    guide = ggplot2::guide_colourbar(title.position = 'top', label.hjust = 0.5, label.vjust = 0.5),
                                    limits = c(1-plot.max, 1+plot.max),
                                    breaks = c(1-plot.max, 1, 1+plot.max)) +
      ggplot2::scale_y_continuous(name = 'MSOA') +
      ggplot2::scale_x_continuous(name = '', breaks = plot.xbreaks, limits = plot.xlimits) +
      ## splitting by region
      ### lines
      ggplot2::geom_segment(data = data.frame(x = rep(-Inf, times = (nrow(plot.breaks.st.rgn.msoa)-1)), xend = rep(Inf, times = (nrow(plot.breaks.st.rgn.msoa)-1)),
                                              y = plot.breaks.st.rgn.msoa$idx.min[-1], yend = plot.breaks.st.rgn.msoa$idx.min[-1]),
                            aes(x = x, xend = xend, y = y, yend = yend),
                            colour = 'black') +
      ### labels
      ggplot2::annotate('text',
                        x = rep(2002, times = nrow(plot.breaks.st.rgn.msoa)),
                        y = plot.breaks.st.rgn.msoa$idx.mid,
                        label = plot.breaks.st.rgn.msoa$RGN11NM,
                        fontface = 'bold',
                        colour = 'black',
                        size = plot.text * 0.3528 * 0.75,
                        hjust = 0) +
      helper.plotTheme(text = ggplot2::element_text(size = plot.text),
               legend.position = 'bottom',
               legend.key.width = unit(x = 2, units = 'cm'),
               legend.title = ggplot2::element_text(hjust = 0.5),
               axis.text.y = ggplot2::element_blank(),
               axis.ticks.y = ggplot2::element_blank())
    
    plot.exceedence <- 
      ggplot2::ggplot(data = data.2) +
      ggplot2::geom_tile(mapping = ggplot2::aes(x = year, y = idx, fill = exceedanceProb), show.legend = TRUE) +
      ggplot2::scale_fill_manual(name = '', 
                                 values = plot.colourExceedence,
                                 drop = FALSE) +
      ggplot2::scale_y_continuous(name = '') +
      ggplot2::scale_x_continuous(name = '', breaks = plot.xbreaks, limits = plot.xlimits) +
      ## splitting by region
      ### lines
      ggplot2::geom_segment(data = data.frame(x = rep(-Inf, times = (nrow(plot.breaks.st.rgn.msoa)-1)), xend = rep(Inf, times = (nrow(plot.breaks.st.rgn.msoa)-1)),
                                              y = plot.breaks.st.rgn.msoa$idx.min[-1], yend = plot.breaks.st.rgn.msoa$idx.min[-1]),
                            aes(x = x, xend = xend, y = y, yend = yend),
                            colour = 'black') +
      # ### labels
      # ggplot2::annotate('text',
      #                   x = rep(2002, times = nrow(plot.breaks.st.rgn.msoa)),
      #                   y = plot.breaks.st.rgn.msoa$idx.mid,
      #                   label = plot.breaks.st.rgn.msoa$RGN11NM,
      #                   fontface = 'bold',
      #                   colour = 'black',
      #                   size = plot.text * 0.3528 * 0.75,
      #                   hjust = 0) +
      helper.plotTheme(text = ggplot2::element_text(size = plot.text),
               legend.position = 'bottom',
               legend.key.width = unit(x = 2, units = 'cm'),
               legend.title = ggplot2::element_text(hjust = 0.5),
               axis.text.y = ggplot2::element_blank(),
               axis.ticks.y = ggplot2::element_blank())
    
    
    # 3. save ----
    
    ggplot2::ggsave(filename = file.path(dir.target, sprintf('%s_median.png', filename)),
                    plot = plot.median,
                    width = plot.width,
                    height = plot.height)
    
    ggplot2::ggsave(filename = file.path(dir.target, sprintf('%s_exceedance.png', filename)),
                    plot = plot.exceedence,
                    width = plot.width,
                    height = plot.height)
    
    
    
    # 4. return ----
    
    out <- 
      list(
        plot.median = plot.median,
        plot.exceedence = plot.exceedence
      )
    
    return(out)
    
  }

# hyperparameter table ----

makeTable.hyperparameters <-
  function(
    data, 
    dir.target,
    filename) {
    
    # 0. function parmeters ----
    
    # res <- results
    # filename <- 'its_lad'
    # dir.target <- dir.res.main.modres
    
    # 1. parameters ----
    
    # 2. save ----
    
    data.table::fwrite(x = data,
                       file = file.path(dir.target, sprintf('%s.csv', filename)))
    
    # 3. return ----
    
    return(data)
    
  }

