# 1. parameters ----

colours.diverging <- c('#008B00', '#E9E0C8', '#CD0000')
colours.exceedance <- c('#A0D6D0', '#009B92', '#00768D')

colours.scenario <- c('Primary model' = '#104E88', 'Alternative model' = '#FF8C00')
fill.scenario <- c('Primary model' = '#87CEEB', 'Alternative model' = '#FFD39B')

colours.scenario.2 <- c('Primary model' = '#104E88', 'National primary model' = '#2E8B57')
fill.scenario.2 <- c('Primary model' = '#87CEEB', 'National primary model' = '#9FD5B3')

colours.scenario.3 <- c('Primary model' = '#104E88', 'Alternative model' = '#FF8C00', 'National primary model' = '#2E8B57')
fill.scenario.3 <- c('Primary model' = '#87CEEB', 'Alternative model' = '#FFD39B', 'National primary model' = '#9FD5B3')

scale.label <- format(100000, big.mark = ',', scientific = FALSE)
plot.text <- 20
plot.width <- plot.height <- 10

rownames.its <- c('year_id', 'intervention_idpost', 'period_id')
rownames.its.formal <- c('Time', 'Immediate', 'Sustained')
rownames.ageSex <- c('agesex_id15_24-female', 'agesex_id25_34-female', 'agesex_id35_44-female', 'agesex_id45_54-female', 'agesex_id55_64-female', 'agesex_id65+-female',
                     'agesex_id15_24-male', 'agesex_id25_34-male', 'agesex_id35_44-male', 'agesex_id45_54-male', 'agesex_id55_64-male', 'agesex_id65+-male')
rownames.ageSex.formal <- c('Female [15, 25) vs. Ref.', 'Female [25, 35) vs. Ref.', 'Female [35, 45) vs. Ref.', 'Female [45, 55) vs. Ref.', 'Female [55, 65) vs. Ref.', 'Female 65+ vs. Ref.',
                            'Male [15, 25) vs. Ref.', 'Male [25, 35) vs. Ref.', 'Male [35, 45) vs. Ref.', 'Male [45, 55) vs. Ref.', 'Male [55, 65) vs. Ref.', 'Male 65+ vs. Ref.')
rownames.socioenv <- c('deprivation_id', 'ethnicDensity_id', 'populationDensity_id', 'lightPollution_id', 'railwayDensity_id', 'roadwayDensity_id', 'greenspace_id', 'mentalHealth_id')
rownames.socioenv.formal <- c('Deprivation', 'Ethnic Density', 'Population Density', 'Light Pollution', 'Railway Density', 'Roadway Density', 'Greenspace', 'Mental Wellbeing')
rownames.hyper <- c('zero-probability parameter for zero-inflated poisson_1',
                    'Precision for year2_id',
                    'Precision for msoa_id', 'Phi for msoa_id',
                    'Precision for msoa_year_id',
                    'Precision for lad_id', 'Phi for lad_id',
                    'Precision for lad2_id', 'Phi for lad2_id', 
                    'Precision for lad3_id', 'Phi for lad3_id')
rownames.hyper.formal <-
  c('Overdispersion',
    'Temporal Precision',
    'Spatial Precision', 'Spatial Mixing',
    'Spatiotemporal Perecision',
    'LTLA Overall Time Precision', 'LTLA Overall Time Spatial Mixing',
    'LTLA Immediate Precision', 'LTLA Immediate Spatial Mixing',
    'LTLA Sustained Precision', 'LTLA Sustained Spatial Mixing')

# 2. format results ----

results <- 
  helper.loadOrRun(
    filepath = file.path(dir.res.main.mfit, 'model_posteriorsummary_factual_main.rds'), 
    FUN =  stop(),
    message.run = 'Stop: Need to run from model fitting code.',
    message.load = 'Loading results summary'
  )

results.cfact <- 
  helper.loadOrRun(
    filepath = file.path(dir.res.main.mfit, 'model_posteriorsummary_counterfactual_main.rds'), 
    FUN =  stop(),
    message.run = 'Stop: Need to run from model fitting code.',
    message.load = 'Loading results counterfactual summary'
  )

# 3. year x national plot ----

data.yrCTRY <-
  list(
    results$summary.yearCTRY %>% data.table::copy() %>% .[, scenario := 'primary'],
    results.cfact$summary.yearCTRY %>% data.table::copy() %>% .[, scenario := 'alternative']
  ) %>% 
  data.table::rbindlist() %>% 
  .[, scenario := scenario %>% factor(x = ., levels = c('primary', 'alternative'), labels = c('Primary model', 'Alternative model'))]

plot.yrCTRY <-
  makePlot.yrCTRY(
    data = data.yrCTRY,
    filename = 'year_ctry',
    dir.target = dir.res.main.res,
    plot.colour = colours.scenario,
    plot.fill = fill.scenario,
    plot.scaleLabel = scale.label
  )

rm(
  data.yrCTRY,
  plot.yrCTRY
)
gc()

# 4.a. year x region x cf plot ----

data.yrRGN <-
  list(
    results$summary.yearRGN %>% data.table::copy() %>% .[, scenario := 'primary'],
    results.cfact$summary.yearRGN %>% data.table::copy() %>% .[, scenario := 'alternative']
  ) %>% 
  data.table::rbindlist() %>% 
  .[, scenario := scenario %>% factor(x = ., levels = c('primary', 'alternative'), labels = c('Primary model', 'Alternative model'))]

plot.yrRGN <-
  makePlot.yrRGN(
    data = data.yrRGN,
    filename = 'year_rgn',
    dir.target = dir.res.main.res,
    plot.colour = colours.scenario,
    plot.fill = fill.scenario,
    plot.scaleLabel = scale.label
  )

rm(
  data.yrRGN,
  plot.yrRGN
)
gc()

# 4.b. year x region x nat plot ----

data.yrRGN <-
  list(
    results$summary.yearRGN %>% data.table::copy() %>% .[, scenario := 'primary'],
    data.table::CJ(
      RGN11NM = lookup.msoa.rgn$RGN11NM %>% unique() %>% grep(pattern = 'Wales', x = ., value = TRUE, invert = TRUE),
      year = 2002:2022
    ) %>% 
      merge(
        x = .,
        y = results$summary.yearCTRY,
        by = 'year',
        all.x = TRUE
      ) %>% 
      .[, scenario := 'national']
  ) %>% 
  data.table::rbindlist(l = ., 
                        use.names = TRUE) %>% 
  .[, scenario := scenario %>% factor(x = ., levels = c('primary', 'national'), labels = c('Primary model', 'National primary model'))]

plot.yrRGN <-
  makePlot.yrRGN(
    data = data.yrRGN,
    filename = 'year_rgn_nat',
    dir.target = dir.res.main.res,
    plot.colour = colours.scenario.2,
    plot.fill = fill.scenario.2,
    plot.scaleLabel = scale.label
  )

rm(
  data.yrRGN,
  plot.yrRGN
)
gc()


# 4.c. year x region x nat plot ----

data.yrRGN <-
  list(
    results$summary.yearRGN %>% data.table::copy() %>% .[, scenario := 'primary'],
    results.cfact$summary.yearRGN %>% data.table::copy() %>% .[, scenario := 'alternative'],
    data.table::CJ(
      RGN11NM = lookup.msoa.rgn$RGN11NM %>% unique() %>% grep(pattern = 'Wales', x = ., value = TRUE, invert = TRUE),
      year = 2002:2022
    ) %>% 
      merge(
        x = .,
        y = results$summary.yearCTRY,
        by = 'year',
        all.x = TRUE
      ) %>% 
      .[, scenario := 'national']
  ) %>% 
  data.table::rbindlist(l = ., 
                        use.names = TRUE) %>% 
  .[, scenario := scenario %>% factor(x = ., levels = c('primary', 'alternative', 'national'), labels = c('Primary model', 'Alternative model', 'National primary model'))]

plot.yrRGN <-
  makePlot.yrRGN(
    data = data.yrRGN,
    filename = 'year_rgn_nat_cf',
    dir.target = dir.res.main.res,
    plot.colour = colours.scenario.3,
    plot.fill = fill.scenario.3,
    plot.scaleLabel = scale.label
  )

rm(
  data.yrRGN,
  plot.yrRGN
)
gc()

# 5. year x lad plot ----

data.yrLAD <-
  results$summary.yearLAD %>% 
  data.table::copy()

plot.yrLAD <-
  makePlot.yrLAD(
    data = data.yrLAD, 
    filename = 'year_lad',
    dir.target = dir.res.main.res,
    plot.scaleLabel = scale.label
  )

rm(
  data.yrLAD,
  plot.yrLAD
)
gc()

# 6. year x msoa plot ----

data.yrMSOA <-
  results$summary.yearMSOA %>% 
  data.table::copy()

plot.yrMSOA <-
  makePlot.yrMSOA(
    data = data.yrMSOA, 
    filename = 'year_msoa',
    dir.target = dir.res.main.res,
    plot.scaleLabel = scale.label
  )

rm(
  data.yrMSOA,
  plot.yrMSOA
)
gc()

# 7 its x national plot ----

data.itsCTRY <-
  results$summary.itsCTRY %>% 
  data.table::copy() %>% 
  .[, `:=` (
    scenario = 'primary' %>% factor(x = ., levels = c('primary'), labels = c('Primary model')),
    parameter = parameter %>% factor(x = ., levels = rownames.its, labels = rownames.its.formal)
  )]

plot.itsCTRY <- 
  makePlot.itsCTRY(
    data = data.itsCTRY, 
    filename = 'its_ctry',
    dir.target = dir.res.main.res
  )

rm(
  data.itsCTRY,
  plot.itsCTRY
)
gc()

# 8. its x lad plot ----

rownames.its.short <- c('intervention_idpost', 'period_id')
rownames.its.short.formal <- c('Immediate', 'Sustained')

data.itsLAD <-
  results$summary.itsLAD %>% 
  data.table::copy() %>%
  .[, `:=` (
    scenario = 'primary' %>% factor(x = ., levels = c('primary'), labels = c('Primary model')),
    parameter = parameter %>% factor(x = ., levels = rownames.its.short, labels = rownames.its.short.formal)
  )] %>% 
  .[!is.na(parameter)] %>%
  merge(x = ., 
        y = poly.lad,
        by = c('LAD11CD', 'LAD11NM'),
        all.x = TRUE) %>% 
  sf::st_as_sf()

plot.itsLAD <- 
  makePlot.itsLAD(
    data = data.itsLAD, 
    poly.rgn = poly.rgn,
    plot.colourMedian = colours.diverging,
    plot.colourExceedence = colours.exceedance,
    filename = 'its_lad',
    dir.target = dir.res.main.res
  )

rm(
  rownames.its.short,
  rownames.its.short.formal,
  data.itsLAD,
  plot.itsLAD
)
gc()

# 9. fixed effects ----

data.fixedEffects <-
  list(results$summary.fixedEffects %>% data.table::copy(),
       data.table::data.table(parameter = 'agesex_id15_24-female', mean = 1, var = 1, median = 1, lower = 1, upper = 1)
  ) %>%
  data.table::rbindlist() %>% 
  .[!(parameter %in% c('(Intercept)', rownames.its))] %>% 
  .[, `:=` (
    scenario = 'primary' %>% factor(x = ., levels = c('primary'), labels = c('Primary model')),
    parameter = parameter %>% factor(x = ., levels = c(rownames.ageSex, rownames.socioenv), labels = c(rownames.ageSex.formal, rownames.socioenv.formal))
  )]

data.fixedEffects.ageSex <- 
  data.fixedEffects %>% 
  .[parameter %in% rownames.ageSex.formal] %>% 
  .[, `:=` (
    sex = parameter %>% gsub(x = ., pattern = '(ale).*', replacement = '\\1') %>% factor(x = ., levels = c('Male', 'Female')),
    age = parameter %>% gsub(x = ., pattern = '.*ale ', replacement = '') %>% factor(x = ., levels = sprintf('%s vs. Ref.', c('[15, 25)', '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)', '65+')))
  )] %>% 
  .[order(age, sex)] %>% 
  .[, idx := age %>% as.numeric()]

data.fixedEffects.socioenv <- 
  data.fixedEffects %>% 
  .[parameter %in% rownames.socioenv.formal] %>% 
  .[order(parameter)] %>% 
  .[, idx := parameter %>% as.numeric()]  

plot.fixedEffects.ageSex <-
  makePlot.ageSexEffects(
    data = data.fixedEffects.ageSex,
    plot.colour = NULL,
    filename = 'fixed_effects_agesex',
    dir.target = dir.res.main.res
  )

plot.fixedEffects.socioenv <-
  makePlot.socioEnvEffects(
    data = data.fixedEffects.socioenv,
    plot.colour = NULL,
    filename = 'fixed_effects_socioenv',
    dir.target = dir.res.main.res
  )

rm(
  data.fixedEffects,
  data.fixedEffects.ageSex,
  data.fixedEffects.socioenv,
  plot.fixedEffects.ageSex,
  plot.fixedEffects.socioenv
)
gc()

# 10. random effects ----

data.randomEffects.temporal <-
  results$summary.randomEffects$temporal

data.randomEffects.spatial <-
  results$summary.randomEffects$spatial %>% 
  merge(x = .,
        y = poly.msoa,
        by = 'MSOA11CD',
        all.x = TRUE) %>% 
  sf::st_as_sf()

data.randomEffects.spatiotemporal <-
  results$summary.randomEffects$spatiotemporal

plot.randomEffects.temporal <- 
  makePlot.randomEffects.temporal(
    data = data.randomEffects.temporal, 
    filename = 'random_effects_temporal',
    dir.target = dir.res.main.res
  )

plot.randomEffects.spatial <- 
  makePlot.randomEffects.spatial(
    data = data.randomEffects.spatial,  
    poly.rgn = poly.rgn,
    plot.colourMedian = colours.diverging,
    plot.colourExceedence = colours.exceedance,
    filename = 'random_effects_spatial',
    dir.target = dir.res.main.res
  )

plot.randomEffects.spatiotemporal <- 
  makePlot.randomEffects.spatiotemporal(
    data = data.randomEffects.spatiotemporal,  
    plot.colourMedian = colours.diverging,
    plot.colourExceedence = colours.exceedance,
    filename = 'random_effects_spatiotemporal',
    dir.target = dir.res.main.res
  )

rm(
  data.randomEffects.temporal,
  data.randomEffects.spatial,
  data.randomEffects.spatiotemporal,
  plot.randomEffects.temporal,
  plot.randomEffects.spatial,
  plot.randomEffects.spatiotemporal
)
gc()

# 11. hyper parameters ----

data.hyperparameters <- 
  results$summary.hyperpriors %>% 
  data.table::copy() %>% 
  .[, parameter := parameter %>% factor(x = ., levels = rownames.hyper, labels = rownames.hyper.formal)]

table.hyperparameters <-
  makeTable.hyperparameters(
    data = data.hyperparameters, 
    filename = 'hyperparameters',
    dir.target = dir.res.main.res
  )

rm(
  data.hyperparameters,
  table.hyperparameters
)
gc()

# 99. miscellaneous ----

## 99.1. number of LTLA with short- and long-term changes -----

### 99.1.0. import ----

tmp.ltla.rgn.total <-
  lookup.msoa.rgn %>%
  .[grep(pattern = '^E', x = LAD11CD)] %>%
  .[, c('LAD11NM', 'RGN11NM'), with = TRUE] %>%
  unique() %>%
  .[, ltla.per.rgn := 1] %>%
  .[, .(
    ltla.per.rgn = ltla.per.rgn %>% sum()
  ),
  by = 'RGN11NM'] %>%
  .[order(RGN11NM)]

tmp.ltla.ctry.total <-
  lookup.msoa.rgn %>%
  .[grep(pattern = '^E', x = LAD11CD)] %>%
  .[, c('LAD11NM', 'RGN11NM'), with = TRUE] %>%
  unique() %>%
  .[, `:=` (
    country = 'England',
    ltla.per.ctry = 1
  )] %>%
  .[, .(
    ltla.per.ctry = ltla.per.ctry %>% sum()
  ),
  by = 'country']

### 99.1.1. short-term increase ----

tmp.short.inc.rgn <-
  file.path(dir.res.main.res, 'its_lad.csv') %>%
  data.table::fread(file = .) %>%
  .[parameter == 'Immediate'] %>%
  .[exceedanceCTRY_prob == '(80%, 100%]'] %>%
  .[, total.count := 1] %>%
  .[, .(
    total.count = total.count %>% sum()
  ),
  by = 'RGN11NM'] %>%
  .[order(RGN11NM)] %>%
  merge(x = .,
        y = tmp.ltla.rgn.total,
        by = 'RGN11NM',
        all.x = TRUE) %>%
  .[, total.percent := round(100 * total.count / ltla.per.rgn, digits = 2)]

tmp.short.inc.ctry <-
  file.path(dir.res.main.res, 'its_lad.csv') %>%
  data.table::fread(file = .) %>%
  .[parameter == 'Immediate'] %>%
  .[exceedanceCTRY_prob == '(80%, 100%]'] %>%
  .[, `:=` (
    country = 'England',
    total.count = 1
  )] %>%
  .[, .(
    total.count = total.count %>% sum()
  ),
  by = 'country'] %>%
  merge(x = .,
        y = tmp.ltla.ctry.total,
        by = 'country',
        all.x = TRUE) %>%
  .[, total.percent := round(100 * total.count / ltla.per.ctry, digits = 2)]

### 99.1.2. short-term decrease ----

tmp.short.dec.rgn <-
  file.path(dir.res.main.res, 'its_lad.csv') %>%
  data.table::fread(file = .) %>%
  .[parameter == 'Immediate'] %>%
  .[exceedanceCTRY_prob == '[0%, 20%]'] %>%
  .[, total.count := 1] %>%
  .[, .(
    total.count = total.count %>% sum()
  ),
  by = 'RGN11NM'] %>%
  .[order(RGN11NM)] %>%
  merge(x = .,
        y = tmp.ltla.rgn.total,
        by = 'RGN11NM',
        all.x = TRUE) %>%
  .[, total.percent := round(100 * total.count / ltla.per.rgn, digits = 2)]

tmp.short.dec.cty <-
  file.path(dir.res.main.res, 'its_lad.csv') %>%
  data.table::fread(file = .) %>%
  .[parameter == 'Immediate'] %>%
  .[exceedanceCTRY_prob == '[0%, 20%]'] %>%
  .[, `:=` (
    country = 'England',
    total.count = 1
  )] %>%
  .[, .(
    total.count = total.count %>% sum()
  ),
  by = 'country'] %>%
  merge(x = .,
        y = tmp.ltla.ctry.total,
        by = 'country',
        all.x = TRUE) %>%
  .[, total.percent := round(100 * total.count / ltla.per.ctry, digits = 2)]

### 99.1.3. long-term increase ----

tmp.long.inc.rgn <-
  file.path(dir.res.main.res, 'its_lad.csv') %>%
  data.table::fread(file = .) %>%
  .[parameter == 'Sustained'] %>%
  .[exceedanceCTRY_prob == '(80%, 100%]'] %>%
  .[, total.count := 1] %>%
  .[, .(
    total.count = total.count %>% sum()
  ),
  by = 'RGN11NM'] %>%
  .[order(RGN11NM)] %>%
  merge(x = .,
        y = tmp.ltla.rgn.total,
        by = 'RGN11NM',
        all.x = TRUE) %>%
  .[, total.percent := round(100 * total.count / ltla.per.rgn, digits = 2)]

tmp.long.inc.ctry <-
  file.path(dir.res.main.res, 'its_lad.csv') %>%
  data.table::fread(file = .) %>%
  .[parameter == 'Sustained'] %>%
  .[exceedanceCTRY_prob == '(80%, 100%]'] %>%
  .[, `:=` (
    country = 'England',
    total.count = 1
  )] %>%
  .[, .(
    total.count = total.count %>% sum()
  ),
  by = 'country'] %>%
  merge(x = .,
        y = tmp.ltla.ctry.total,
        by = 'country',
        all.x = TRUE) %>%
  .[, total.percent := round(100 * total.count / ltla.per.ctry, digits = 2)]

### 99.1.4. long-term decrease ----

tmp.long.dec.rgn <-
  file.path(dir.res.main.res, 'its_lad.csv') %>%
  data.table::fread(file = .) %>%
  .[parameter == 'Sustained'] %>%
  .[exceedanceCTRY_prob == '[0%, 20%]'] %>%
  .[, total.count := 1] %>%
  .[, .(
    total.count = total.count %>% sum()
  ),
  by = 'RGN11NM'] %>%
  .[order(RGN11NM)] %>%
  merge(x = .,
        y = tmp.ltla.rgn.total,
        by = 'RGN11NM',
        all.x = TRUE) %>%
  .[, total.percent := round(100 * total.count / ltla.per.rgn, digits = 2)]

tmp.long.dec.cty <-
  file.path(dir.res.main.res, 'its_lad.csv') %>%
  data.table::fread(file = .) %>%
  .[parameter == 'Sustained'] %>%
  .[exceedanceCTRY_prob == '[0%, 20%]'] %>%
  .[, `:=` (
    country = 'England',
    total.count = 1
  )] %>%
  .[, .(
    total.count = total.count %>% sum()
  ),
  by = 'country'] %>%
  merge(x = .,
        y = tmp.ltla.ctry.total,
        by = 'country',
        all.x = TRUE) %>%
  .[, total.percent := round(100 * total.count / ltla.per.ctry, digits = 2)]

## 99.2. suicide rate per year ----

### 99.2.0 import ----

tmp.yr.ctry <-
  file.path(dir.res.main.res, 'year_ctry.csv') %>%
  data.table::fread(file = .)

### 99.2.1. range ----

tmp.yr.ctry.range <-
  tmp.yr.ctry %>%
  .[scenario == 'Primary model'] %>%
  .[year %in% c(2002, 2022)] %>%
  .[, .(year, median, lower, upper)] %>%
  round(x = ., digits = 2)

### 99.2.2. high ----

tmp.yr.ctry.high <-
  tmp.yr.ctry %>%
  .[scenario == 'Primary model'] %>%
  .[median == max(median)] %>%
  .[, .(year, median, lower, upper)] %>%
  round(x = ., digits = 2)

### 99.2.3. low ----

tmp.yr.ctry.low <-
  tmp.yr.ctry %>%
  .[scenario == 'Primary model'] %>%
  .[median == min(median)] %>%
  .[, .(year, median, lower, upper)] %>%
  round(x = ., digits = 2)

## 99.3. age-sex interaction ----

### 99.3.0. import ----

tmp.posterior <-
  file.path(dir.res.main.mfit, 'model_posterior_factual_main.rds') %>%
  helper.loadOrRun(
    filepath = .,
    FUN = NULL,
    message.run = 'Need to run model posterior...',
    message.load = 'Loading model posterior...')

tmp.posterior.fixed <-
  tmp.posterior %>%
  format.fixedeffects(posterior = .)

colnames.theta <- sprintf('theta:%s', 1:1000)

### 99.3.1. all ages male vs female ----

tmp.sex <-
  # 1. add alternative
  list(
    data.table::data.table(parameter = 'agesex_id15_24-female') %>%
      .[, (colnames.theta) := 0],
    tmp.posterior.fixed
  ) %>%
  data.table::rbindlist() %>%
  # 2. extract agesex
  .[grepl(pattern = '^agesex_id', x = parameter)] %>%
  # 3. turn to long
  .[, c('age', 'sex') := data.table::tstrsplit(x = sub(pattern = '^agesex_id', replacement = '', x = parameter), '-', fixed = TRUE)] %>%
  melt(
    data = .,
    id.vars = c('sex'),
    measure.vars = colnames.theta,
    variable.name = 'theta',
    value.name = 'value'
  ) %>%
  # 4. calculate percetage increast
  ## 4.1. inverse log
  .[, value := value %>% exp()] %>%
  ## 4.2. avergae over age
  .[, .(value = mean(value, na.rm = TRUE)), by = .(sex, theta)] %>%
  ## 4.3. calculate percentage
  .[, .(
    pct = (value[sex == 'male'] - value[sex == 'female']) / value[sex == 'female']
  ),
  by = .(theta)
  ] %>%
  ## 4.4. add label
  .[, sex := 'male:female'] %>%
  # 5. make wide
  data.table::dcast(
    data = .,
    formula = sex ~ theta,
    value.var = 'pct'
  ) %>%
  # 6. summarise
  {
    dt <- .
    m <- as.matrix(dt[, ..colnames.theta])
    dt[, `:=` (
      mean = Matrix::rowMeans(x = m) %>% round(x = ., digits = 2),
      var = matrixStats::rowVars(x = m) %>% round(x = ., digits = 2),
      median = matrixStats::rowMedians(x = m) %>% round(x = ., digits = 2),
      lower = matrixStats::rowQuantiles(x = m, probs = 0.025) %>% round(x = ., digits = 2),
      upper = matrixStats::rowQuantiles(x = m, probs = 0.975) %>% round(x = ., digits = 2)
    )]
    dt
  } %>%
  .[, -colnames.theta, with = FALSE] %>%
  .[, .(sex, median, lower, upper)]

### 99.3.2. by age ----

tmp.age <-
  # 1. add alternative
  list(
    data.table::data.table(parameter = 'agesex_id15_24-female') %>%
      .[, (colnames.theta) := 0],
    tmp.posterior.fixed
  ) %>%
  data.table::rbindlist() %>%
  # 2. extract agesex
  .[grepl(pattern = '^agesex_id', x = parameter)] %>%
  # 3. turn to long
  .[, c('age', 'sex') := data.table::tstrsplit(x = sub(pattern = '^agesex_id', replacement = '', x = parameter), '-', fixed = TRUE)] %>%
  melt(
    data = .,
    id.vars = c('age', 'sex'),
    measure.vars = colnames.theta,
    variable.name = 'theta',
    value.name = 'value'
  ) %>%
  # 4. calculate percetage increast
  ## 4.1. inverse log
  .[, value := value %>% exp()] %>%
  ## 4.2. calculate percentage
  .[, .(
    pct = (value[sex == 'male'] - value[sex == 'female']) / value[sex == 'female']
  ),
  by = .(age, theta)
  ] %>%
  # 5. make wide
  data.table::dcast(
    data = .,
    formula = age ~ theta,
    value.var = 'pct'
  ) %>%
  # 6. summarise
  {
    dt <- .
    m <- as.matrix(dt[, ..colnames.theta])
    dt[, `:=` (
      mean = Matrix::rowMeans(x = m) %>% round(x = ., digits = 2),
      var = matrixStats::rowVars(x = m) %>% round(x = ., digits = 2),
      median = matrixStats::rowMedians(x = m) %>% round(x = ., digits = 2),
      lower = matrixStats::rowQuantiles(x = m, probs = 0.025) %>% round(x = ., digits = 2),
      upper = matrixStats::rowQuantiles(x = m, probs = 0.975) %>% round(x = ., digits = 2)
    )]
    dt
  } %>%
  .[, -colnames.theta, with = FALSE] %>%
  .[, .(age, median, lower, upper)]

# ## 99.4. sensitivity comparison ----
# 
# tmp.XY <-
#   model.data.XY %>%
#   .[, .(MSOA11CD, year, sex, age,  Y, N)] %>%
#   .[, Y_XY := Y] %>%
#   .[, Y := NULL]
#
# tmp.X <-
#   model.data.X %>%
#   .[, .(MSOA11CD, year, sex, age,  Y, N)] %>%
#   .[, Y_X := Y] %>%
#   .[, Y := NULL]
#
# tmp.XY.X <-
#   tmp.XY %>%
#   merge(x = .,
#         y = tmp.X,
#         by = c('MSOA11CD', 'year', 'sex', 'age', 'N'),
#         all.x = TRUE) %>%
#   .[, Y_diff := Y_XY - Y_X]
#
# tmp.1 <-
#   tmp.XY.X %>%
#   .[Y_diff !=  0]
#
# nrow(tmp.1)/nrow(model.data.X) * 100
# tmp.XY.X$Y_diff %>% mean()
# tmp.XY.X$Y_diff %>% var() %>% sqrt()
