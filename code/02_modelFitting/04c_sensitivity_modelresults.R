# 1. parameters ----

colours.diverging <- c('#008B00', '#E9E0C8', '#CD0000')
colours.exceedance <- c('#A0D6D0', '#009B92', '#00768D')

colours.scenario <- c('Primary analysis' = '#104E88', 'Sensitivity analysis: X codes only' = '#FF2A00', 'Sensitivity analysis: 2002-2017' = '#2E8B57')
fill.scenario <- c('Primary analysis' = '#87CEEB', 'Sensitivity analysis: X codes only' = '#FFB3A8', 'Sensitivity analysis: 2002-2017' = '#9FD5B3')

scale.label <- format(100000, big.mark = ',', scientific = FALSE)
plot.text <- 20
plot.width <- plot.height <- 10

rownames.its <- c('intervention_idpost', 'period_id')
rownames.its.formal <- c('Immediate', 'Sustained')
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

res <- 
  helper.loadOrRun(
    filepath = file.path(dir.res, '01_main', '02_modelfit', 'model_posteriorsummary_factual_main.rds'),
    FUN =  stop(),
    message.run = 'Stop: Need to run main model factual summary.',
    message.load = 'Loading main model factual summary...'
  )

res.cfact <- 
  helper.loadOrRun(
    filepath = file.path(dir.res, '01_main', '02_modelfit', 'model_posteriorsummary_counterfactual_main.rds'),
    FUN =  stop(),
    message.run = 'Stop: Need to run main model counterfactual summary.',
    message.load = 'Loading main model counterfactual summary...'
  )

res.xcodes <- 
  helper.loadOrRun(
    filepath = file.path(dir.res, '03_sensitivity', '02_modelfit', 'xcodes', 'model_posteriorsummary_factual_sensitivity_xcodes.rds'), 
    FUN =  stop(),
    message.run = 'Stop: Need to run xcodes SA model factual summary.',
    message.load = 'Loading xcodes SA model factual summary...'
  )

res.xcodes.cfact <- 
  helper.loadOrRun(
    filepath = file.path(dir.res, '03_sensitivity', '02_modelfit', 'xcodes', 'model_posteriorsummary_counterfactual_sensitivity_xcodes.rds'), 
    FUN =  stop(),
    message.run = 'Stop: Need to run xcodes SA model counterfactual summary.',
    message.load = 'Loading xcodes SA model counterfactual summary...'
  )

res.pre2017 <- 
  helper.loadOrRun(
    filepath = file.path(dir.res, '03_sensitivity', '02_modelfit', 'pre2017', 'model_posteriorsummary_factual_sensitivity_pre2017.rds'), 
    FUN =  stop(),
    message.run = 'Stop: Need to run pre2017 SA model factual summary.',
    message.load = 'Loading pre2017 SA model factual summary...'
  )

res.pre2017.cfact <- 
  helper.loadOrRun(
    filepath = file.path(dir.res, '03_sensitivity', '02_modelfit', 'pre2017', 'model_posteriorsummary_counterfactual_sensitivity_pre2017.rds'), 
    FUN =  stop(),
    message.run = 'Stop: Need to run pre2017 SA model counterfactual summary.',
    message.load = 'Loading pre2017 SA model counterfactual summary...'
  )

# 3. year x national plot ----

data.yrCTRY <-
  list(
    res$summary.yearCTRY %>% data.table::copy() %>% .[, scenario := 'primary'],
    res.xcodes$summary.yearCTRY %>% data.table::copy() %>% .[, scenario := 'sensitivity1'],
    res.pre2017$summary.yearCTRY %>% data.table::copy() %>% .[, scenario := 'sensitivity2']
  ) %>% 
  data.table::rbindlist() %>% 
  .[, 
    scenario := 
      scenario %>% 
      factor(x = ., 
             levels = c('primary', 'sensitivity1', 'sensitivity2'), 
             labels = c('Primary analysis', 'Sensitivity analysis: X codes only', 'Sensitivity analysis: 2002-2017')
      )
  ]

plot.yrCTRY <-
  makePlot.yrCTRY(
    data = data.yrCTRY,
    filename = 'year_ctry_sensitivity',
    dir.target = dir.res.main.res,
    plot.colour = colours.scenario,
    plot.fill = fill.scenario,
    plot.xbreaks = seq(from = 2002, to = 2022, by = 2),
    plot.xlimits = c(2002, 2022),
    plot.scaleLabel = scale.label
  )

rm(
  data.yrCTRY,
  plot.yrCTRY
)
gc()

# 4. year x region plot ----

data.yrRGN <-
  list(
    res$summary.yearRGN %>% data.table::copy() %>% .[, scenario := 'primary'],
    res.xcodes$summary.yearRGN %>% data.table::copy() %>% .[, scenario := 'sensitivity1'],
    res.pre2017$summary.yearRGN %>% data.table::copy() %>% .[, scenario := 'sensitivity2']
  ) %>% 
  data.table::rbindlist() %>% 
  .[, 
    scenario := 
      scenario %>% 
      factor(x = ., 
             levels = c('primary', 'sensitivity1', 'sensitivity2'), 
             labels = c('Primary analysis', 'Sensitivity analysis: X codes only', 'Sensitivity analysis: 2002-2017')
      )
  ]

plot.yrRGN <-
  makePlot.yrRGN(
    data = data.yrRGN,
    filename = 'year_rgn_sensitivity',
    dir.target = dir.res.main.res,
    plot.colour = colours.scenario,
    plot.fill = fill.scenario,
    plot.xbreaks = seq(from = 2002, to = 2022, by = 10),
    plot.xlimits = c(2002, 2022),
    plot.scaleLabel = scale.label
  )

rm(
  data.yrRGN,
  plot.yrRGN
)
gc()

# 5. its x national plot ----

data.itsCTRY <-
  list(
    res$summary.itsCTRY %>% data.table::copy() %>% .[, scenario := 'primary'],
    res.xcodes$summary.itsCTRY %>% data.table::copy() %>% .[, scenario := 'sensitivity1'],
    res.pre2017$summary.itsCTRY %>% data.table::copy() %>% .[, scenario := 'sensitivity2']
  ) %>% 
  data.table::rbindlist() %>% 
  .[, `:=` (
    scenario = scenario %>% 
      factor(x = ., 
             levels = c('primary', 'sensitivity1', 'sensitivity2'), 
             labels = c('Primary analysis', 'Sensitivity analysis: X codes only', 'Sensitivity analysis: 2002-2017')
      ),
    parameter = parameter %>% factor(x = ., levels = rownames.its, labels = rownames.its.formal)
  )] %>% 
  .[!is.na(parameter)]

plot.itsCTRY <- 
  makePlot.itsCTRY(
    data = data.itsCTRY, 
    plot.colour = colours.scenario,
    filename = 'its_ctry_sensitivity',
    dir.target = dir.res.main.res
  )

rm(
  data.itsCTRY,
  plot.itsCTRY
)
gc()

# 6. fixed effects ----

data.fixedEffects <-
  list(
    list(
      res$summary.fixedEffects %>% data.table::copy(),
      data.table::data.table(parameter = 'agesex_id15_24-female', mean = 1, var = 1, median = 1, lower = 1, upper = 1)
    ) %>%
      data.table::rbindlist() %>% 
      .[, scenario := 'primary'],
    list(
      res.xcodes$summary.fixedEffects %>% data.table::copy(),
      data.table::data.table(parameter = 'agesex_id15_24-female', mean = 1, var = 1, median = 1, lower = 1, upper = 1)
    ) %>%
      data.table::rbindlist() %>% 
      .[, scenario := 'sensitivity1'],
    list(
      res.pre2017$summary.fixedEffects %>% data.table::copy(),
      data.table::data.table(parameter = 'agesex_id15_24-female', mean = 1, var = 1, median = 1, lower = 1, upper = 1)
    ) %>%
      data.table::rbindlist() %>% 
      .[, scenario := 'sensitivity2']
  ) %>% 
  data.table::rbindlist() %>% 
  .[!(parameter %in% c('(Intercept)', rownames.its))] %>% 
  .[, `:=` (
    scenario = scenario %>% 
      factor(x = ., 
             levels = c('primary', 'sensitivity1', 'sensitivity2'), 
             labels = c('Primary analysis', 'Sensitivity analysis: X codes only', 'Sensitivity analysis: 2002-2017')
      ),
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
    plot.colour = colours.scenario,
    filename = 'fixed_effects_agesex_sensitivity',
    dir.target = dir.res.main.res
  )

plot.fixedEffects.socioenv <-
  makePlot.socioEnvEffects(
    data = data.fixedEffects.socioenv,
    plot.colour = colours.scenario,
    filename = 'fixed_effects_socioenv_sensitivity',
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
