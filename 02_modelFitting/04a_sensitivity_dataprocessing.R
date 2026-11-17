dataprocess.buildSuicides(
  year.min = 2002,
  year.max = 2022, # UP TO 2022
  year.intervention = 2012,
  filter.intentional = TRUE, # EXCLUDE Y CODES HERE
  save.counterfactual = TRUE,
  output.dir = file.path(dir.res.main.data, 'xcodes'),
  output.name = 'MSOA11_suicidedeaths_sensitivity_xcodes',
  info.msoa = info.msoa,
  lookup.msoa.rgn = lookup.msoa.rgn,
  lookup.lsoa.msoa = lookup.lsoa.msoa,
  dir.data.org = file.path(dir.data, 'organised'),
  dir.data = dir.data
)

dataprocess.buildSuicides(
  year.min = 2002,
  year.max = 2017, # REMOVE AFTER 2017 HERE
  year.intervention = 2012,
  filter.intentional = FALSE, # INCLUDE BOTH X AND Y CODES
  save.counterfactual = TRUE,
  output.dir = file.path(dir.res.main.data, 'pre2017'),
  output.name = 'MSOA11_suicidedeaths_sensitivity_pre2017',
  info.msoa = info.msoa,
  lookup.msoa.rgn = lookup.msoa.rgn,
  lookup.lsoa.msoa = lookup.lsoa.msoa,
  dir.data.org = file.path(dir.data, 'organised'),
  dir.data = dir.data
)