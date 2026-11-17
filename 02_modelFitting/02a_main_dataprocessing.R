dataprocess.buildSuicides(
  year.min = 2002,
  year.max = 2022, # UP TO 2022
  year.intervention = 2012, 
  filter.intentional = FALSE, # INCLUDE BOTH X AND Y CODES
  save.counterfactual = TRUE,
  output.dir = dir.res.main.data,
  output.name = 'MSOA11_suicidedeaths_main',
  info.msoa = info.msoa,
  lookup.msoa.rgn = lookup.msoa.rgn,
  lookup.lsoa.msoa = lookup.lsoa.msoa,
  dir.data.org = file.path(dir.data, 'organised'),
  dir.data = dir.data
)

