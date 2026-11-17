for (intyr in c(2003:2011, 2013:2021)) {
  
  dataprocess.buildSuicides(
    year.min = 2002,
    year.max = 2022, # UP TO 2022
    year.intervention = intyr,
    filter.intentional = FALSE, # INCLUDE BOTH X AND Y CODES
    save.counterfactual = FALSE, # DO NOT NEED FOR ROBUSTNESS
    output.dir = file.path(dir.res.main.data, intyr),
    output.name = sprintf('MSOA11_suicidedeaths_robustness_%s', intyr),
    info.msoa = info.msoa,
    lookup.msoa.rgn = lookup.msoa.rgn,
    lookup.lsoa.msoa = lookup.lsoa.msoa,
    dir.data.org = file.path(dir.data, 'organised'),
    dir.data = dir.data
  )
  
}