# 1. data ----

load(file.path(dir.data, 'organised', 'ENGLAND_AMAT_LSOA_MSOA_LAD_RGN.rda'))
rm(mat.lsoa) # remove unneeded variables (space)

# 2. parameters ----

n.space <- mat.msoa %>% nrow()
n.sims <- 1000
n.sims.cheap <- 5

# 3. bayesian set up ----

## 3.1. priors ----

pc.u <- 1
pc.alpha <- 0.01
pu.u.phi <- 0.5
pc.alpha.phi <- 2/3
hyper.pc <- list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)))
hyper.pc.bym2 <- list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)),
                      phi = list(prior = 'pc', param = c(pu.u.phi, pc.alpha.phi)))

## 3.2. formula ----

formula <-
  Y ~ 
  # fixed effects
  ## global intercept
  1 + 
  ## global ITS components
  year_id + intervention_id + period_id +
  ## covariaties
  agesex_id + deprivation_id + ethnicDensity_id + populationDensity_id + lightPollution_id + railwayDensity_id + roadwayDensity_id + greenspace_id + mentalHealth_id +
  # random effects
  ## temporal iid
  f(year2_id, model = 'rw1', hyper = hyper.pc, constr = TRUE, scale.model = TRUE) + 
  ## spatial structure
  f(msoa_id, model = 'bym2', hyper = hyper.pc.bym2, constr = TRUE, scale.model = TRUE, graph = mat.msoa) +
  ## spatio-temporal interaction iid
  f(msoa_year_id, model = 'iid', hyper = hyper.pc) +
  ## ITS varying coefficients
  ### by lad
  f(lad_id, year_id, model = 'bym2', hyper = hyper.pc.bym2, constr = TRUE, scale.model = TRUE, graph = mat.lad) +
  f(lad2_id, intervention_id, model = 'bym2', hyper = hyper.pc.bym2, constr = TRUE, scale.model = TRUE, graph = mat.lad) +
  f(lad3_id, period_id, model = 'bym2', hyper = hyper.pc.bym2, constr = TRUE, scale.model = TRUE, graph = mat.lad) +
  # offset
  offset(log(N))

# 4. model fitting ----

## 4.0. function ----

model.fitOnly <- function(
    formula,
    data,
    family = c('poisson', 'zeroinflatedpoisson0', 'zeroinflatedpoisson1')[3],
    filepath.model,
    ...
) {
  
  # 0. function arguments ----
  
  # formula = formula
  # data = model.data.XY
  # filepath.model = file.path(dir.res.modelfit, 'model_fit.rds')
  
  # 1. model fit ----
  
  model.fit <- 
    helper.loadOrRun(
      filepath = filepath.model,
      FUN = function(...) {
        
        # model fit 
        fit <-
          INLA::inla(
          formula = formula,
          data = data,
          family = family,
          # control.compute = list(config = TRUE), # remove to be cheaper
          control.predictor = list(link = 1),
          control.inla = list(strategy = 'auto', int.strategy = 'auto'),
          control.mode = list(restart = TRUE),
          verbose = TRUE,
          num.threads = future::availableCores(),
          ...)
        
        # remove data to be cheaper to store
        fit$.args$data <- NULL
        fit$model.matrix <- NULL
        
        # return cheaper fit
        return(fit)
        
        
      },
      message.run = 'Fitting model...',
      message.load = 'Loading model...'
    )
  
  # 4. return ----
  
  return(model.fit)
  
}

## 4.1. run loop ----

tmp.yr <- c(2003:2011, 2013:2021)
# tmp.yr <- c(2018:2021)

for(intyr in tmp.yr){
  
  # 0. function arguments ----
  
  # intyr <- 2003
  
  # 1. parameters ----
  
  sprintf('Running for intervention at year %s.', intyr) %>% message()
  
  dir.data.target <- file.path(dir.res.main.data, intyr)
  
  dir.mfit.target <- file.path(dir.res.main.mfit, intyr)
  fs::dir_create(dir.mfit.target)
  
  # 2. load data ----
  
  model.data <- 
    file.path(dir.data.target, sprintf('MSOA11_suicidedeaths_robustness_%s.rds', intyr)) %>% 
    readRDS(file = .)
  
  n.years <- model.data$year %>% unique() %>% sort() %>% length() # do this here as it needs the data
  
  # 3. model fit ----
  
  model.fit <-
    model.fitOnly(
      formula = formula,
      data = model.data,
      filepath.model = file.path(dir.mfit.target, sprintf('model_fit_robustness_%s.rds', intyr))
    )
  
  # 4. clear ----
  
  rm(
    dir.data.target,
    dir.mfit.target,
    model.data,
    model.fit
  )
  gc()
  
}



