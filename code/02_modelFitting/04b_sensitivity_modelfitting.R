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

## 4.1. run loop ----

for(saType in c('xcodes', 'pre2017')){
  
  # 0. function arguments ----
  
  # saType <- 2003
  
  # 1. parameters ----
  
  sprintf('Running for sensitivty analysis %s.', saType) %>% message()
  
  dir.data.target <- file.path(dir.res.main.data, saType)
  
  dir.mfit.target <- file.path(dir.res.main.mfit, saType)
  fs::dir_create(dir.mfit.target)
  
  # 2. load data ----
  
  model.data <- 
    file.path(dir.data.target, sprintf('MSOA11_suicidedeaths_sensitivity_%s.rds', saType)) %>% 
    readRDS(file = .)
  
  model.data.cfact <- 
    file.path(dir.data.target, sprintf('MSOA11_suicidedeaths_sensitivity_%s_counterfactual.rds', saType)) %>%
    readRDS(file = .)
  
  n.years <- model.data$year %>% unique() %>% sort() %>% length() # do this here as it needs the data
  
  # 3. model fit ----
  
  model.fit <- 
    model.fitAndSample(
      formula = formula,
      data = model.data,
      filepath.model = file.path(dir.mfit.target, sprintf('model_fit_sensitivity_%s.rds', saType)),
      filepath.samples = file.path(dir.mfit.target, sprintf('model_samples_sensitivity_%s.rds', saType)),
      n.samples = 1000,
      filepath.samples.cheap = file.path(dir.mfit.target, sprintf('model_samples_sensitivity_%s_5.rds', saType)),
      n.samples.cheap = 5
    )
  
  # 4. compute factual posterior ----
  
  model.posterior.fact <-
    model.computePosterior(
      model.fit = model.fit$model.fit,
      model.samples = model.fit$model.samples,
      data = model.data,
      filepath.posterior = file.path(dir.mfit.target, sprintf('model_posterior_factual_sensitivity_%s.rds', saType)),
      FUN.EVAL = extract.samples,
      return.cheap = FALSE
    )
  
  model.posterior.fact.cheap <-
    model.computePosterior(
      model.fit = model.fit$model.fit,
      model.samples = model.fit$model.samples.cheap,
      data = model.data,
      filepath.posterior = file.path(dir.mfit.target, sprintf('model_posterior_factual_sensitivity_%s_5.rds', saType)),
      FUN.EVAL = extract.samples,
      return.cheap = FALSE
    )
  
  # 5. compute counterfactual posterior ----
  
  model.posterior.cfact <-
    model.computePosterior(
      model.fit = model.fit$model.fit,
      model.samples = model.fit$model.samples,
      data = model.data.cfact,
      filepath.posterior = file.path(dir.mfit.target, sprintf('model_posterior_counterfactual_sensitivity_%s.rds', saType)),
      FUN.EVAL = extract.samples,
      return.cheap = FALSE
    )
  
  model.posterior.cfact.cheap <-
    model.computePosterior(
      model.fit = model.fit$model.fit,
      model.samples = model.fit$model.samples.cheap,
      data = model.data.cfact,
      filepath.posterior = file.path(dir.mfit.target, sprintf('model_posterior_counterfactual_sensitivity_%s_5.rds', saType)),
      FUN.EVAL = extract.samples,
      return.cheap = FALSE
    )
  
  # 6. format results ----
  
  model.posteriorsummary.fact <- 
    helper.loadOrRun(
      filepath = file.path(dir.mfit.target, sprintf('model_posteriorsummary_factual_sensitivity_%s.rds', saType)), 
      FUN =  
        function() {
          
          format.allresults(
            data = model.data,
            posterior = model.posterior.fact
          )
          
        },
      message.run = 'Running results summary',
      message.load = 'Loading results summary'
    )
  
  model.posteriorsummary.cfact <- 
    helper.loadOrRun(
      filepath = file.path(dir.mfit.target, sprintf('model_posteriorsummary_counterfactual_sensitivity_%s.rds', saType)), 
      FUN =  
        function() {
          
          format.allresults(
            data = model.data,
            posterior = model.posterior.cfact
          )
          
        },
      message.run = 'Running results counterfactual summary',
      message.load = 'Loading results counterfactual summary'
    )
  
  # 7. clear ----
  
  rm(
    dir.data.target,
    dir.mfit.target,
    model.data,
    n.years,
    model.fit,
    model.posterior.fact,
    model.posterior.fact.cheap,
    model.posterior.cfact,
    model.posterior.cfact.cheap,
    model.posteriorsummary.fact,
    model.posteriorsummary.cfact
  )
  gc()
  
}
