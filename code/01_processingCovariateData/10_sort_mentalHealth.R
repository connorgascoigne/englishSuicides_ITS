# 0. set up ----

## 0.1. librarys ----

# data wrangling
library(tidyverse)
library(data.table)
library(fs)
library(haven)
library(animation)

# spatial data
library(sf)

# model fitting 
library(INLA)

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
print(sprintf('Number of cores selected: %s.', n.cores))

dir.code <- file.path(dir.home, 'code')
dir.data <- file.path(dir.home, 'data')
dir.res <- file.path(dir.home, 'results')

### 0.2.2. create ----

dir.data.org.mHealth <- file.path(dir.data, 'organised', '10_mentalHealth')
fs::dir_create(dir.data.org.mHealth)

dir.data.org.final <- file.path(dir.data, 'organised', '99_final')
fs::dir_create(dir.data.org.final)

dir.res.cov.mHealth <- file.path(dir.res, '01_covariateData', '10_mentalHealth')
fs::dir_create(dir.res.cov.mHealth)

## 0.3. functions ----

source(file.path(dir.code, '00_functions.R'))

## 0.4. spatial ----

### 0.4.1. polygon ----

poly.msoa <- 
  file.path(dir.data, 'spatial', 'ons11_MSOA') %>% 
  sf::st_read(dsn = ., layer = 'ons11_MSOA') %>% 
  dplyr::filter(str_detect(MSOA11CD, '^E'))

poly.rgn <- 
  file.path(dir.data, 'spatial', 'ons21_RGN') %>% 
  sf::st_read(dsn = ., layer = 'ons21_RGN') %>% 
  dplyr::filter(str_detect(RGN21CD, '^E'))

poly.ldn <-
  poly.rgn %>% 
  dplyr::filter(RGN21NM == 'London')

### 0.4.2. lookup file ----

lookup.lsoa01_lsoa11 <- 
  file.path(dir.data, 'spatial', 'ons_lookup', 'LSOA01_LSOA11_LAD11_lookUp.csv') %>% 
  data.table::fread(file = .)

lookup.lsoa11_msoa11 <- 
  file.path(dir.data, 'spatial', 'ons_lookup', 'LSOA11_MSOA11_LAD11_lookUp.csv') %>% 
  data.table::fread(file = .) %>% 
  .[, !c('OA11CD', 'ObjectId'), with = FALSE] %>% 
  unique()

### 0.4.3. adjacency matrix ----

mat.msoa <-
  file.path(dir.data, 'organised', '01_spatial', 'amat_msoa.rds') %>% 
  readRDS(file = .)

## 0.5. plot arguments ----

# plot arguments
text.size <- 20
width <- height <- 10

# colour schemes
colours.diverging.full <- c('#00768D', '#7CB5C0', '#E9E0C8', '#FB8171', '#C13E2E')
colours.diverging <- c('#00768D', '#C13E2E')
colours.exceedance <- c('#A8E6CF', '#5CDB95', '#004D3A')
colours.sequential <- c('#E9E0C8', '#4B0082')

# 1. data ----

## 1.1. filepaths ----

fp.mHealth <-
  c(
    # sample deisgn
    file.path(dir.data, 'mentalHealth', 'UKDA-6614-stata', 'stata', 'stata13_se') %>% 
      fs::dir_ls(path = .,
                 recurse = TRUE,
                 type = 'file',
                 regexp = '_hhsamp.dta$'),
    # survey responses 
    file.path(dir.data, 'mentalHealth', 'UKDA-6614-stata', 'stata', 'stata13_se') %>% 
      fs::dir_ls(path = .,
                 recurse = TRUE,
                 type = 'file',
                 regexp = '_indresp.dta$'),
    # lsoa
    file.path(dir.data, 'mentalHealth', 'UKDA-6670-stata', 'stata', 'stata13') %>% 
      fs::dir_ls(path = .,
                 recurse = TRUE,
                 type = 'file',
                 regexp = '_lsoa01_protect.dta$')
    ) %>%  
  data.table::data.table(name.filepath = .) %>% 
  .[, 
    name.file := 
      name.filepath %>% 
      basename() %>% 
      tools::file_path_sans_ext() %>% 
      gsub(pattern = '_protect$', replacement = '', x = .)] %>%
  .[, c('name.wave', 'name.survey') := data.table::tstrsplit(name.file, '_')] %>% 
  split(x = ., 
        by = 'name.wave')
  
## 1.2. save sorted ukhls files ----

future.apply::future_lapply(
  X = fp.mHealth %>% seq_along(),
  FUN = function(i,
                 dir.target = getwd()) {
    
    # 0. function arguments ----
    
    # i <- 1
    # dir.target <- file.path(dir.data, 'organised')
    
    # 1. parameter ----
    
    name.wave <- names(fp.mHealth)[i]
    name.fp.hhs <- fp.mHealth[[i]][name.survey == 'hhsamp', name.filepath]
    name.fp.idr <- fp.mHealth[[i]][name.survey == 'indresp', name.filepath]
    name.fp.spa <- fp.mHealth[[i]][name.survey == 'lsoa01', name.filepath]
    
    # 2. new file ----
    
    name.file.new <- sprintf('ukhls_wave_%s', name.wave)
    name.dir.new <- file.path(dir.target, '10_mentalHealth')
    fs::dir_create(name.dir.new)
    name.fp.new <- file.path(name.dir.new, sprintf('%s.csv', name.file.new))
    
    if(file.exists(name.fp.new)){
      message(sprintf('File for %s exists already. Skipping.', name.file.new))
      return(invisible(NULL))
    }
    
    message(sprintf('Creating file for %s.', name.file.new))
    
    # 3. import data ----
    
    data.import.hhs <- 
      haven::read_dta(name.fp.hhs) %>% 
      data.table::as.data.table()
    data.import.idr <- 
      haven::read_dta(name.fp.idr) %>% 
      data.table::as.data.table()
    data.import.spa <- 
      haven::read_dta(name.fp.spa) %>% 
      data.table::as.data.table()
    
    # 4. data sort ----
    
    # info.hhs <-
    #   data.table(
    #     varname = names(data.import.hhs),
    #     label = sapply(X = data.import.hhs, 
    #                    FUN = function(x) attr(x, 'label'))
    #   )
    # 
    # info.idr <-
    #   data.table(
    #     varname = names(data.import.idr),
    #     label = sapply(X = data.import.hhs, 
    #                    FUN = function(x) attr(x, 'label'))
    #   )
    # 
    # info.spa <-
    #   data.table(
    #     varname = names(data.import.spa),
    #     label = sapply(X = data.import.hhs, 
    #                    FUN = function(x) attr(x, 'label'))
    #   )
    
    data.sort.hhs <-
      data.table::copy(data.import.hhs) %>% 
      # remove wave identifier from colunm names
      data.table::setnames(x = .,
                           old = names(.),
                           new = gsub(pattern = sprintf('^%s_', name.wave),
                                      replacement = '',
                                      x = names(.))) %>% 
      # select columns
      .[, .(
        # linkage
        hidp, # household identifier
        # survey design
        psu, # primary sampling unit
        strata # stratification
      )]
    
    data.sort.idr <-
      data.table::copy(data.import.idr) %>% 
      # remove wave identifier from colunm names
      data.table::setnames(x = .,
                           old = names(.),
                           new = gsub(pattern = sprintf('^%s_', name.wave),
                                      replacement = '',
                                      x = names(.))) %>% 
      # select columns
      .[, .(
        # linkage
        pidp, # person identifier
        hidp, # household identifier
        # interview
        istrtdaty, # interview year
        istrtdatm, # interview month
        istrtdatd, # interview day
        # ghq-12 resp
        scghq1_dv, # ghq-12 Likert
        scghq2_dv # ghq-12 caseness
      )]
    
    data.sort.spa <-
      data.table::copy(data.import.spa) %>% 
      # remove wave identifier from colunm names
      data.table::setnames(x = .,
                           old = names(.),
                           new = gsub(pattern = sprintf('^%s_', name.wave),
                                      replacement = '',
                                      x = names(.)))
    
    rm(
      data.import.hhs,
      data.import.idr,
      data.import.spa
    )
    gc()
    
    # 5. join data ----
    
    ## 5.1. names ----
    
    names.ukhls <-
      c(
        'pidp', 'hidp',
        'psu', 'strata', 'lsoa01',
        'istrtdaty', 'istrtdatm', 'istrtdatd', 
        'scghq1_dv', 'scghq2_dv'
      )
    names.new <-
      c(
        'idx', 'hidx',
        'psu', 'strata', 'LSOA01CD',
        'year', 'month', 'day', 
        'ghq36', 'ghq12'
      )
    
    ## 5.2. final ----
    
    data.final <-
      # start with individual as many in one house
      data.table::copy(data.sort.idr) %>% 
      # join house
      merge(x = .,
            y = data.table::copy(data.sort.hhs),
            by = 'hidp',
            all.x = TRUE) %>%
      # join spatial
      merge(x = .,
            y = data.table::copy(data.sort.spa),
            by = 'hidp',
            all.x = TRUE) %>% 
      data.table::setnames(x = .,
                           old = names.ukhls,
                           new = names.new) %>% 
      .[, 
        (setdiff(names.new, 'LSOA01CD')) := 
          lapply(X = .SD, 
                 FUN = as.numeric),
        .SDcols = setdiff(names.new, 'LSOA01CD')
      ] %>% 
      .[, LSOA01CD := LSOA01CD %>% as.character()] %>% 
      # filtering
      ## 1. -9 = missing; -7 = proxy
      .[psu >=0 & strata >= 0 & month >= 0 & day >= 0 & ghq36 >= 0 & ghq12 >= 0] %>% 
      ## 2. England only LSOAs
      .[grepl(pattern = '^E', x = LSOA01CD)] %>% 
      # LSOA01CD to MSOA11
      ## 1. LSOA01CD - LSOA11CD
      merge(x = .,
            y = lookup.lsoa01_lsoa11 %>% .[, .(LSOA01CD, LSOA11CD)],
            by = 'LSOA01CD',
            all.x = TRUE) %>% 
      ## 2. LSOA11CD - MSOA11CD
      merge(x = .,
            y = lookup.lsoa11_msoa11 %>% .[, .(LSOA11CD, MSOA11CD)],
            by = 'LSOA11CD',
            all.x = TRUE) %>% 
      .[, gsub(pattern = 'LSOA01CD', replacement = 'MSOA11CD', x = names.new), with = FALSE]
    
    rm(
      data.sort.idr,
      data.sort.hhs,
      data.sort.spa
    )
    gc()
    
    # 6. save ----
    
    data.table::fwrite(x = data.final,
                       file = name.fp.new)
    
    # 7. return ----
    
    return(invisible(NULL))
    
  },
  dir.target = file.path(dir.data, 'organised'),
  future.packages = c('tidyverse', 'data.table'),
  future.globals = c('fp.mHealth',
                     'dir.data',
                     'lookup.lsoa01_lsoa11',
                     'lookup.lsoa11_msoa11')
  ) %>% 
  invisible()

## 1.2. open sorted ukhls files ----

data.ukhls <-
  dir.data.org.mHealth %>% 
  fs::dir_ls(path = .,
             recurse = TRUE,
             type = 'file') %>% 
  lapply(X = .,
         FUN = function(x) data.table::fread(file = x)) %>% 
  data.table::rbindlist() %>% 
  .[order(idx, hidx, year, month, day, hidx)]

## 1.3. generic ----

data.generic.temporal <-
  data.table::data.table(year = 1991:2024) %>% 
  .[, time_id := .I]

data.generic.spatial <-
  data.table::data.table(MSOA11CD = poly.msoa$MSOA11CD) %>% 
  .[, space_id := .I]

data.generic.spatiotemporal <-
  data.table::CJ(year = 1991:2024,
                 MSOA11CD = poly.msoa$MSOA11CD) %>% 
  merge(x = .,
        y = data.generic.temporal,
        by = 'year',
        all.x = TRUE) %>% 
  merge(x = .,
        y = data.generic.spatial,
        by = 'MSOA11CD',
        all.x = TRUE) %>% 
  data.table::as.data.table() %>% 
  .[, spacetime_id := (time_id - 1) * max(space_id) + space_id]

## 1.4. model data ----

data.model <- 
  data.ukhls %>% 
  # _id variabels for inla
  ## from generic
  merge(x = .,
        y = data.generic.spatiotemporal,
        by = c('year', 'MSOA11CD'),
        all.x = TRUE) %>% 
  ## defined
  .[, `:=` (
    idx_id = idx %>% as.factor() %>% as.numeric(),
    hidx_id = hidx %>% as.factor() %>% as.numeric(),
    psu_id = psu %>% as.factor() %>% as.numeric(),
    strata_id = strata %>% as.factor() %>% as.numeric()
  )]

# 2. model ----

## 2.0. function ----

extract.samples <- 
  function(newdata, fit, return.time = TRUE, ...){
    
    time.start <- Sys.time()
    # message(sprintf('sorting...'))
    
    # 0. function arguments ----
    
    # newdata <- data.prediction
    # fit <- model.fit
    
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
      # sprintf('~ 1 + %s', .) %>% 
      { if (nzchar(.)) sprintf('~ 1 + %s', .) else '~ 1' } %>% 
      as.formula() %>%
      # define model matrix
      Matrix::sparse.model.matrix(., data = newdata)
    
    X.re <-
      newdata %>% 
      .[, intersect(nms.re, names(.)), with = FALSE]
    
    ## 3.2. individual linear predictors ----
    
    ### 3.2.1. names selected ----
    
    nms.fe.sel <- colnames(X.fe)
    nms.re.sel <- colnames(X.re)
    
    ### 3.2.2. lp from selected names ----
    
    lp.fe <- 
      sweep(x = X.fe[, nms.fe.sel, drop = FALSE],
            MARGIN = 2,
            STATS = coef.fe[parameter %in% nms.fe.sel, value],
            FUN = `*`)
    
    lp.re <-
      lapply(X = nms.re.sel %>% seq_along(),
             FUN = function(i, X.re){
               
               # 0. function arguments ----
               
               # i <- 1
               # X.re <- X.re
               
               # 1. parameters ----
               
               nm.re <- nms.re.sel[i]
               
               # 2. data ----
               
               # data <- fit$summary.random[[nm.re]]$`0.5quant`[X.re[[nm.re]]]
               
               data <- get(nm.re)[X.re[[nm.re]]]
               
               # 3. return ----
               
               return(data)
               
             },
             X.re = X.re) %>% 
      stats::setNames(object = .,
                      nm = nms.re.sel) %>% 
      data.table::as.data.table()
    
    ## 3.3. final linear predictors ----
    
    lp <- 
      (rowSums(x = lp.fe, na.rm = TRUE) + 
         rowSums(x = lp.re, na.rm = TRUE)) %>% 
      as.numeric()
    
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
    
    n  <- length(lp)
    family <- fit$.args$family
    
    set.seed(1234)
    if (family == 'poisson') {
      mu <- exp(lp) 
      fitted <- stats::rpois(n = n, lambda = mu) %>% pmin(., 36)
    }
    
    if (family == 'nbinomial') {
      mu <- exp(lp)
      idx.size <- grep('size', nms.hypr, ignore.case = TRUE)
      # size <- fit$summary.hyperpar[idx.size, '0.5quant']
      size <- theta[idx.size]
      fitted <- stats::rnbinom(n = n, mu = mu, size = size) %>% pmin(., 36)
    }
    
    if (family == 'gaussian') {
      mu <- lp
      idx.prec <- grep('Precision', nms.hypr, ignore.case = TRUE)
      # prec <- fit$summary.hyperpar[idx.prec, '0.5quant']
      prec <- theta[idx.prec]
      sd <- 1 / sqrt(precision)
      fitted <- stats::rnorm(n = n, mean = mu, sd = sd)
    }
    
    if (family == 'betabinomial'){
      n.trials <- 36
      mu <- stats::plogis(lp)
      idx.rho <- grep('overdispersion', nms.hypr, ignore.case = TRUE)
      # rho <- fit$summary.hyperpar[idx.rho, '0.5quant']
      rho <- theta[idx.rho]
      # convert to alpha and beta
      alpha <- mu * (1 - rho) / rho
      beta  <- (1 - mu) * (1 - rho) / rho
      # draw beta random probabilities
      p <- stats::rbeta(n, shape1 = alpha, shape2 = beta)
      # draw beta-binomial outcome
      fitted <- stats::rbinom(n = n, size = n.trials, prob = p)
    }
    
    pred <-
      data.table::data.table(linearpredictor = lp,
                             fitted = fitted)
    
    rm(n,
       family, 
       mu,
       fitted)
    gc()
    
    time.end.pred <- Sys.time()
    
    # 6. messaging ----
    
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
    
    # 7. return ----
    
    return(list(linear.predictor = lp,
                prediction = pred))
    
  }

## 2.1. inla arguments ----

# family <- 'nbinomial'
family <- 'betabinomial'
control.compute <- list(config = TRUE)
control.predictor <- list(compute = TRUE, link = 1)
control.inla <- list(strategy = 'auto', int.strategy = 'auto')
control.mode <- list(restart = TRUE)

## 2.2. priors ----

pc.u <- 1 
pc.alpha <- 0.01
pc.u.phi <- 0.5
pc.alpha.phi <- 0.5

hyper.pc <- 
  list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)))

hyper.pc.bym2 <- 
  list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)), 
       phi = list(prior = 'pc', param = c(pc.u.phi, pc.alpha.phi)))

## 2.3. values ----

n.time <- length(1991:2024)
n.space <- nrow(poly.msoa)
n.spacetime <- length(1991:2024)*nrow(poly.msoa)

## 2.4. formula ----

formula <-
  ghq36 ~ 
  # global intercept
  1 +
  # spatio-temporal
  f(time_id, model = 'rw2', hyper = hyper.pc, constr = TRUE, scale.model = TRUE, values = 1:n.time) +
  f(space_id, model = 'bym2', hyper = hyper.pc.bym2, constr = TRUE, scale.model = TRUE, graph = mat.msoa) +
  f(spacetime_id, model = 'iid', hyper = hyper.pc, values = 1:n.spacetime) +
  # individual
  f(idx_id, model = 'iid', hyper = hyper.pc) +
  # survey design
  f(psu_id, model = 'iid', hyper = hyper.pc) +
  f(strata_id, model = 'iid', hyper = hyper.pc)

## 2.5. model fit ----

fp.modelfit <- file.path(dir.res.cov.mHealth, 'model_fit.rds')

if(file.exists(fp.modelfit)) {
  
  message(sprintf('Load model fit:'))
  t0 <- Sys.time()
  model.fit <- readRDS(file = fp.modelfit)
  t1 <- Sys.time()
  message(sprintf(' time to load: %s.',
                  time.format(time.end = t1, time.start = t0)))
  
} else {
  
  message(sprintf('Run model fit:'))
  
  t0 <- Sys.time()
  model.fit <-
    INLA::inla(formula = formula,
               data = data.model,
               family = family, 
               Ntrials = rep(x = 36, times = nrow(data.model)),
               control.compute = control.compute,
               control.predictor = control.predictor,
               control.inla = control.inla,
               control.mode = control.mode,
               num.threads = n.cores)
  t1 <- Sys.time()
  message(sprintf(' time to fit: %s.',
                  time.format(time.end = t1, time.start = t0)))
  
  t0 <- Sys.time()
  saveRDS(object = model.fit,
          file = fp.modelfit)
  t1 <- Sys.time()
  message(sprintf(' time to save: %s.',
                  time.format(time.end = t1, time.start = t0)))
  
}

## 2.6. model posterior ----

fp.modelposterior <- file.path(dir.res.cov.mHealth, 'model_posterior.rds')

if(file.exists(fp.modelposterior)) {
  
  message(sprintf('Load model posterior:'))
  
  t0 <- Sys.time()
  model.posterior <- readRDS(file = fp.modelposterior)
  model.posterior.small <- readRDS(file = fp.modelposterior %>% gsub(pattern = '.rds$', replacement = '_5.rds', x = .))
  t1 <- Sys.time()
  message(sprintf(' time to load: %s.',
                  time.format(time.end = t1, time.start = t0)))
  
} else {
  
  message(sprintf('Sample model posterior:'))
  
  t0 <- Sys.time()
  model.posterior <-
    INLA::inla.posterior.sample(n = 1000,
                                result = model.fit,
                                num.threads = n.cores)
  t1 <- Sys.time()
  message(sprintf(' time to sample: %s.',
                  time.format(time.end = t1, time.start = t0)))
  
  t0 <- Sys.time()
  saveRDS(object = model.posterior,
          file = fp.modelposterior)
  t1 <- Sys.time()
  message(sprintf(' time to save: %s.',
                  time.format(time.end = t1, time.start = t0)))
  
  message(sprintf('Model posterior n = 5:'))
  
  t0 <- Sys.time()
  model.posterior.small <-
    INLA::inla.posterior.sample(n = 5,
                                result = model.fit,
                                num.threads = n.cores)
  t1 <- Sys.time()
  message(sprintf(' time to sample: %s.',
                  time.format(time.end = t1, time.start = t0)))
  
  t0 <- Sys.time()
  saveRDS(object = model.posterior.small,
          file = fp.modelposterior %>% gsub(pattern = '.rds$', replacement = '_5.rds', x = .))
  t1 <- Sys.time()
  message(sprintf(' time to save: %s.',
                  time.format(time.end = t1, time.start = t0)))
  
}

## 2.7. model predictions ----

### 2.7.1. prediction data ----

# make copies for safety
data.model.copy <- data.table::copy(data.model)
data.generic.spatiotemporal.copy <- data.table::copy(data.generic.spatiotemporal)

# set key to join
data.table::setkey(data.model.copy, year, MSOA11CD, time_id, space_id, spacetime_id)
data.table::setkey(data.generic.spatiotemporal.copy, year, MSOA11CD, time_id, space_id, spacetime_id)

# join
data.prediction <-
  data.model.copy %>%
  .[data.generic.spatiotemporal.copy]

# clear
rm(data.model.copy,
   data.generic.spatiotemporal.copy)
gc()

### 2.7.2. run or load ----

fp.modelpredictions <- file.path(dir.res.cov.mHealth, 'model_predictions.rds')

if(file.exists(fp.modelpredictions)) {

  message(sprintf('Load model predictions:'))

  t0 <- Sys.time()
  model.predictions <- readRDS(file = fp.modelpredictions)
  model.predictions.small <- readRDS(file = fp.modelpredictions %>% gsub(pattern = '.rds$', replacement = '_5.rds', x = .))
  t1 <- Sys.time()
  message(sprintf(' time to load: %s.',
                  time.format(time.end = t1, time.start = t0)))

} else {

  message(sprintf('Define model predictions n = 1000:'))

  t0 <- Sys.time()
  model.predictions <-
    INLA::inla.posterior.sample.eval(fun = extract.samples,
                                     newdata = data.prediction,
                                     fit = model.fit,
                                     return.time = FALSE,
                                     # inla.posterior.sample arguments
                                     samples = model.posterior,
                                     return.matrix = FALSE) %>%
    stats::setNames(object = .,
                    nm = sprintf('theta:%s', 1:1000))
  t1 <- Sys.time()
  message(sprintf(' time to define: %s.',
                  time.format(time.end = t1, time.start = t0)))

  t0 <- Sys.time()
  saveRDS(object = model.predictions,
          file = fp.modelpredictions)
  t1 <- Sys.time()
  message(sprintf(' time to save: %s.',
                  time.format(time.end = t1, time.start = t0)))

  message(sprintf('Model predictions n = 5:'))

  t0 <- Sys.time()
  model.predictions.small <-
    INLA::inla.posterior.sample.eval(fun = extract.samples,
                                     newdata = data.prediction,
                                     fit = model.fit,
                                     return.time = TRUE,
                                     # inla.posterior.sample arguments
                                     samples = model.posterior.small,
                                     return.matrix = FALSE) %>%
    stats::setNames(object = .,
                    nm = sprintf('theta:%s', 1:5))
  t1 <- Sys.time()
  message(sprintf(' time to define: %s.',
                  time.format(time.end = t1, time.start = t0)))

  t0 <- Sys.time()
  saveRDS(object = model.predictions.small,
          file = fp.modelpredictions %>% gsub(pattern = '.rds$', replacement = '_5.rds', x = .))
  t1 <- Sys.time()
  message(sprintf(' time to save: %s.',
                  time.format(time.end = t1, time.start = t0)))

}

## 3.0. function ----

format.prediction <-
  function(posterior) {

    # 0. function arguments ----

    # posterior <- model.predictions.tmp

    # 1. extract results ----

    res <-
      lapply(X = posterior %>% seq_along(),
             FUN = function(i) {

               # 0. function arguments ----

               # i <- 1

               # 1. parameters ----

               name.theta <- names(posterior)[i]
               sample <- posterior[[i]]

               # 2. results ----

               res <-
                 data.table::copy(sample$prediction) %>%
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

## 3.1. format data ----

colnames.theta <- sprintf('theta:%s', 1:1000)

## 3.2. formatted results ----

fp.resultstheta <- file.path(dir.data.org.final, 'MSOA11_MENTALHEALTH.csv')

if(file.exists(fp.resultstheta)) {

  message(sprintf('Results summary:'))

  t0 <- Sys.time()
  results.summary <- data.table::fread(file = fp.resultstheta)
  t1 <- Sys.time()
  message(sprintf(' time to load: %s.',
                  time.format(time.end = t1, time.start = t0)))

} else {

  message(sprintf('Results summary:'))

  t0 <- Sys.time()
  results.theta <-
    model.predictions %>%
    format.prediction(posterior = .) %>%
    # join to data
    merge(x = data.table::copy(data.prediction) %>% .[, id := .I],
          y = .,
          by = 'id',
          all.x = TRUE,
          sort = FALSE) %>%
    data.table::as.data.table() %>%
    .[, `:=` (
      id = NULL
    )]

  results.summary <-
    data.table::copy(results.theta) %>%
    # average over year - MSOA11
    .[,
      lapply(X = .SD,
             FUN = function(x) mean(x, na.rm = TRUE)),
      by = .(year, MSOA11CD),
      .SDcols = colnames.theta
    ] %>%
    # calculate posterior summaries
    {
      dt <- .
      m  <- as.matrix(dt[, ..colnames.theta])
      
      dt[, `:=`(
        mh_mean   = Matrix::rowMeans(m),
        mh_var    = matrixStats::rowVars(m),
        mentalHealth = matrixStats::rowMedians(m),
        mh_lower  = matrixStats::rowQuantiles(m, probs = 0.025),
        mh_upper  = matrixStats::rowQuantiles(m, probs = 0.975)
      )]
      
      dt
    } %>% 
    .[, -colnames.theta, with = FALSE]
  t1 <- Sys.time()
  message(sprintf(' time to define: %s.',
                  time.format(time.end = t1, time.start = t0)))

  t0 <- Sys.time()
  data.table::fwrite(x = results.summary,
                     file = fp.resultstheta)
  t1 <- Sys.time()
  message(sprintf(' time to save: %s.',
                  time.format(time.end = t1, time.start = t0)))

}

# 4. plot ----

## 4.1. prepare data ----

data.plot <-
  file.path(dir.data.org.final, 'MSOA11_MENTALHEALTH.csv') %>% 
  data.table::fread() %>%  
  .[year %in% 2002:2017] %>% 
  merge(x = .,
        y = lookup.lsoa11_msoa11 %>% .[, .(MSOA11CD, LAD11CD)] %>% unique(),
        by = 'MSOA11CD',
        all.x = TRUE) %>%
  .[, insideLondon :=
      LAD11CD %>%
      grepl(pattern = '^E09', x = .) %>%
      as.integer()]

## 4.2. prepare plot layer ----

fill.ggLayer <-
  list(
    ggplot2::scale_fill_gradient(name = 'Mental Wellbeing (Measured by GHQ-12 Score)',
                                 low = colours.sequential[1], high = colours.sequential[2],
                                 limits = c(0, 36), breaks = c(0, 18, 36),
                                 guide = guide_colourbar(title.position = 'top'))
  )

## 4.3. make animation ----

make.animationPlot(
  data = data.plot,
  var.frame = 'year',
  var.fill = 'mentalHealth',
  var.zoom = 'insideLondon',
  polygon = poly.msoa,
  polygon.cd = 'MSOA11CD',
  polygon.border = poly.rgn,
  polygon.border.zoom = poly.ldn,
  fill.ggLayer = fill.ggLayer,
  target.filename = 'MSOA11_MENTALHEALTH',
  target.dir = dir.res.cov.mHealth
)

## 4.4. average plot ----

data.average <-
  file.path(dir.data.org.final, 'MSOA11_MENTALHEALTH.csv') %>% 
  data.table::fread() %>%  
  .[year %in% 2002:2017] %>% 
  .[, .(
    mentalHealth = mentalHealth %>% mean()
  ),
  by = MSOA11CD] %>% 
  merge(x = .,
        y = lookup.lsoa11_msoa11 %>% .[, .(MSOA11CD, LAD11CD)] %>% unique(),
        by = 'MSOA11CD',
        all.x = TRUE) %>% 
  .[, insideLondon := 
      LAD11CD %>% 
      grepl(pattern = '^E09', x = .) %>% 
      as.integer()]

plot.average <-
  make.singlePlot(
    data = data.average,
    var.fill = 'mentalHealth',
    var.zoom = 'insideLondon',
    polygon = poly.msoa,
    polygon.cd = 'MSOA11CD',
    polygon.border = poly.rgn,
    polygon.border.zoom = poly.ldn,
    fill.ggLayer = fill.ggLayer,
    text.size = 20
  )

ggplot2::ggsave(
  filename = file.path(dir.res.cov.mHealth, 'MSOA11_MENTALHEALTH_AVERAGE.png'),
  plot = plot.average,
  width = width,
  height = height
)
