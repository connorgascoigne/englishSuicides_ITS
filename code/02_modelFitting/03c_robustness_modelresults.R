# 1. parameters ----

plot.text <- 20
plot.width <- plot.height <- 10

colours.scenario <- c('Primary analysis' = '#104E88', 'Robustness analysis' = '#FF2A00')
fill.scenario <- c('Primary analysis' = '#87CEEB', 'Robustness analysis' = '#FFB3A8')

plot.text <- 20
plot.width <- plot.height <- 10

rownames.its <- c('intervention_idpost', 'period_id')
rownames.its.formal <- c('Immediate', 'Sustained')

# 2. extract results ----

res <- 
  file.path(dir.res, '01_main', '03_results', 'its_ctry.csv') %>% 
  data.table::fread()

if(fs::file_exists(file.path(dir.res, '02_robustness', '03_results', 'its_ctry_robustness.csv'))){
  
  res.robustness <- 
    data.table::fread(
      file = file.path(dir.res, '02_robustness', '03_results', 'its_ctry_robustness.csv')
    )
  
} else {
  
  tmp.yr <- c(2003:2011, 2013:2021)
  
  res.robustness <- 
    lapply(
      X = tmp.yr,
      FUN = function(intyr) {
        
        # 0. function arguments ----
        
        # intyr <- 2003
        
        # 1. parameters ----
        
        sprintf('Formating ITS results for year %s:', intyr) %>% message()
        
        # 1. import model fit ----
        
        sel.mfit <- 
          helper.loadOrRun(
            filepath = file.path(dir.res.main.mfit, intyr, sprintf('model_fit_robustness_%s.rds', intyr)), 
            FUN = NULL, 
            message.run = ' need to run', 
            message.load = ' importing...'
          )
        
        # 2. extract parameters ----
        
        res <-
          sel.mfit$summary.fixed %>% 
          data.table::as.data.table(x = , keep.rownames = 'parameter') %>% 
          .[parameter %in% c('year_id', 'intervention_idpost', 'period_id')] %>% 
          .[, `:=` (
            year = intyr,
            median = `0.5quant` %>% exp(),
            lower = `0.025quant` %>% exp(),
            upper = `0.975quant` %>% exp()
          )] %>% 
          .[, .(year, parameter, median, lower, upper)]
        
        # 3. clear -----
        
        rm(
          intyr,
          sel.mfit
        )
        gc()
        
        # 4. return ----
        
        return(res)
        
      }) %>% 
    data.table::rbindlist()
  
  
  data.table::fwrite(
    x = res.robustness,
    file = file.path(dir.res, '02_robustness', '03_results', 'its_ctry_robustness.csv')
  )
}

# 3. immediate results ----

data.itsCTRY <-
  list(
    res %>% 
      .[parameter == 'Immediate'] %>% 
      .[, `:=` (
        year = 2012,
        scenario = 'primary'
      )] %>% 
      .[, .(year, parameter, median, lower, upper, scenario)],
    res.robustness %>% 
      .[parameter == 'intervention_idpost'] %>% 
      .[, `:=` (
        upper = upper %>% as.numeric(),
        scenario = 'robustness',
        parameter = parameter %>% factor(x = ., levels = 'intervention_idpost', labels = 'Immediate')
      )]
  ) %>% 
  data.table::rbindlist(
    l = ., 
    use.name = TRUE
  ) %>% 
  .[, `:=` (
    scenario = scenario %>% 
      factor(x = ., 
             levels = c('primary', 'robustness'), 
             labels = c('Primary analysis', 'Robustness analysis')
      )
  )]

plot.text <- 20
plot.range <- c(min(data.itsCTRY$lower), max(data.itsCTRY$upper))
plot.limit <- c(floor(10*plot.range[1])/10, ceiling(10*plot.range[2])/10)
plot.breaks <- seq(from = plot.limit[1], to = plot.limit[2], length.out = 5)
plot.jitter <- ggplot2::position_dodge(width = 0.5)
plot.colour <- colours.scenario

plot <-
  ggplot2::ggplot(data = data.itsCTRY, 
                  mapping = ggplot2::aes(x = year, y = median, group = scenario, colour = scenario)) +
  # 1. null
  ggplot2::geom_hline(mapping = aes(yintercept = 1), 
                      colour = 'black', 
                      linetype = 'dashed') +
  # 2. forest plot
  ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = lower, ymax = upper),
                         width = 0,
                         position = plot.jitter) +
  ggplot2::geom_point(position = plot.jitter) +
  # 3. colour
  ggplot2::scale_colour_manual(name = '',
                               values = plot.colour) +
  # 4. sort axis
  ggplot2::scale_x_continuous(name = '',
                              breaks = seq(from = 2003, to = 2021, by = 1)) +
  ggplot2::scale_y_continuous(name = 'Relative risk', 
                              limits = plot.limit,
                              breaks = plot.breaks) +
  ggplot2::coord_flip() +
  # 5. theme
  ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1, 
                                                 label.position = 'left',
                                                 label.hjust = 1)) +
  helper.plotTheme(text = ggplot2::element_text(size = plot.text),
                   legend.title = ggplot2::element_blank(),
                   legend.position = 'inside',
                   legend.position.inside = c(0.98, 0.02),
                   legend.justification = c(1, 0),
                   legend.text.align = 1,
                   legend.box.just = 'right',
                   legend.background = ggplot2::element_blank()); plot

ggplot2::ggsave(filename = file.path(dir.res.main.res, 'its_ctry_robustness_shortTerm.png'),
                plot = plot,
                width = plot.width,
                height = plot.height)

data.table::fwrite(
  x = data.itsCTRY,
  file = file.path(dir.res.main.res, 'its_ctry_robustness_shortTerm.csv')
)

# 4. sustained results ----

data.itsCTRY.all <-
  list(
    res %>% 
      .[parameter %in% rownames.its.formal] %>% 
      .[, `:=` (
        year = 2012,
        scenario = 'primary'
      )] %>% 
      .[, .(year, parameter, median, lower, upper, scenario)],
    res.robustness %>% 
      .[parameter %in% rownames.its] %>% 
      .[, `:=` (
        upper = upper %>% as.numeric(),
        scenario = 'robustness',
        parameter = parameter %>% factor(x = ., levels = rownames.its, labels = rownames.its.formal)
      )]
  ) %>% 
  data.table::rbindlist(
    l = ., 
    use.name = TRUE
  ) %>% 
  .[, `:=` (
    scenario = scenario %>% 
      factor(x = ., 
             levels = c('primary', 'robustness'), 
             labels = c('Primary analysis', 'Robustness analysis')
      )
  )]
  
data.itsCTRY <- 
  data.itsCTRY.all %>% 
  .[!(year == 2003 & parameter == 'Sustained')]

plot.text <- 20
plot.range <- c(min(data.itsCTRY$lower), max(data.itsCTRY$upper))
plot.limit <- c(floor(10*plot.range[1])/10, ceiling(10*plot.range[2])/10)
plot.breaks <- seq(from = plot.limit[1], to = plot.limit[2], length.out = 5)
plot.jitter <- ggplot2::position_dodge(width = 0.5)
plot.colour <- colours.scenario

plot <-
  ggplot2::ggplot(data = data.itsCTRY, 
                  mapping = ggplot2::aes(x = year, y = median, group = scenario, colour = scenario)) +
  # 1. null
  ggplot2::geom_hline(mapping = aes(yintercept = 1), 
                      colour = 'black', 
                      linetype = 'dashed') +
  # 2. forest plot
  ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = lower, ymax = upper),
                         width = 0,
                         position = plot.jitter) +
  ggplot2::geom_point(position = plot.jitter) +
  # 3. colour
  ggplot2::scale_colour_manual(name = '',
                               values = plot.colour) +
  # 4. sort axis
  ggplot2::scale_x_continuous(name = '',
                              breaks = seq(from = 2003, to = 2021, by = 1)) +
  ggplot2::scale_y_continuous(name = 'Relative risk', 
                              limits = plot.limit,
                              breaks = plot.breaks) +
  # ggplot2::facet_grid(~parameter, scales = 'free_y') +
  ggh4x::facet_grid2(~ parameter,
                     scales = 'free_x',
                     independent = 'x') +
  ggplot2::coord_flip() +
  # 5. theme
  ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1, 
                                                 label.position = 'left',
                                                 label.hjust = 1)) +
  helper.plotTheme(text = ggplot2::element_text(size = plot.text),
                   legend.title = ggplot2::element_blank(),
                   legend.position = 'inside',
                   legend.position.inside = c(0.98, 0.02),
                   legend.justification = c(1, 0),
                   legend.text.align = 1,
                   legend.box.just = 'right',
                   legend.background = ggplot2::element_blank()); plot

ggplot2::ggsave(filename = file.path(dir.res.main.res, 'its_ctry_robustness_both.png'),
                plot = plot,
                width = plot.width,
                height = plot.height)

data.table::fwrite(
  x = data.itsCTRY.all,
  file = file.path(dir.res.main.res, 'its_ctry_robustness_both.csv')
)

