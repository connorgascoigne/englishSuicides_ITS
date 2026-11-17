# 1. parameters ----

colours.diverging <- c('#008B00', '#E9E0C8', '#CD0000')

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

# 2. correlation ----

## 2.1. data ----

model.data <- 
  file.path(dir.res.main.data, 'MSOA11_suicidedeaths_main.rds') %>%
  readRDS(file = .)

## 2.2. static correlation plot ----

colnames.select <- c(
  'Y', 
  'agesex_id', 
  'deprivation_id', 'ethnicDensity_id', 'populationDensity_id', 'lightPollution_id', 
  'railwayDensity_id', 'roadwayDensity_id', 'greenspace_id', 'mentalHealth_id'
  )

colnames.nice <- c(
  'Y', 
  'Age-sex', 
  'Deprivation', 'Ethnic Density', 'Population Density', 'Light Pollution', 
  'Railway Density', 'Roadway Density', 'Greenspace', 'Mental Wellbeing'
)

get.lower.triangle <- function(matrix){
  matrix[lower.tri(matrix)] <- NA
  return(matrix)
}

corrplot.limits <- c(-1, 1)
corrplot.breaks <- c(-1, 0, 1)

correlation.data <- 
  model.data %>% 
  .[, ..colnames.select] %>% 
  .[, agesex_id := agesex_id %>% as.numeric()] %>% 
  cor(., method = 'pearson') %>% 
  get.lower.triangle() %>% 
  reshape2::melt(., varnames = c('row', 'column'), value.name = 'correlation', na.rm = TRUE) %>% 
  data.table::as.data.table() %>% 
  .[, `:=` (
    row = row %>% factor(., levels = colnames.select, labels = colnames.nice),
    column = column %>% factor(., levels = colnames.select, labels = colnames.nice)
  )]

plot.correlation <-
  ggplot2::ggplot(
    data = correlation.data, 
    aes(x = row, y = column, fill = correlation, label = sprintf(paste0('%.', 2, 'f'), round(correlation, digits = 2)) )
  ) +
  ggplot2::geom_tile(
    colour = 'white'
  ) +
  ggplot2::scale_fill_gradient2(
    name = 'Pearsons Correlation',
    # guide = ggplot2::guide_colorbar(title.pisition = 'top'),
    midpoint = 0,
    low = colours.diverging[3], 
    mid = colours.diverging[2],
    high = colours.diverging[1],
    limits = corrplot.limits,
    breaks = corrplot.breaks
  ) +
  ggplot2::geom_text(
    colour = 'black', 
    size = plot.text/.pt
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_x_discrete(
    limits = correlation.data$row %>% levels() %>% rev(),
    labels = correlation.data$row %>% levels() %>% rev()) +
  ggplot2::scale_y_discrete(labels = correlation.data$row %>% levels()
  ) +
  ggplot2::labs(
    y = '', 
    x = ''
  ) +
  ggplot2::theme(
    text = ggplot2::element_text(size = plot.text),
    legend.position = 'bottom',
    legend.key.width = unit(2, 'cm'),
    legend.title = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
    axis.ticks = element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(hjust = 0),
    legend.key = ggplot2::element_rect(fill = NA, colour = NA)
  )

ggplot2::ggsave(
  filename = file.path(dir.res.main.add, 'correlation.png'),
  plot = plot.correlation,
  width = plot.width,
  height = plot.height
)

# 3. exceedance ----

## 3.1. posterior load ----

posterior <- 
  helper.loadOrRun(
    filepath = file.path(dir.res.main.mfit, 'model_posterior_factual_main.rds'), 
    FUN =  stop(),
    message.run = 'Stop: Need to run from model fitting code.',
    message.load = 'Loading results summary'
  )

colnames.theta <- names(posterior)

posterior.fixed <-
  posterior %>% 
  format.fixedeffects(posterior = .)

posterior.svc <- 
  posterior %>% 
  format.svceffects(posterior = .)

## 3.2. posterior summarise ----

smry.fixed <-
  posterior.fixed %>% 
  # filter its terms
  .[parameter %in% c('intervention_idpost', 'period_id'), ] %>% 
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
  .[, -colnames.theta, with = FALSE] %>% 
  .[, value := median] %>% 
  .[, parameter := parameter %>% factor(x = ., levels = rownames.its, labels = rownames.its.formal)] %>% 
  .[, .(parameter, value)]

posterior.svc.sustained <-
  posterior.svc %>% 
  .[parameter %in% c('intervention_idpost', 'period_id')] %>% 
  melt(
    data = .,
    id.vars = c('parameter', 'lad_id'),
    mesaure.vars = data.table::patterns('^theta:'),
    variable.name = 'theta',
    value.name = 'value'
  ) %>% 
  .[, value := value %>% exp()] %>% 
  merge(
    x = .,
    y = info.lad %>% data.table::as.data.table() %>% .[, .(LAD11NM, Internal)] %>% .[, Internal := Internal %>% gsub(pattern = '^lad_', replacement = '', x = .) %>% as.numeric()],
    all.x = TRUE,
    by.x = 'lad_id',
    by.y = 'Internal'
  )

## 3.3. prepare plot data ----

select.lad <- c('Wakefield', 'Liverpool', 'Plymouth')
posterior.svc.sustaned.select <-
  posterior.svc.sustained %>% 
  .[LAD11NM %in% select.lad] %>% 
  .[, LAD11NM := LAD11NM %>% factor(x = ., levels = select.lad)] %>% 
  .[, parameter := parameter %>% factor(x = ., levels = rownames.its, labels = rownames.its.formal)] %>% 
  .[, {
    d <- stats::density(x = value)
    .(x = d$x, density = d$y)
  },
  by = .(parameter, LAD11NM)] %>% 
  merge(
    x = .,
    y = smry.fixed %>% .[, .(parameter, median = value)],
    all.x = TRUE,
    by = 'parameter'
  ) %>% 
  .[, shadedArea := data.table::fifelse(test = x >= median, yes = density, no = NA_real_)]

label.dt <-
  posterior.svc.sustaned.select %>% 
  .[, .(ylabel = max(density) * 0.5), 
    by = .(parameter, LAD11NM)
  ] %>% 
  .[, xlabel := max(posterior.svc.sustaned.select$x, na.rm = TRUE)]

## 3.4. plot ----

plot.exceedance <-
  ggplot2::ggplot(
    data = posterior.svc.sustaned.select,
    mapping = ggplot2::aes(x = x)
  ) +
  ggplot2::geom_area(
    mapping = ggplot2::aes(y = shadedArea),
    fill = 'steelblue',
    alpha = 0.4
  ) +
  ggplot2::geom_text(
    data = label.dt,
    mapping = ggplot2::aes(x = xlabel, y = ylabel),
    label = 'P(LA-specific effect >\nNational median effect)',
    colour = 'steelblue',
    hjust = 1,
    inherit.aes = FALSE,
    size = plot.text/.pt
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(y = density),
    colour = 'black'
  ) +
  ggplot2::geom_vline(
    data = smry.fixed,
    mapping = ggplot2::aes(xintercept = value),
    colour = 'red3',
    linetype = 'dashed'
  ) +
  ggplot2::geom_text(
    data = smry.fixed,
    mapping = ggplot2::aes(x = value, y = Inf, label = sprintf('National median = %s', round(x = value, digits = 3))),
    colour = 'red3',
    hjust = -0.05,
    vjust = 1.5,
    size = plot.text/.pt
  ) +
  ggplot2::facet_grid(
    parameter ~ LAD11NM,
    scales = 'free_y'
  ) +
  ggplot2::labs(
    x = 'Value',
    y = 'Posterior'
  ) +
  helper.plotTheme(
    text = ggplot2::element_text(size = 3*plot.text),
    legend.position = 'none',
    legend.background = ggplot2::element_blank(),
    legend.box.background = ggplot2::element_blank()
  ); plot.exceedance

ggplot2::ggsave(
  filename = file.path(dir.res.main.add, 'exceedance.png'),
  plot = plot.exceedance,
  width = 3*plot.width,
  height = 3*plot.height
)


