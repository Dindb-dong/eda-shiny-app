ink <- "#14282b"
teal <- "#197266"
orange <- "#a74125"
quiet <- "#48605e"
year_colors <- c("2021" = teal, "2022" = orange)
period_colors <- c("Before onset" = teal, "Since onset" = orange)

chart_theme <- theme_minimal(base_size = 13) +
  theme(text = element_text(colour = ink), axis.text = element_text(colour = quiet),
        panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "#e4ede9"),
        strip.text = element_text(face = "bold", size = 11, colour = ink),
        panel.spacing = grid::unit(1.35, "lines"), legend.position = "bottom",
        plot.margin = margin(8, 12, 8, 8))

panel_grid <- function(columns = 2L, scales = "fixed") {
  facet_wrap(vars(panel), ncol = columns, scales = scales)
}

calendar_axis <- function() scale_x_date(date_breaks = "2 years", date_labels = "%Y")

relationship_plot <- function(paired, spending, covid, view, metric, measure, omit_feb, columns) {
  y_label <- spending_measures[[measure]]
  x_label <- covid_measures[[metric]]
  if (metric != "stringency") x_label <- paste0(x_label, " (asinh of count / 100)")
  paired$year <- format(paired$date, "%Y")
  grid <- panel_grid(columns)

  if (view == "lags") {
    points <- do.call(rbind, lapply(0:3, function(lead) {
      d <- pair_months(spending, covid, metric, lead, omit_feb)
      by_series(d, function(g) data.frame(panel = g$panel[1], lead = lead, n = nrow(g),
                                          rho = safe_correlation(g$x, g$value)))
    }))
    return(ggplot(points, aes(lead, rho)) + geom_hline(yintercept = 0, colour = quiet) +
      geom_line(colour = teal, linewidth = .8) + geom_point(colour = teal, size = 2.5) +
      geom_text(aes(label = paste0("n=", n)), nudge_y = .13, size = 3, colour = quiet) +
      scale_x_continuous(breaks = 0:3) + coord_cartesian(ylim = c(-1.1, 1.2)) +
      labs(x = "Months by which COVID-19 leads spending", y = "Spearman correlation") +
      grid + chart_theme)
  }

  if (view == "aligned") {
    standard <- by_series(paired, function(g) {
      g$segment <- cumsum(c(TRUE, diff(g$month_id) != 1))
      rbind(transform(g, series = "Spending", z = as.numeric(scale(value))),
            transform(g, series = "COVID-19", z = as.numeric(scale(x))))
    })
    return(ggplot(standard, aes(date, z, colour = series, group = interaction(series, segment))) +
      geom_hline(yintercept = 0, colour = quiet) + geom_line(linewidth = .8) + geom_point(size = 1.3) +
      scale_colour_manual(values = c("COVID-19" = orange, "Spending" = teal)) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b %y") +
      labs(x = "Spending month", y = "Within-series z-score", colour = NULL) + grid + chart_theme)
  }

  if (view == "adjusted") {
    paired <- by_series(paired, adjust_association)
    paired$x <- paired$x_adjusted
    paired$value <- paired$y_adjusted
    x_label <- paste("Residual:", x_label)
    y_label <- paste("Residual:", y_label)
  }

  if (view == "binary") {
    curves <- by_series(paired, binary_curve)
    paired$decline <- as.integer(paired$yoy < 0)
    labels <- unique(paired["panel"])
    labels <- subset(labels, !panel %in% curves$panel)
    p <- ggplot(paired, aes(stringency, decline)) +
      geom_point(colour = orange, alpha = .72, size = 2) +
      geom_line(data = curves, aes(x, probability), colour = teal, linewidth = .85) +
      scale_y_continuous(breaks = c(0, .25, .5, .75, 1), labels = scales::label_percent()) +
      labs(x = "Restriction stringency (0–100)", y = "Observed outcome / fitted probability") + grid + chart_theme
    if (nrow(labels)) p <- p + geom_text(data = labels, aes(x = Inf, y = .5, label = "Sparse or separated outcomes"),
                                       inherit.aes = FALSE, hjust = 1.06, size = 3, colour = quiet)
    return(p)
  }

  if (view %in% c("conditioning", "outcomes")) {
    paired <- subset(paired, is.finite(stringency))
    paired$band <- factor(ifelse(paired$stringency >= 50, "50–100", "Below 50"),
                          levels = c("Below 50", "50–100"))
    if (view == "outcomes") {
      labels <- c("Decline", "Near-flat", "Growth")
      paired$outcome <- factor(ifelse(paired$yoy < -5, labels[1],
                                     ifelse(paired$yoy > 5, labels[3], labels[2])), levels = labels)
      counts <- as.data.frame(table(panel = paired$panel, band = paired$band, outcome = paired$outcome))
      counts <- subset(counts, Freq > 0)
      return(ggplot(counts, aes(band, Freq, fill = outcome)) +
        geom_col(position = "fill", width = .65) +
        geom_text(aes(label = Freq), position = position_fill(vjust = .5), colour = "white", size = 3.2) +
        scale_fill_manual(values = c("Decline" = orange, "Near-flat" = quiet, "Growth" = teal)) +
        scale_y_continuous(labels = scales::label_percent()) +
        labs(x = "Restriction stringency", y = "Share of months", fill = NULL) + grid + chart_theme)
    }
    return(ggplot(paired, aes(x, value, shape = band, colour = band)) +
      geom_point(size = 2.8, alpha = .88) +
      scale_colour_manual(values = c("Below 50" = teal, "50–100" = orange)) +
      labs(x = x_label, y = y_label, colour = "Stringency band", shape = "Stringency band") +
      grid + chart_theme)
  }

  p <- ggplot(paired, aes(x, value)) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = quiet, linetype = 2, linewidth = .65)
  if (view == "scatter") {
    p <- p + geom_smooth(method = "loess", formula = y ~ x, span = 1, method.args = list(degree = 1),
                         se = FALSE, colour = teal, linewidth = .8)
  }
  p + geom_point(aes(colour = year, shape = year), size = 2.8, alpha = .9) +
    scale_colour_manual(values = year_colors) +
    labs(x = x_label, y = y_label, colour = "Spending year", shape = "Spending year") +
    grid + chart_theme
}

spending_plot <- function(d, full, view, measure, columns) {
  y_label <- spending_measures[[measure]]
  grid <- panel_grid(columns)
  d$period <- ifelse(d$date < onset, "Before onset", "Since onset")
  d <- subset(d, is.finite(value))
  if (view == "distribution") {
    return(ggplot(d, aes(value, colour = period, linetype = period)) +
      stat_ecdf(linewidth = .9, pad = FALSE) + geom_rug(alpha = .25) +
      scale_colour_manual(values = period_colors) +
      labs(x = y_label, y = "Cumulative share of months", colour = NULL, linetype = NULL) +
      grid + chart_theme)
  }
  if (view == "box") {
    d$period <- factor(d$period, levels = names(period_colors))
    return(ggplot(d, aes(period, value, fill = period)) +
      geom_boxplot(width = .5, alpha = .65, outlier.alpha = .55) +
      scale_fill_manual(values = period_colors, guide = "none") +
      labs(x = NULL, y = y_label) + grid + chart_theme)
  }
  if (view == "heatmap") {
    limit <- max(abs(d$value - if (measure == "index") 100 else 0))
    center <- if (measure == "index") 100 else 0
    return(ggplot(d, aes(season, factor(format(date, "%Y")), fill = value)) + geom_tile() +
      scale_x_continuous(breaks = seq(1, 12, 2), labels = month.abb[seq(1, 12, 2)]) +
      scale_fill_gradient2(low = orange, mid = "#f4f7f5", high = teal,
                           midpoint = center, limits = center + c(-limit, limit)) +
      labs(x = NULL, y = NULL, fill = y_label) + grid + chart_theme)
  }
  if (view == "projection") {
    projected <- by_series(full, historical_projection)
    d <- merge(d, projected[c("panel", "date", "projected")], by = c("panel", "date"))
    return(ggplot(d, aes(date, index)) +
      geom_vline(xintercept = onset, linetype = 3, colour = quiet) +
      geom_line(aes(colour = "Observed"), linewidth = .8) +
      geom_line(aes(y = projected, colour = "Pre-COVID trend and seasonality"), linewidth = .8, linetype = 2) +
      scale_colour_manual(values = c("Observed" = teal, "Pre-COVID trend and seasonality" = orange)) +
      calendar_axis() + labs(x = NULL, y = spending_measures[["index"]], colour = NULL) +
      grid + chart_theme)
  }
  reference <- if (measure == "index") 100 else 0
  ggplot(d, aes(date, value)) + geom_hline(yintercept = reference, colour = "#becfcb") +
    geom_vline(xintercept = onset, linetype = 3, colour = quiet) + geom_line(colour = teal, linewidth = .8) +
    calendar_axis() + labs(x = NULL, y = y_label) + grid + chart_theme
}

diagnostic_plot <- function(d, paired, view, measure, columns) {
  grid <- panel_grid(columns)
  if (view == "stl") {
    return(ggplot(by_series(d, seasonal_decomposition), aes(date, value)) +
      geom_vline(xintercept = onset, linetype = 3, colour = quiet) +
      geom_line(colour = teal, linewidth = .7) + calendar_axis() +
      facet_wrap(vars(component, panel), ncol = columns, scales = "free_y") +
      labs(x = NULL, y = "Log spending") + chart_theme)
  }
  if (view %in% c("acf", "pacf")) {
    ac <- by_series(d, function(g) {
      values <- g$value[is.finite(g$value)]
      a <- if (view == "pacf") pacf(values, plot = FALSE, lag.max = 24)
           else acf(values, plot = FALSE, lag.max = 24)
      keep <- as.numeric(a$lag) > 0
      data.frame(panel = g$panel[1], lag = as.numeric(a$lag)[keep], value = as.numeric(a$acf)[keep])
    })
    return(ggplot(ac, aes(lag, value)) + geom_hline(yintercept = 0, colour = quiet) +
      geom_col(fill = teal, width = .62) + coord_cartesian(ylim = c(-1, 1)) +
      labs(x = "Lag (months)", y = if (view == "acf") "Autocorrelation" else "Partial autocorrelation") + grid + chart_theme)
  }
  if (view == "qq") {
    d$period <- ifelse(d$date < onset, "Before onset", "Since onset")
    return(ggplot(subset(d, is.finite(value)), aes(sample = value, colour = period)) +
      stat_qq(size = 1.6, alpha = .7) + stat_qq_line(linewidth = .6) +
      scale_colour_manual(values = period_colors) +
      labs(x = "Theoretical normal quantile", y = spending_measures[[measure]], colour = NULL) +
      grid + chart_theme)
  }
  if (view %in% c("quantiles", "quantile_diff")) {
    points <- by_series(d, quantile_comparison)
    if (view == "quantiles") {
      return(ggplot(points, aes(before, since)) + geom_abline(linetype = 2, colour = quiet) +
        geom_point(colour = teal, size = 2) +
        labs(x = "Before-onset quantile", y = "Since-onset quantile") + grid + chart_theme)
    }
    return(ggplot(points, aes(average, difference)) + geom_hline(yintercept = 0, colour = quiet) +
      geom_point(colour = teal, size = 2) +
      labs(x = "Average of matching quantiles", y = "Since minus before") + grid + chart_theme)
  }
  if (view == "spread") {
    d$year <- format(d$date, "%Y")
    points <- by_series(d, location_spread)
    return(ggplot(points, aes(median, iqr)) + geom_point(colour = teal, size = 2) +
      geom_text(aes(label = year), vjust = -.7, size = 3, check_overlap = TRUE) +
      scale_y_continuous(expand = expansion(mult = c(.05, .15))) +
      labs(x = "Annual median", y = "Annual interquartile range") + grid + chart_theme)
  }
  if (view == "boxcox") {
    profile <- by_series(d, boxcox_profile)
    best <- subset(profile, likelihood == 0)
    return(ggplot(profile, aes(lambda, likelihood)) + geom_line(colour = teal, linewidth = .8) +
      geom_vline(data = best, aes(xintercept = lambda), colour = quiet, linetype = 2) +
      labs(x = "Box–Cox power (zero = log)", y = "Relative profile log likelihood") +
      grid + chart_theme)
  }
  fitted <- by_series(paired, adjust_association)
  if (view == "fitted_resid") {
    return(ggplot(fitted, aes(fitted, residual)) + geom_hline(yintercept = 0, colour = quiet) +
      geom_point(colour = teal, size = 2.5) + labs(x = "Adjusted model fitted value", y = "Residual") +
      grid + chart_theme)
  }
  if (view == "residuals") {
    return(ggplot(fitted, aes(date, residual)) + geom_hline(yintercept = 0, colour = quiet) +
      geom_point(colour = teal, size = 2.5) + scale_x_date(date_breaks = "6 months", date_labels = "%b %y") +
      labs(x = "Spending month", y = "Adjusted model residual") + grid + chart_theme)
  }
  ggplot(fitted, aes(date, cook)) + geom_col(fill = orange, width = 20) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %y") +
    labs(x = "Spending month", y = "Cook’s distance") + grid + chart_theme
}

forecast_plot <- function(predictions, view, columns) {
  actual <- unique(predictions[c("panel", "date", "actual")])
  inks <- c("Observed" = ink, "Seasonal naïve" = "#7254a4", "Holt–Winters" = teal, "SARIMA" = orange)
  ggplot(predictions, aes(date, prediction, colour = model, linetype = model)) +
    geom_line(linewidth = .77) +
    geom_line(data = actual, aes(date, actual, colour = "Observed", linetype = "Observed"),
              inherit.aes = FALSE, linewidth = .95) +
    scale_colour_manual(values = inks, breaks = names(inks)) +
    scale_linetype_manual(values = c("Observed" = 1, "Seasonal naïve" = 3, "Holt–Winters" = 2, "SARIMA" = 4),
                          breaks = names(inks)) +
    scale_x_date(date_breaks = if (view == "backtest") "3 months" else "6 months", date_labels = "%b %y") +
    labs(x = NULL, y = spending_measures[["index"]], colour = NULL, linetype = NULL) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) + panel_grid(columns) + chart_theme
}

break_plot <- function(fitted, columns) {
  ggplot(fitted, aes(date, index)) +
    geom_vline(xintercept = onset, linetype = 3, colour = quiet) +
    geom_line(aes(colour = "Observed"), alpha = .55, linewidth = .65) +
    geom_line(aes(y = segmented, colour = "Fitted with break"), linewidth = .8) +
    geom_line(data = subset(fitted, date >= onset), aes(y = continued, colour = "Pre-break slope continued"),
              linetype = 2, linewidth = .8) +
    scale_colour_manual(values = c("Observed" = quiet, "Fitted with break" = teal, "Pre-break slope continued" = orange)) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
    calendar_axis() + labs(x = NULL, y = spending_measures[["index"]], colour = NULL) +
    panel_grid(columns) + chart_theme
}

age_plot <- function(d, view) {
  if (view == "age_residual") {
    return(ggplot(d, aes(date, age, fill = residual)) + geom_tile() +
      scale_fill_gradient2(low = orange, mid = "#f4f7f5", high = teal, midpoint = 0,
                           name = "Log difference (×100)") +
      scale_y_continuous(breaks = seq(22.5, 77.5, 10)) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(x = NULL, y = "Age-band midpoint") + chart_theme)
  }
  if (view == "age_compare") {
    values <- rbind(transform(d, series = "Ordinary LOESS", mean_ticket = ordinary),
                    transform(d, series = "Robust LOESS", mean_ticket = robust))
  } else {
    values <- transform(d, series = "Observed", mean_ticket = observed)
  }
  ggplot(values, aes(date, age, fill = mean_ticket)) + geom_tile() +
    scale_fill_viridis_c(option = "cividis", name = "TWD per transaction", labels = scales::label_comma()) +
    scale_y_continuous(breaks = seq(22.5, 77.5, 10)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    facet_wrap(vars(series), ncol = 1) +
    labs(x = NULL, y = "Age-band midpoint") + chart_theme
}
