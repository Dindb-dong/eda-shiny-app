# Nationwide card comparison adapted from the attachment to PR #5.
# Keep these inputs separate from the joint analysis's food/apparel extracts.
card_category_labels <- c(
  FD = "Food (FD)", CT = "Clothing (CT)", LG = "Housing and lodging (LG)",
  TR = "Transport (TR)", EE = "Education and entertainment (EE)",
  DP = "Department stores (DP)", OT = "Other (OT)", EC = "E-commerce (EC)"
)
card_country_colors <- c(Korea = "#2464bd", Taiwan = "#b95013")
# Category-share views (from the PR #13 attachment): stacking order, bottom to top
card_share_order <- c("EC", "FD", "CT", "LG", "TR", "EE", "DP", "OT")
card_channel_labels <- c(EC = "E-commerce", OFF = "Offline (7 categories)")
card_share_palette <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                        "#e87ba4", "#008300", "#6250d6", "#e34948")
card_month_id <- function(date) as.integer(format(date, "%Y")) * 12L + as.integer(format(date, "%m"))

read_card_comparison <- function(path = "data/card_comparison") {
  kr <- read.csv(file.path(path, "korea_en_on_off.csv"), stringsAsFactors = FALSE)
  tw <- read.csv(file.path(path, "taiwan_all_en_on_off.csv"), stringsAsFactors = FALSE)
  cp <- read.csv(file.path(path, "korea_taiwan_cpi_monthly.csv"),
                 fileEncoding = "UTF-8-BOM", check.names = FALSE, stringsAsFactors = FALSE)
  stopifnot(all(c("date", "category", "total_amount_krw") %in% names(kr)),
            all(c("date", "category", "offline_amount_ntd", "online_amount_ntd") %in% names(tw)),
            ncol(cp) >= 3L)
  kr$date <- as.Date(kr$date)
  tw$date <- as.Date(tw$date)
  cp_date <- as.Date(paste0(cp[[1]], "-01"))
  cpi <- rbind(data.frame(date = cp_date, country = "Korea", cpi = as.numeric(cp[[3]])),
               data.frame(date = cp_date, country = "Taiwan", cpi = as.numeric(cp[[2]])))
  cpi <- cpi[!is.na(cpi$date), ]
  stopifnot(!anyDuplicated(kr[c("date", "category")]),
            !anyDuplicated(tw[c("date", "category")]),
            !anyDuplicated(cpi[c("date", "country")]),
            all(kr$category %in% names(card_category_labels)),
            all(tw$category %in% setdiff(names(card_category_labels), "EC")))
  valid_cpi <- cpi[is.finite(cpi$cpi) & cpi$cpi > 0, ]
  coverage <- list(kr$date, tw$date,
                   valid_cpi$date[valid_cpi$country == "Korea"],
                   valid_cpi$date[valid_cpi$country == "Taiwan"])
  stopifnot(all(lengths(coverage) > 0L))
  start <- max(as.Date("2014-01-01"), do.call(c, lapply(coverage, min)))
  end <- min(do.call(c, lapply(coverage, max)))
  stopifnot(start <= as.Date("2014-01-01"), end >= as.Date("2014-12-01"))
  dates <- seq(start, end, by = "month")
  kr_card <- data.frame(date = kr$date, country = "Korea", category = kr$category,
                        origin = kr$category, amount_local = as.numeric(kr$total_amount_krw))
  tw_off <- data.frame(date = tw$date, country = "Taiwan", category = tw$category,
                       origin = tw$category, amount_local = as.numeric(tw$offline_amount_ntd))
  tw_on <- data.frame(date = tw$date, country = "Taiwan", category = "EC",
                      origin = tw$category, amount_local = as.numeric(tw$online_amount_ntd))
  raw <- rbind(kr_card, tw_off, tw_on)

  # A complete calendar preserves missing observations instead of shifting lags
  # or silently treating an absent category as zero in a total.
  kr_map <- data.frame(country = "Korea", category = names(card_category_labels),
                        origin = names(card_category_labels))
  tw_categories <- setdiff(names(card_category_labels), "EC")
  tw_map <- rbind(data.frame(country = "Taiwan", category = tw_categories, origin = tw_categories),
                  data.frame(country = "Taiwan", category = "EC", origin = tw_categories))
  grid <- merge(data.frame(date = dates), rbind(kr_map, tw_map), by = NULL)
  d <- merge(grid, raw, by = c("date", "country", "category", "origin"), all.x = TRUE, sort = FALSE)
  d <- merge(d, cpi, by = c("date", "country"), all.x = TRUE, sort = FALSE)
  d$cpi[!is.finite(d$cpi) | d$cpi <= 0] <- NA_real_
  d <- d[order(d$country, d$category, d$origin, d$date), ]
  rownames(d) <- NULL
  list(data = d, start = start, end = end)
}

card_aggregate <- function(data, scope = "total", categories = c("FD", "TR", "EC"),
                            drop_other = FALSE, countries = c("Korea", "Taiwan"),
                            offline_only = FALSE) {
  d <- data[data$country %in% countries, ]
  if (drop_other) d <- d[d$origin != "OT", ]
  offline_total <- scope == "total" && offline_only
  if (offline_total) d <- d[d$category != "EC", ]
  if (scope == "total") {
    d$category <- "total"
  } else {
    d <- d[d$category %in% intersect(names(card_category_labels), categories), ]
  }
  if (!nrow(d)) return(data.frame())
  groups <- split(d, interaction(d$country, d$category, d$date, drop = TRUE))
  out <- do.call(rbind, lapply(groups, function(g) {
    data.frame(date = g$date[1], country = g$country[1], category = g$category[1],
               amount_local = sum(g$amount_local), cpi = g$cpi[1])
  }))
  out$panel <- ifelse(out$category != "total", card_category_labels[out$category],
                      if (offline_total) "Offline spending" else "Total spending")
  out <- out[order(out$country, out$category, out$date), ]
  rownames(out) <- NULL
  out
}

card_transform <- function(data, unit = "real_index", series = "raw") {
  if (!nrow(data)) return(data)
  stopifnot(unit %in% c("local", "index", "real_index"), series %in% c("raw", "ma12", "yoy"))
  groups <- split(data, interaction(data$country, data$category, drop = TRUE))
  out <- do.call(rbind, lapply(groups, function(g) {
    g <- g[order(g$date), ]
    real <- g$amount_local / g$cpi * 100
    base_rows <- format(g$date, "%Y") == "2014"
    base <- if (unit == "real_index") real[base_rows] else g$amount_local[base_rows]
    baseline <- if (length(base) == 12L && all(is.finite(base))) mean(base) else NA_real_
    g$unit_value <- switch(unit,
      local = g$amount_local / if (g$country[1] == "Korea") 1e12 else 1e9,
      index = if (is.finite(baseline) && baseline > 0) g$amount_local / baseline * 100 else NA_real_,
      real_index = if (is.finite(baseline) && baseline > 0) real / baseline * 100 else NA_real_)
    months <- card_month_id(g$date)
    g$value <- switch(series,
      raw = g$unit_value,
      yoy = {
        previous <- g$unit_value[match(months - 12L, months)]
        ifelse(is.finite(previous) & previous > 0, (g$unit_value / previous - 1) * 100, NA_real_)
      },
      ma12 = vapply(months, function(month) {
        values <- g$unit_value[match(seq.int(month - 11L, month), months)]
        if (all(is.finite(values))) mean(values) else NA_real_
      }, numeric(1)))
    g
  }))
  out <- out[order(out$country, out$category, out$date), ]
  rownames(out) <- NULL
  out
}

# Robust LOESS trend per country and panel; one-off spikes carry little weight.
card_loess <- function(d, span = 0.5) {
  groups <- split(d, interaction(d$country, d$panel, drop = TRUE))
  out <- do.call(rbind, lapply(groups, function(g) {
    x <- as.numeric(g$date)
    fit <- tryCatch(suppressWarnings(stats::loess(value ~ x, data.frame(value = g$value, x = x),
                                                  span = span, degree = 2, family = "symmetric")),
                    error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    g$trend <- as.numeric(predict(fit, data.frame(x = x)))
    g[is.finite(g$trend), ]
  }))
  if (is.null(out)) d[0, ] else out
}

# Each category's share of a country's annual spending (%). Currency and the
# overall price level cancel out. A year with a missing month or category is dropped.
card_shares <- function(data, years, drop_other = FALSE, countries = c("Korea", "Taiwan"),
                        channel = FALSE) {
  d <- data[data$country %in% countries, ]
  if (drop_other) d <- d[d$origin != "OT", ]
  d$year <- as.integer(format(d$date, "%Y"))
  d <- d[d$year >= years[1] & d$year <= years[2], ]
  if (!nrow(d)) return(data.frame())
  if (channel) d$category <- ifelse(d$category == "EC", "EC", "OFF")
  out <- aggregate(amount_local ~ country + year + category, d, sum, na.action = na.pass)
  out$share <- ave(out$amount_local, out$country, out$year, FUN = function(x) x / sum(x) * 100)
  out <- out[is.finite(out$share), ]
  order <- if (channel) names(card_channel_labels) else card_share_order
  out <- out[order(out$country, out$year, match(out$category, order)), ]
  out$ymax <- ave(out$share, out$country, out$year, FUN = cumsum)
  out$ymin <- out$ymax - out$share
  labels <- if (channel) card_channel_labels else card_category_labels[card_share_order]
  out$label <- factor(labels[out$category], levels = labels)
  rownames(out) <- NULL
  out
}

card_partial_year <- function(end) {
  if (format(end, "%m") == "12") NA_integer_ else as.integer(format(end, "%Y"))
}
card_year_labels <- function(years, end) {
  partial <- card_partial_year(end)
  ifelse(!is.na(partial) & years == partial,
         paste0(years, "\n(Jan–", month.abb[as.integer(format(end, "%m"))], ")"), years)
}

card_share_shade <- function(p, years) {
  if (is.null(years)) p else p + annotate("rect", xmin = years[1] - .5, xmax = years[2] + .5,
                                          ymin = -Inf, ymax = Inf, fill = "#999999", alpha = .14)
}

card_share_bar_plot <- function(d, end, columns = 2L, shaded_years = NULL, reclass = FALSE) {
  years <- sort(unique(d$year))
  colors <- setNames(card_share_palette[seq_along(levels(d$label))], levels(d$label))
  p <- card_share_shade(ggplot(d), shaded_years) +
    geom_rect(aes(xmin = year - .36, xmax = year + .36, ymin = ymin, ymax = ymax, fill = label),
              color = "white", linewidth = .6)
  if (reclass && "Taiwan" %in% d$country) {
    mark <- data.frame(country = "Taiwan", x = 2020.5, label = "Category change, Feb 2021")
    p <- p + geom_segment(data = mark, aes(x = x, xend = x, y = 0, yend = 101),
                          linetype = "dashed", color = "#111111", linewidth = .4) +
      geom_text(data = mark, aes(x = x + .1, y = 104.5, label = label), hjust = 1, size = 3.2, color = "#111111")
  }
  p + scale_fill_manual(values = colors, breaks = rev(levels(droplevels(d$label))), name = NULL) +
    scale_x_continuous(breaks = years, labels = card_year_labels(years, end), expand = expansion(add = .3)) +
    scale_y_continuous(breaks = seq(0, 100, 25), labels = function(x) paste0(x, "%"),
                       limits = c(0, 108), expand = c(0, 0)) +
    labs(x = NULL, y = "Share of annual card spending") +
    facet_wrap(vars(country), ncol = columns) + chart_theme +
    theme(legend.position = if (columns == 1L) "bottom" else "right", legend.justification = "top",
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

card_share_line_plot <- function(d, end, columns = 2L, free_y = FALSE, shaded_years = NULL, reclass = FALSE) {
  years <- sort(unique(d$year))
  breaks <- if (length(years) > 6L) years[seq(1L, length(years), by = 2L)] else years
  p <- card_share_shade(ggplot(d, aes(year, share, color = country, shape = country)), shaded_years)
  if (reclass) p <- p + geom_vline(xintercept = 2020.5, linetype = "dashed", color = "#555555", linewidth = .4)
  p + geom_line(linewidth = .75) + geom_point(size = 2) +
    scale_colour_manual(values = card_country_colors, name = NULL) +
    scale_shape_manual(values = c(Korea = 16, Taiwan = 17), name = NULL) +
    scale_x_continuous(breaks = breaks, labels = card_year_labels(breaks, end)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(x = NULL, y = "Share of annual card spending") + chart_theme +
    facet_wrap(vars(panel), ncol = columns, scales = if (free_y) "free_y" else "fixed")
}

card_y_label <- function(unit, series) {
  if (series == "yoy") return("Year-over-year change (%)")
  label <- switch(unit, local = "Korea: KRW trillion\nTaiwan: NT$ billion",
                   index = "Index\n2014 average = 100",
                   real_index = "Inflation-adjusted index\n2014 average = 100")
  if (series == "ma12") paste0(label, "\n12-month average") else label
}

card_comparison_plot <- function(d, unit, series, columns = 2L, free_y = FALSE,
                                  shaded_period = NULL, restrictions = FALSE, trend = NULL) {
  comparable <- unit != "local" || series == "yoy"
  panels <- c("Total spending", "Offline spending", unname(card_category_labels))
  d$panel <- factor(d$panel, levels = panels)
  d$segment <- ave(card_month_id(d$date), interaction(d$country, d$panel, drop = TRUE),
                   FUN = function(month) cumsum(c(TRUE, diff(month) != 1L)))
  p <- ggplot(d, aes(date, value, color = country, shape = country,
                     group = interaction(country, panel, segment)))
  if (!is.null(shaded_period)) {
    # Date controls select calendar months, so include the entire last month.
    upper <- seq(as.Date(format(shaded_period[2], "%Y-%m-01")), by = "month", length.out = 2)[2] - 1
    shade <- data.frame(start = as.Date(format(shaded_period[1], "%Y-%m-01")), end = upper)
    p <- p + geom_rect(data = shade, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                        inherit.aes = FALSE, fill = "#999999", alpha = .14)
    if (restrictions) {
      windows <- data.frame(country = c("Korea", "Taiwan"),
                             start = as.Date(c("2021-07-01", "2021-05-01")),
                             end = as.Date(c("2021-10-31", "2021-07-31")))
      windows <- windows[windows$country %in% unique(d$country), ]
      p <- p + geom_rect(data = windows, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = country),
                          inherit.aes = FALSE, alpha = .12) +
        scale_fill_manual(values = card_country_colors, guide = "none")
    }
  }
  reference <- if (series == "yoy") 0 else if (unit != "local") 100 else NA_real_
  if (is.finite(reference)) p <- p + geom_hline(yintercept = reference, linetype = "dashed", color = "#555555")
  points <- d[format(d$date, "%m") %in% c("01", "07"), ]
  if (is.null(trend)) {
    p <- p + geom_line(linewidth = .75) + geom_point(data = points, size = 2)
  } else {
    # The series turns light; the bold line is the LOESS trend.
    trend$panel <- factor(trend$panel, levels = panels)
    trend$segment <- 1L
    trend_points <- trend[format(trend$date, "%m") %in% c("01", "07"), ]
    p <- p + geom_line(linewidth = .45, alpha = .35) +
      geom_line(data = trend, aes(y = trend), linewidth = 1.1) +
      geom_point(data = trend_points, aes(y = trend), size = 2)
  }
  p <- p +
    scale_colour_manual(values = card_country_colors, name = NULL) +
    scale_shape_manual(values = c(Korea = 16, Taiwan = 17), name = NULL) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(x = NULL, y = card_y_label(unit, series)) + chart_theme
  if (comparable) {
    p + facet_wrap(vars(panel), ncol = columns, scales = if (free_y) "free_y" else "fixed")
  } else {
    p + facet_wrap(vars(panel, country), ncol = columns, scales = "free_y")
  }
}

card_section_ui <- function(id, dates = as.Date(c("2014-01-01", "2023-08-01"))) {
  ns <- NS(id)
  tagList(
    tags$div(class = "filter-grid main-filters",
      selectInput(ns("view"), "View", c("Spending trends" = "trend",
        "Category shares (stacked bars)" = "share_bar", "Category shares (by category)" = "share_line"),
        selectize = FALSE),
      selectInput(ns("country"), "Country", c("Korea and Taiwan" = "both", "Korea", "Taiwan"), selectize = FALSE),
      conditionalPanel("input.view === 'trend'", ns = ns,
        selectInput(ns("scope"), "Compare", c("Total spending" = "total", "Selected categories" = "category"), selectize = FALSE)),
      conditionalPanel("input.view === 'trend' && input.scope === 'total'", ns = ns,
        selectInput(ns("total_type"), "Total includes", c("All spending (offline + e-commerce)" = "all",
          "Offline only (excluding e-commerce)" = "offline"), selectize = FALSE)),
      conditionalPanel("input.view === 'trend'", ns = ns,
        selectInput(ns("unit"), "Amount shown as", c("Local currency (KRW, NT$)" = "local",
          "2014 index" = "index", "Inflation-adjusted 2014 index" = "real_index"),
          selected = "real_index", selectize = FALSE)),
      conditionalPanel("input.view === 'trend'", ns = ns,
        selectInput(ns("series"), "Series", c("Monthly values" = "raw", "12-month moving average" = "ma12",
          "Year-over-year change (%)" = "yoy"), selectize = FALSE)),
      conditionalPanel("input.view === 'share_bar'", ns = ns,
        selectInput(ns("bar_split"), "Split bars by", c("All categories" = "cats",
          "Offline vs e-commerce" = "channel"), selectize = FALSE))),
    tags$div(class = "filter-grid analysis-options",
      dateRangeInput(ns("dates"), "Date range", start = dates[1], end = dates[2], min = dates[1], max = dates[2], format = "yyyy-mm"),
      checkboxInput(ns("drop_other"), "Exclude Other (OT)", FALSE),
      conditionalPanel("input.view !== 'trend'", ns = ns,
        checkboxInput(ns("include_partial"), paste0("Include ", format(dates[2], "%Y"), " (Jan–",
          month.abb[as.integer(format(dates[2], "%m"))], " only)"), FALSE)),
      conditionalPanel("(input.view === 'trend' && (input.unit !== 'local' || input.series === 'yoy')) || input.view === 'share_line'", ns = ns,
        checkboxInput(ns("free_y"), "Separate y-axis per category", FALSE)),
      conditionalPanel("input.view === 'trend'", ns = ns,
        checkboxInput(ns("loess"), "Add LOESS trend line", FALSE)),
      conditionalPanel("input.view === 'trend' && input.loess", ns = ns,
        sliderInput(ns("span"), "LOESS span", min = 0.1, max = 1, value = 0.5, step = 0.05)),
      conditionalPanel("(input.view === 'trend' && input.scope === 'category') || input.view === 'share_line'", ns = ns, class = "sensitivity-option",
        checkboxGroupInput(ns("categories"), "Categories", setNames(names(card_category_labels), unname(card_category_labels)),
          selected = c("FD", "TR", "EC"), inline = TRUE))),
    tags$details(class = "methods",
      tags$summary("Chart options"),
      tags$div(class = "filter-grid analysis-options",
        checkboxInput(ns("shade"), "Shade the COVID-19 period", TRUE),
        conditionalPanel("input.shade", ns = ns,
          dateRangeInput(ns("covid_range"), "Shaded period", start = as.Date("2020-01-01"), end = as.Date("2022-12-01"),
            min = dates[1], max = dates[2], format = "yyyy-mm")),
        conditionalPanel("input.shade && input.view === 'trend'", ns = ns,
          checkboxInput(ns("restrictions"), "Mark reference restriction windows", FALSE))),
      tags$p(class = "chart-note", "Optional reference windows supplied with this comparison: Korea, July–October 2021; Taiwan, May–July 2021. These do not define a measured national restriction maximum.")),
    tags$section(class = "analysis-section", "aria-labelledby" = ns("chart-title"),
      tags$h2(id = ns("chart-title"), textOutput(ns("title"), inline = TRUE)),
      tags$p(class = "chart-note", textOutput(ns("note"), inline = TRUE)),
      plotOutput(ns("chart"), height = "auto", hover = hoverOpts(ns("hover"), delay = 100, delayType = "throttle")),
      tags$p(class = "coverage", "aria-live" = "polite", textOutput(ns("hover_text"), inline = TRUE)),
      tags$p(class = "coverage", textOutput(ns("coverage"), inline = TRUE))),
    tags$section(class = "analysis-section", "aria-labelledby" = ns("summary-title"),
      tags$div(class = "section-header",
        tags$h2(id = ns("summary-title"), "Summary by country and category"),
        downloadLink(ns("download"), "Download CSV", class = "download-link")),
      tags$div(class = "table-scroll", tabindex = "0", role = "region", "aria-label" = "Scrollable card spending summary table",
        tableOutput(ns("summary")))),
    tags$details(class = "methods",
      tags$summary("Data and interpretation"),
      tags$p("This nationwide eight-category extract is separate from the food and apparel proxies in the joint analysis. Each index uses its own country and category's 2014 monthly average. CPI adjustment uses the supplied national consumer-price series."),
      tags$p("E-commerce is Korea's EC series and Taiwan's online spending summed across seven categories. The other seven series use offline amounts. Source coverage and category definitions can differ between countries."),
      tags$p("Excluding Other also removes its online component from Taiwan's e-commerce. Korea's e-commerce has no category breakdown and remains unchanged, so the exclusion is asymmetric."),
      tags$p("Share views use calendar years inside the date range and leave out the partial last year unless it is included. Shares divide by each country's annual card spending, so currency and the overall price level cancel out; category-specific price changes, such as fuel in Transport, still show. Taiwan's Food, Department stores and Other shares shift sharply from February 2021, most likely a merchant reclassification (dashed line)."),
      tags$p("The LOESS trend is a robust local quadratic fit (family = \"symmetric\"), so one-off spikes such as tax-payment months carry little weight. Smaller spans follow the data more closely. Hover values and downloads are from the series, not the trend."),
      tags$p("A 12-month average smooths seasonal variation and delays sharp changes. Year-over-year growth compares the same calendar month; it does not remove all holiday or reporting effects. Transformations use the full series before the displayed date range is applied."),
      tags$p("The supplied data identifies Taiwan card spending as NCCC. The Korean card and CPI extracts do not yet include source references.")))
}

card_section_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    view <- reactive(req(input$view))
    countries <- reactive(if (req(input$country) == "both") c("Korea", "Taiwan") else input$country)
    width <- reactive({
      w <- session$clientData[[paste0("output_", session$ns("chart"), "_width")]]
      if (is.null(w) || !is.finite(w) || w <= 0) 1000 else w
    })
    date_range <- reactive({
      dates <- as.Date(req(input$dates))
      validate(need(length(dates) == 2L && !anyNA(dates) && dates[1] <= dates[2], "Choose a valid date range."))
      as.Date(format(dates, "%Y-%m-01"))
    })

    # ---- Spending trends ----
    offline_total <- reactive(identical(input$total_type, "offline"))
    aggregated <- reactive({
      if (req(input$scope) == "category") validate(need(length(input$categories), "Select at least one category."))
      d <- card_aggregate(data$data, input$scope, input$categories, isTRUE(input$drop_other), countries(),
                          offline_only = offline_total())
      validate(need(nrow(d), "No categories remain. Choose another category or include Other."))
      d
    })
    transformed <- reactive(card_transform(aggregated(), req(input$unit), req(input$series)))
    selected <- reactive({
      dates <- date_range()
      d <- transformed()
      d <- d[d$date >= dates[1] & d$date <= dates[2] & is.finite(d$value), ]
      validate(need(nrow(d), "No values in this range. A 12-month history is needed for annual changes or moving averages."))
      d
    })
    trend <- reactive(if (isTRUE(input$loess)) card_loess(selected(), req(input$span)) else NULL)
    comparable <- reactive(req(input$unit) != "local" || req(input$series) == "yoy")

    # ---- Category shares ----
    share_years <- reactive({
      years <- as.integer(format(date_range(), "%Y"))
      partial <- card_partial_year(data$end)
      if (!is.na(partial) && !isTRUE(input$include_partial)) years[2] <- min(years[2], partial - 1L)
      validate(need(years[1] <= years[2], "Widen the date range to include at least one full year."))
      years
    })
    by_channel <- reactive(view() == "share_bar" && identical(input$bar_split, "channel"))
    reclass <- reactive(share_years()[1] <= 2020 && share_years()[2] >= 2021 && "Taiwan" %in% countries())
    shares <- reactive({
      d <- card_shares(data$data, share_years(), isTRUE(input$drop_other), countries(), by_channel())
      if (view() == "share_line") {
        keep <- intersect(names(card_category_labels), input$categories)
        if (isTRUE(input$drop_other)) keep <- setdiff(keep, "OT")
        validate(need(length(keep), "Select at least one category."))
        d <- d[d$category %in% keep, ]
        d$panel <- factor(card_category_labels[d$category], levels = card_category_labels[keep])
      }
      validate(need(nrow(d), "No data to show."))
      d
    })
    shaded_years <- reactive({
      if (!isTRUE(input$shade)) return(NULL)
      covid <- as.integer(format(as.Date(req(input$covid_range)), "%Y"))
      years <- c(max(covid[1], share_years()[1]), min(covid[2], share_years()[2]))
      if (years[1] <= years[2]) years else NULL
    })

    # ---- Shown data, layout and text ----
    shown <- reactive(if (view() == "trend") selected() else shares())
    layout <- reactive({
      d <- shown()
      count <- switch(view(),
        trend = if (comparable()) length(unique(d$panel)) else nrow(unique(d[c("country", "panel")])),
        share_bar = length(unique(d$country)),
        share_line = length(unique(d$panel)))
      columns <- if (width() < 590 || count == 1L) 1L else 2L
      per <- if (view() == "share_bar") 440L else 300L
      list(columns = columns, height = per * ceiling(count / columns) + if (view() == "share_bar" && columns == 1L) 160L else 55L)
    })
    output$title <- renderText(switch(view(),
      share_bar = if (by_channel()) "Offline and e-commerce shares of annual card spending"
                  else "Category shares of annual card spending",
      share_line = "Category shares of annual card spending, by category",
      trend = if (req(input$scope) != "total") "Card spending by category"
              else if (offline_total()) "Offline card spending" else "Total card spending"))
    output$note <- renderText({
      if (view() != "trend") {
        return(paste(
          if (view() == "share_bar") "Each bar splits one year's card spending and sums to 100%."
          else "Each category's share of yearly spending, Korea against Taiwan.",
          "Currency and the overall price level cancel out in the ratio."))
      }
      amount <- switch(req(input$unit),
        local = if (req(input$series) == "yoy")
          "Percentage changes in local-currency spending are compared across countries."
        else "Local-currency amounts use separate country panels: KRW trillion and NT$ billion.",
        index = "Each country's selected series is indexed to its own 2014 monthly average of 100.",
        real_index = "CPI-adjusted amounts are indexed to each series' own 2014 monthly average of 100.")
      treatment <- switch(req(input$series), raw = "Monthly values retain seasonal variation.",
        ma12 = "The trailing 12-month average smooths seasonality and delays sharp changes.",
        yoy = "Percentage change from the same calendar month one year earlier.")
      paste(c(amount, treatment,
              if (offline_total() && input$scope == "total") "The total covers the seven offline categories without e-commerce.",
              if (isTRUE(input$loess)) sprintf("Bold lines: robust LOESS trend (span %.2f); light lines: the series.", input$span)),
            collapse = " ")
    })
    output$coverage <- renderText({
      if (view() != "trend") {
        years <- share_years()
        partial <- card_partial_year(data$end)
        return(paste0(years[1], "–", years[2], " · Annual shares of nationwide card spending",
          if (!is.na(partial) && years[2] == partial) paste0(" · ", partial, " covers Jan–", format(data$end, "%b"), " only")
          else if (!is.na(partial)) paste0(" · ", partial, " left out (data end in ", format(data$end, "%B"), ")")))
      }
      d <- selected()
      paste(format(min(d$date), "%b %Y"), "–", format(max(d$date), "%b %Y"),
            "· Nationwide monthly card spending · 2014 baseline")
    })
    output$chart <- renderPlot({
      switch(view(),
        share_bar = card_share_bar_plot(shares(), data$end, layout()$columns, shaded_years(), reclass() && !by_channel()),
        share_line = card_share_line_plot(shares(), data$end, layout()$columns, isTRUE(input$free_y),
                                          shaded_years(), reclass()),
        trend = {
          period <- if (isTRUE(input$shade)) as.Date(req(input$covid_range)) else NULL
          card_comparison_plot(selected(), input$unit, input$series, layout()$columns,
                               isTRUE(input$free_y), period, isTRUE(input$restrictions), trend())
        })
    }, height = function() layout()$height, execOnResize = TRUE,
      alt = function() switch(view(),
        share_bar = paste0("Stacked bar chart for ", paste(countries(), collapse = " and "), ": each year's card spending split ",
          if (by_channel()) "into offline and e-commerce" else "by category", ", every bar summing to 100%. Values are available in the summary and CSV download."),
        share_line = "Line charts of each category's share of annual card spending, Korea in blue circles and Taiwan in orange triangles. Values are available in the summary and CSV download.",
        trend = paste("Monthly card spending for", paste(unique(selected()$country), collapse = " and "),
          "in the selected categories.", card_y_label(input$unit, input$series),
          if (isTRUE(input$loess)) "A bold LOESS trend line is drawn over the lighter series." else "",
          "Korea uses blue circles and Taiwan orange triangles. Values are available in the summary and CSV download.")))
    output$hover_text <- renderText({
      h <- input$hover
      if (is.null(h)) return("Hover over the chart for a value, or download the selected data below.")
      if (view() == "share_bar") {
        year <- round(h$x)
        segment <- if (is.null(h$panelvar1) || abs(h$x - year) > .36) shares()[0, ]
                   else subset(shares(), country == h$panelvar1 & year == round(h$x) & h$y >= ymin & h$y < ymax)
        if (!nrow(segment)) return("Hover over a bar segment to read its share.")
        return(sprintf("%d · %s · %s · %.1f%%", segment$year, segment$country, segment$label, segment$share))
      }
      share <- view() == "share_line"
      points <- nearPoints(shown(), h, xvar = if (share) "year" else "date", yvar = if (share) "share" else "value",
                           threshold = 12, maxpoints = 2)
      if (!nrow(points)) return("Hover over a line to read a value.")
      if (share) return(paste(points$year, points$country, points$panel, sprintf("%.1f%%", points$share),
                              sep = " · ", collapse = "; "))
      paste(paste(format(points$date, "%Y-%m"), points$country, points$panel,
                   formatC(points$value, format = "f", digits = 2, big.mark = ","), sep = " · "), collapse = "; ")
    })
    output$summary <- renderTable({
      d <- shown()
      if (view() != "trend") {
        groups <- split(d, interaction(d$country, d$label, drop = TRUE))
        return(do.call(rbind, lapply(groups, function(g) data.frame(
          Series = paste(g$country[1], g$label[1], sep = " · "), Years = nrow(g),
          `First share (%)` = g$share[which.min(g$year)], `Last share (%)` = g$share[which.max(g$year)],
          `Change (pp)` = g$share[which.max(g$year)] - g$share[which.min(g$year)], check.names = FALSE))))
      }
      groups <- split(d, interaction(d$country, d$panel, drop = TRUE))
      do.call(rbind, lapply(groups, function(g) data.frame(
        Series = paste(g$country[1], g$panel[1], sep = " · "), Months = nrow(g),
        Median = median(g$value), IQR = IQR(g$value), Mean = mean(g$value), SD = sd(g$value))))
    }, digits = 2, striped = FALSE, bordered = FALSE, spacing = "s")
    output$download <- downloadHandler(
      filename = function() if (view() == "trend") paste0("card-comparison-", input$unit, "-", input$series, ".csv")
                            else paste0("card-comparison-", view(), ".csv"),
      content = function(file) {
        d <- shown()
        if (view() == "trend") {
          d$amount_unit <- ifelse(d$country == "Korea", "KRW", "NTD")
          d$display_unit <- input$unit
          d$series <- input$series
        } else {
          d <- d[c("country", "year", "category", "label", "amount_local", "share")]
          d$amount_unit <- ifelse(d$country == "Korea", "KRW", "NTD")
        }
        d$excluded_other <- isTRUE(input$drop_other)
        write.csv(d, file, row.names = FALSE, na = "")
      })
  })
}
