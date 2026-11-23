# Nationwide card comparison adapted from the attachment to PR #5.
# Keep these inputs separate from the joint analysis's food/apparel extracts.
card_category_labels <- c(
  FD = "Food (FD)", CT = "Clothing (CT)", LG = "Housing and lodging (LG)",
  TR = "Transport (TR)", EE = "Education and entertainment (EE)",
  DP = "Department stores (DP)", OT = "Other (OT)", EC = "E-commerce (EC)"
)
card_country_colors <- c(Korea = "#2464bd", Taiwan = "#b95013")
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
                            drop_other = FALSE, countries = c("Korea", "Taiwan")) {
  d <- data[data$country %in% countries, ]
  if (drop_other) d <- d[d$origin != "OT", ]
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
  out$panel <- ifelse(out$category == "total", "Total spending", card_category_labels[out$category])
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

card_y_label <- function(unit, series) {
  if (series == "yoy") return("Year-over-year change (%)")
  label <- switch(unit, local = "Korea: KRW trillion / Taiwan: NT$ billion",
                   index = "Index, 2014 average = 100",
                   real_index = "Inflation-adjusted index, 2014 average = 100")
  if (series == "ma12") paste0(label, " · 12-month average") else label
}

card_comparison_plot <- function(d, unit, series, columns = 2L, free_y = FALSE,
                                  shaded_period = NULL, restrictions = FALSE) {
  comparable <- unit != "local" || series == "yoy"
  d$panel <- factor(d$panel, levels = c("Total spending", unname(card_category_labels)))
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
  p <- p + geom_line(linewidth = .75) + geom_point(data = points, size = 2) +
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
      selectInput(ns("country"), "Country", c("Korea and Taiwan" = "both", "Korea", "Taiwan"), selectize = FALSE),
      selectInput(ns("scope"), "Compare", c("Total spending" = "total", "Selected categories" = "category"), selectize = FALSE),
      selectInput(ns("unit"), "Amount shown as", c("Local currency (KRW, NT$)" = "local",
        "2014 index" = "index", "Inflation-adjusted 2014 index" = "real_index"),
        selected = "real_index", selectize = FALSE),
      selectInput(ns("series"), "Series", c("Monthly values" = "raw", "12-month moving average" = "ma12",
        "Year-over-year change (%)" = "yoy"), selectize = FALSE)),
    tags$div(class = "filter-grid analysis-options",
      dateRangeInput(ns("dates"), "Date range", start = dates[1], end = dates[2], min = dates[1], max = dates[2], format = "yyyy-mm"),
      checkboxInput(ns("drop_other"), "Exclude Other (OT)", FALSE),
      conditionalPanel("input.unit !== 'local' || input.series === 'yoy'", ns = ns,
        checkboxInput(ns("free_y"), "Separate y-axis per category", FALSE)),
      conditionalPanel("input.scope === 'category'", ns = ns, class = "sensitivity-option",
        checkboxGroupInput(ns("categories"), "Categories", setNames(names(card_category_labels), unname(card_category_labels)),
          selected = c("FD", "TR", "EC"), inline = TRUE))),
    tags$details(class = "methods",
      tags$summary("Chart options"),
      tags$div(class = "filter-grid analysis-options",
        checkboxInput(ns("shade"), "Shade the COVID-19 period", TRUE),
        conditionalPanel("input.shade", ns = ns,
          dateRangeInput(ns("covid_range"), "Shaded period", start = as.Date("2020-01-01"), end = as.Date("2022-12-01"),
            min = dates[1], max = dates[2], format = "yyyy-mm")),
        conditionalPanel("input.shade", ns = ns,
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
      tags$p("A 12-month average smooths seasonal variation and delays sharp changes. Year-over-year growth compares the same calendar month; it does not remove all holiday or reporting effects. Transformations use the full series before the displayed date range is applied."),
      tags$p("The supplied data identifies Taiwan card spending as NCCC. The Korean card and CPI extracts do not yet include source references.")))
}

card_section_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    aggregated <- reactive({
      country <- if (req(input$country) == "both") c("Korea", "Taiwan") else input$country
      if (req(input$scope) == "category") validate(need(length(input$categories), "Select at least one category."))
      d <- card_aggregate(data$data, input$scope, input$categories, isTRUE(input$drop_other), country)
      validate(need(nrow(d), "No categories remain. Choose another category or include Other."))
      d
    })
    transformed <- reactive(card_transform(aggregated(), req(input$unit), req(input$series)))
    selected <- reactive({
      dates <- as.Date(req(input$dates))
      validate(need(length(dates) == 2L && !anyNA(dates) && dates[1] <= dates[2], "Choose a valid date range."))
      dates <- as.Date(format(dates, "%Y-%m-01"))
      d <- transformed()
      d <- d[d$date >= dates[1] & d$date <= dates[2] & is.finite(d$value), ]
      validate(need(nrow(d), "No values in this range. A 12-month history is needed for annual changes or moving averages."))
      d
    })
    comparable <- reactive(req(input$unit) != "local" || req(input$series) == "yoy")
    layout <- reactive({
      d <- selected()
      width <- session$clientData$output_chart_width
      if (is.null(width) || !is.finite(width) || width <= 0) width <- 1000
      count <- if (comparable()) length(unique(d$panel)) else nrow(unique(d[c("country", "panel")]))
      columns <- if (width < 590 || count == 1L) 1L else 2L
      list(columns = columns, height = 300L * ceiling(count / columns) + 55L)
    })
    output$title <- renderText(if (req(input$scope) == "total") "Total card spending" else "Card spending by category")
    output$note <- renderText({
      amount <- switch(req(input$unit),
        local = "Local-currency amounts use separate country panels: KRW trillion and NT$ billion.",
        index = "Each country's selected series is indexed to its own 2014 monthly average of 100.",
        real_index = "CPI-adjusted amounts are indexed to each series' own 2014 monthly average of 100.")
      treatment <- switch(req(input$series), raw = "Monthly values retain seasonal variation.",
        ma12 = "The trailing 12-month average smooths seasonality and delays sharp changes.",
        yoy = "Percentage change from the same calendar month one year earlier.")
      paste(amount, treatment)
    })
    output$coverage <- renderText({
      d <- selected()
      paste(format(min(d$date), "%b %Y"), "–", format(max(d$date), "%b %Y"),
            "· Nationwide monthly card spending · 2014 baseline")
    })
    output$chart <- renderPlot({
      period <- if (isTRUE(input$shade)) as.Date(req(input$covid_range)) else NULL
      card_comparison_plot(selected(), input$unit, input$series, layout()$columns,
                             isTRUE(input$free_y), period, isTRUE(input$restrictions))
    }, height = function() layout()$height, execOnResize = TRUE,
      alt = function() paste("Monthly card spending for", paste(unique(selected()$country), collapse = " and "),
        "in the selected categories.", card_y_label(input$unit, input$series),
        "Korea uses blue circles and Taiwan orange triangles. Values are available in the summary and CSV download."))
    output$hover_text <- renderText({
      if (is.null(input$hover)) return("Hover over the chart for a value, or download the selected monthly data below.")
      points <- nearPoints(selected(), input$hover, xvar = "date", yvar = "value", threshold = 12, maxpoints = 2)
      if (!nrow(points)) return("Hover over a line to read a monthly value.")
      paste(paste(format(points$date, "%Y-%m"), points$country, points$panel,
                   formatC(points$value, format = "f", digits = 2, big.mark = ","), sep = " · "), collapse = "; ")
    })
    output$summary <- renderTable({
      groups <- split(selected(), interaction(selected()$country, selected()$panel, drop = TRUE))
      do.call(rbind, lapply(groups, function(g) data.frame(
        Series = paste(g$country[1], g$panel[1], sep = " · "), Months = nrow(g),
        Median = median(g$value), IQR = IQR(g$value), Mean = mean(g$value), SD = sd(g$value))))
    }, digits = 2, striped = FALSE, bordered = FALSE, spacing = "s")
    output$download <- downloadHandler(
      filename = function() paste0("card-comparison-", input$unit, "-", input$series, ".csv"),
      content = function(file) {
        d <- selected()
        d$amount_unit <- ifelse(d$country == "Korea", "KRW", "NTD")
        d$display_unit <- input$unit
        d$series <- input$series
        d$excluded_other <- isTRUE(input$drop_other)
        write.csv(d, file, row.names = FALSE, na = "")
      })
  })
}
