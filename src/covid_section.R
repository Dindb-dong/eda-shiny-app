# COVID-19 comparison section: daily Korea vs Taiwan, 2021–2022.
# A Shiny module so its input and output IDs do not collide with the
# COVID-19 ↔ Card spending section. Uses base R, shiny and ggplot2 only.
# Depends on `countries` (src/analysis.R) and the colours and
# `chart_theme` defined in src/plots.R, so source it after both.

# Fixed 2021 denominators: Korea's projected population and Taiwan's year-end
# registered population. These national definitions differ; see README sources.
covid_population <- c(Korea = 51744876, Taiwan = 23375314)

covid_daily_measures <- c(cases = "New cases per million (7-day average)",
                          deaths = "New deaths per million (7-day average)")
covid_daily_views <- c(trend = "Daily trend and restriction stringency",
                       scatter = "Daily cases or deaths against stringency")
covid_view_choices <- c("Trend over time" = "trend", "Against stringency" = "scatter")
covid_measure_choices <- c("Cases per million" = "cases", "Deaths per million" = "deaths")
covid_daily_notes <- c(
  trend = paste("Lines show trailing 7-day averages of daily reports per million residents.",
                "The lower panel shows the Stringency Index on the same days.",
                "Dotted lines mark policy dates for context."),
  scatter = paste("Each point is one country-day. Circles are Korea and triangles are Taiwan.",
                  "Lines are smooth local linear fits (LOESS, span 0.9) within each country.")
)

covid_country_colors <- c(Korea = blue, Taiwan = orange)
covid_country_lines <- c(Korea = "solid", Taiwan = "22")
covid_country_shapes <- c(Korea = 16, Taiwan = 17)

# Policy dates shown for context; official announcement links are in README.md.
covid_events <- data.frame(
  date = as.Date(c("2021-05-19", "2021-11-01", "2022-04-18", "2022-10-13")),
  country = c("Taiwan", "Korea", "Korea", "Taiwan"),
  label = c("Taiwan Level 3 alert", "Korea gradual recovery",
            "Korea distancing lifted", "Taiwan quarantine ends")
)

# Mean of the current day and the previous `days - 1` calendar days.
# Windows follow dates, not rows, so a missing day cannot stretch a window.
trailing_mean <- function(x, dates, days = 7L) {
  full <- seq(min(dates), max(dates), by = "day")
  v <- x[match(full, dates)]
  out <- vapply(seq_along(full), function(i) {
    if (i < days) return(NA_real_)
    w <- v[(i - days + 1L):i]
    if (any(!is.finite(w))) NA_real_ else mean(w)
  }, numeric(1))
  out[match(dates, full)]
}

read_covid_daily <- function(path = "data") {
  d <- read.csv(file.path(path, "covid_daily.csv"), fileEncoding = "UTF-8-BOM")
  d$date <- as.Date(d$date)
  stopifnot(!anyNA(d$date), !anyDuplicated(d[c("country_code", "date")]),
            setequal(d$country_code, names(countries)))
  d$country <- unname(countries[d$country_code])
  d <- d[order(d$country, d$date), ]
  population <- unname(covid_population[d$country])
  # Retain reporting corrections, consistently with the monthly joint analysis.
  per_million <- function(x) x / population * 1e6
  d$cases_pm <- per_million(d$jhu_new_confirmed)
  d$deaths_pm <- per_million(d$jhu_new_deaths)
  d$stringency <- d$owid_stringency_index
  d <- do.call(rbind, lapply(split(d, d$country), function(g) {
    g$cases_7day <- trailing_mean(g$cases_pm, g$date)
    g$deaths_7day <- trailing_mean(g$deaths_pm, g$date)
    g
  }))
  rownames(d) <- NULL
  d[c("country", "date", "jhu_new_confirmed", "jhu_new_deaths",
      "cases_pm", "deaths_pm", "cases_7day", "deaths_7day", "stringency")]
}


# ---- Plot helpers ----------------------------------------------------------

covid_value_scale <- function(metric, compressed) {
  number_labels <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE,
                                      drop0trailing = TRUE)
  if (!compressed) return(scale_y_continuous(labels = number_labels))
  breaks <- if (metric == "deaths") c(0, .1, 1, 10) else c(0, 1, 10, 100, 1000, 10000)
  breaks <- sort(unique(c(-breaks, breaks)))
  scale_y_continuous(breaks = asinh(breaks), labels = number_labels(breaks))
}

# Breaks start on the first of a month so labels read Jan, Apr, Jul, Oct.
covid_date_axis <- function(dates, narrow = FALSE) {
  dates <- range(dates)
  span <- as.numeric(diff(dates))
  months <- if (span > 400) (if (narrow) 6 else 3) else if (span > 120) (if (narrow) 2 else 1) else 0
  if (months == 0) {
    breaks <- seq(dates[1], dates[2], by = if (narrow) "2 weeks" else "1 week")
    labels <- "%d %b"
  } else {
    first <- as.Date(format(dates[1], "%Y-%m-01"))
    breaks <- seq(first, dates[2], by = "month")
    breaks <- breaks[(as.integer(format(breaks, "%m")) - 1) %% months == 0 & breaks >= dates[1]]
    labels <- if (narrow) "%b %y" else "%b %Y"
  }
  scale_x_date(limits = dates, breaks = breaks, date_labels = labels)
}

covid_event_layers <- function(events, labels) {
  if (nrow(events) == 0) return(list())
  layers <- list(geom_vline(data = events, aes(xintercept = date),
                            colour = quiet, linetype = "dotted", linewidth = .5))
  if (labels) {
    layers <- c(layers, list(geom_text(
      data = events, aes(x = date, y = Inf, label = label), inherit.aes = FALSE,
      angle = 90, hjust = 1.05, vjust = -.45, size = 3.4, colour = quiet)))
  }
  layers
}

covid_country_scales <- function(shapes = FALSE) list(
  scale_colour_manual(values = covid_country_colors, name = NULL),
  scale_linetype_manual(values = covid_country_lines, name = NULL),
  if (shapes) scale_shape_manual(values = covid_country_shapes, name = NULL)
)

covid_trend_plot <- function(d, metric, compressed, dates, events, narrow = FALSE) {
  value <- d[[paste0(metric, "_7day")]]
  d$y <- if (compressed) asinh(value) else value
  ggplot(d, aes(date, y, colour = country, linetype = country)) +
    covid_event_layers(events, labels = !narrow) +
    geom_line(linewidth = .8, na.rm = TRUE) +
    covid_country_scales() + covid_date_axis(dates, narrow) + covid_value_scale(metric, compressed) +
    labs(x = NULL, y = if (metric == "deaths") "Deaths per million" else "Cases per million") +
    chart_theme + theme(legend.position = "top", legend.justification = "left")
}

covid_stringency_plot <- function(d, dates, events, narrow = FALSE) {
  ggplot(d, aes(date, stringency, colour = country, linetype = country)) +
    covid_event_layers(events, labels = FALSE) +
    geom_step(linewidth = .7, direction = "hv", na.rm = TRUE) +
    covid_country_scales() + covid_date_axis(dates, narrow) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 50)) +
    labs(x = NULL, y = "Stringency") +
    chart_theme + theme(legend.position = "none")
}

# Stacks the two panels so their date axes line up exactly.
draw_covid_trend <- function(d, metric, compressed, dates, events, narrow = FALSE) {
  top <- ggplotGrob(covid_trend_plot(d, metric, compressed, dates, events, narrow))
  bottom <- ggplotGrob(covid_stringency_plot(d, dates, events, narrow))
  grid::grid.newpage()
  if (ncol(top) != ncol(bottom)) {
    layout <- grid::grid.layout(2, 1, heights = grid::unit(c(2.2, 1), "null"))
    grid::pushViewport(grid::viewport(layout = layout))
    for (row in 1:2) {
      grid::pushViewport(grid::viewport(layout.pos.row = row))
      grid::grid.draw(if (row == 1) top else bottom)
      grid::upViewport()
    }
    return(invisible(NULL))
  }
  both <- rbind(top, bottom, size = "max")
  panels <- both$layout$t[grepl("^panel", both$layout$name)]
  both$heights[min(panels)] <- grid::unit(2.2, "null")
  grid::grid.draw(both)
  invisible(NULL)
}

covid_scatter_plot <- function(d, metric, compressed) {
  value <- d[[paste0(metric, "_7day")]]
  d$y <- if (compressed) asinh(value) else value
  d <- d[is.finite(d$y) & is.finite(d$stringency), , drop = FALSE]
  # LOESS needs enough distinct points; skip the fit for very short ranges.
  enough <- names(Filter(function(g) nrow(g) >= 20 && length(unique(g$stringency)) >= 5,
                         split(d, d$country)))
  ggplot(d, aes(stringency, y, colour = country, shape = country)) +
    geom_point(size = 1.6, alpha = .45) +
    geom_smooth(data = d[d$country %in% enough, , drop = FALSE], aes(linetype = country),
                method = "loess", formula = y ~ x, method.args = list(degree = 1), span = .9,
                se = FALSE, linewidth = .9) +
    (if (nrow(d)) covid_country_scales(shapes = TRUE) else NULL) + covid_value_scale(metric, compressed) +
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
    labs(x = "Restriction stringency (0–100), same day",
         y = if (metric == "deaths") "Deaths per million (7-day average)" else "Cases per million (7-day average)") +
    chart_theme
}

covid_summary <- function(d) {
  finite_or_na <- function(x) if (is.finite(x)) x else NA_real_
  complete_total <- function(x) if (any(!is.finite(x))) NA_real_ else sum(x)
  do.call(rbind, lapply(split(d, d$country), function(g) {
    peak <- which.max(g$cases_7day)
    data.frame(
      Country = g$country[1], Days = nrow(g),
      "Peak cases per million (7-day avg)" = if (length(peak)) g$cases_7day[peak] else NA_real_,
      "Peak date" = if (length(peak)) format(g$date[peak], "%d %b %Y") else NA_character_,
      "Cases per million (total)" = complete_total(g$cases_pm),
      "Deaths per million (total)" = complete_total(g$deaths_pm),
      "Mean stringency" = finite_or_na(mean(g$stringency, na.rm = TRUE)),
      "Days with stringency ≥ 50" = sum(g$stringency >= 50, na.rm = TRUE),
      check.names = FALSE)
  }))
}


# ---- Module UI -------------------------------------------------------------

covid_section_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(class = "filter-grid main-filters",
      selectInput(ns("country"), "Country", c("Korea and Taiwan" = "both", "Korea", "Taiwan"),
                  selectize = FALSE),
      selectInput(ns("view"), "View", covid_view_choices, selectize = FALSE),
      selectInput(ns("metric"), "COVID-19 measure (7-day average)", covid_measure_choices,
                  selectize = FALSE),
      dateRangeInput(ns("dates"), "Dates", start = "2021-01-01", end = "2022-12-31",
                     min = "2021-01-01", max = "2022-12-31", format = "d M yyyy")),
    tags$div(class = "filter-grid analysis-options",
      checkboxInput(ns("compressed"), "Compress large values (inverse hyperbolic sine scale)",
                    value = FALSE),
      conditionalPanel("input.view === 'trend'", ns = ns,
        checkboxInput(ns("events"), "Mark key policy dates", value = TRUE))),
    tags$section(class = "analysis-section", "aria-labelledby" = ns("chart-title"),
      tags$h2(id = ns("chart-title"), textOutput(ns("title"), inline = TRUE)),
      tags$p(class = "chart-note", textOutput(ns("note"), inline = TRUE)),
      plotOutput(ns("chart"), height = "auto"),
      tags$p(class = "coverage", textOutput(ns("coverage"), inline = TRUE))),
    tags$section(class = "analysis-section", "aria-labelledby" = ns("summary-title"),
      tags$div(class = "section-header",
        tags$h2(id = ns("summary-title"), "Summary by country"),
        downloadLink(ns("download"), "Download CSV", class = "download-link")),
      tags$div(class = "table-scroll", tabindex = "0", role = "region",
               "aria-label" = "Scrollable COVID-19 summary table",
               tableOutput(ns("summary"))),
      tags$p(class = "coverage",
             "Totals add daily reports per million over the selected days; missing reports leave a total unavailable. The peak uses the 7-day average.")),
    tags$details(class = "methods",
      tags$summary("Data and interpretation"),
      tags$p("Daily cases and deaths come from Johns Hopkins University CSSE. The Stringency Index comes from the Oxford COVID-19 Government Response Tracker via Our World in Data."),
      tags$p("Rates use fixed 2021 population denominators (Korea 51.74 million projected residents, Taiwan 23.38 million year-end registered residents). These national population definitions differ. A 7-day average requires reports for the current and previous six calendar days; the first six days of 2021 have no average."),
      tags$p("Negative daily counts are reporting corrections and are retained, consistently with the COVID-19 ↔ Card spending monthly sums."),
      tags$p("The compressed scale uses the inverse hyperbolic sine. It shows small and large waves together and retains zeros and negative reporting corrections."),
      tags$p("Policy dates: Taiwan’s nationwide Level 3 alert (19 May 2021), Korea’s first gradual recovery phase (1 Nov 2021), the end of Korea’s social distancing rules (18 Apr 2022), and the end of Taiwan’s mandatory arrival quarantine (13 Oct 2022). They give context and do not imply cause."),
      tags$p("Reported counts depend on testing and reporting practices in each country. These charts describe patterns and do not estimate policy effects."))
  )
}


# ---- Module server ---------------------------------------------------------

covid_section_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    selected <- reactive({
      dates <- input$dates
      validate(
        need(length(dates) == 2 && !anyNA(dates), "Choose a start and an end date."),
        need(dates[1] <= dates[2], "The start date must be on or before the end date."))
      country <- if (req(input$country) == "both") unname(countries) else input$country
      d <- data[data$country %in% country & data$date >= dates[1] & data$date <= dates[2], ,
                drop = FALSE]
      validate(need(nrow(d) > 0, "No days fall in the selected range."))
      d
    })

    events <- reactive({
      if (!isTRUE(input$events)) return(covid_events[0, ])
      d <- selected()
      covid_events[covid_events$country %in% d$country &
                     covid_events$date >= min(d$date) & covid_events$date <= max(d$date), ]
    })

    output$title <- renderText(covid_daily_views[[req(input$view)]])
    output$note <- renderText(covid_daily_notes[[req(input$view)]])

    chart_width <- reactive(session$clientData[[paste0("output_", session$ns("chart"), "_width")]] %||% 1000)

    output$chart <- renderPlot({
      d <- selected()
      metric <- req(input$metric)
      compressed <- isTRUE(input$compressed)
      narrow <- chart_width() < 600
      if (req(input$view) == "scatter") return(covid_scatter_plot(d, metric, compressed))
      draw_covid_trend(d, metric, compressed, input$dates, events(), narrow)
    }, height = function() if (identical(input$view, "scatter")) 460 else 600, execOnResize = TRUE,
       alt = function() paste(covid_daily_views[[req(input$view)]], "for",
                              paste(unique(selected()$country), collapse = " and ")))

    output$coverage <- renderText({
      d <- selected()
      day <- function(x) sub("^0", "", format(x, "%d %b %Y"))
      paste(day(min(d$date)), "–", day(max(d$date)), "· Daily national reports")
    })

    output$summary <- renderTable(covid_summary(selected()),
                                  digits = 1, spacing = "s", width = "100%", na = "—",
                                  format.args = list(big.mark = ","))

    output$download <- downloadHandler(
      filename = function() paste0("covid-daily-", input$dates[1], "-to-", input$dates[2], ".csv"),
      content = function(file) write.csv(selected(), file, row.names = FALSE, na = "")
    )
  })
}
