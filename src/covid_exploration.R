# COVID-19 timeline, distribution and summary module.

exploration_country_colors <- c(Korea = "#2E6FBA", Taiwan = "#D1495B")

covid_metrics <- c(
  "New cases"                          = "new_cases",
  "New deaths"                         = "new_deaths",
  "Stringency Index (0-100)"           = "stringency",
  "At least one dose (% of population)" = "owid_people_vaccinated_per_hundred",
  "Fully vaccinated (% of population)" = "owid_people_fully_vaccinated_per_hundred",
  "Booster doses per 100 people"       = "owid_total_boosters_per_hundred",
  "Reproduction rate (R)"              = "owid_reproduction_rate"
)
count_metrics <- c(new_cases = "jhu_new_confirmed", new_deaths = "jhu_new_deaths")
rate_metrics <- c(new_cases = "cases_pm", new_deaths = "deaths_pm")

# 두 나라의 같은 분위수끼리 짝짓기 (two-sample Q-Q, Tukey mean-difference용)
two_sample_q <- function(d, probs = seq(0.01, 0.99, by = 0.01)) {
  validate(need(all(c("Korea", "Taiwan") %in% d$country),
                "Select both countries to compare quantiles."))
  data.frame(
    p  = probs,
    kr = quantile(d$value[d$country == "Korea"], probs, names = FALSE),
    tw = quantile(d$value[d$country == "Taiwan"],      probs, names = FALSE)
  )
}

# ---- UI ---------------------------------------------------------------------
covidTabUI <- function(id) {
  ns <- NS(id)   # 모듈 안의 inputId가 다른 탭과 겹치지 않게 해 줌

  sidebarLayout(
    sidebarPanel(
      width = 3,
      checkboxGroupInput(ns("countries"), "Country",
                         choices  = names(exploration_country_colors),
                         selected = names(exploration_country_colors)),
      selectInput(ns("metric"), "Variable", choices = covid_metrics),
      checkboxInput(ns("per_million"),
                    "Per million people (cases and deaths)", value = TRUE),
      radioButtons(ns("freq"), "Time resolution",
                   choices = c("Daily" = "daily",
                               "7-day average" = "ma7",
                               "Monthly" = "monthly"),
                   selected = "ma7"),
      dateRangeInput(ns("dates"), "Period",
                     start = "2021-01-01", end = "2022-12-31",
                     min   = "2021-01-01", max = "2022-12-31"),
      checkboxInput(ns("log"), "Log scale: log10(x + 1)", value = FALSE),
      checkboxInput(ns("facet"), "Separate panel for each country", value = FALSE),
      helpText(
        "Cases and deaths: Johns Hopkins University CSSE.",
        "Stringency Index: Oxford COVID-19 Government Response Tracker, via Our World in Data.",
        "Vaccination: Our World in Data. R: TrackingR, via Our World in Data.",
        "Daily counts are differences of cumulative totals; reporting corrections are retained.",
        "Per-million values adjust for Korea's population being about twice Taiwan's."
      )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Timeline",
          plotOutput(ns("ts_plot"), height = "430px"),
          helpText(
            "When did each country's waves and restrictions rise and fall?",
            "Use 'Separate panel' when the two countries' scales differ a lot,",
            "and the 7-day average to remove day-of-week reporting patterns."
          )
        ),
        tabPanel(
          "Distribution",
          selectInput(ns("dist_type"), "Plot type",
                      choices = c("Histogram + density + rug"     = "hist",
                                  "Boxplot"                       = "box",
                                  "ECDF"                          = "ecdf",
                                  "Two-sample Q-Q plot"           = "qq",
                                  "Tukey mean-difference plot"    = "md")),
          plotOutput(ns("dist_plot"), height = "430px"),
          helpText(
            "Q-Q: points on the dashed line mean the two distributions match;",
            "a parallel shift means a level difference, a different slope means a spread difference.",
            "Days are serially dependent, so read these plots descriptively."
          )
        ),
        tabPanel(
          "Summary",
          tableOutput(ns("summary_tbl")),
          helpText(
            "Compare the mean and SD with the median and IQR:",
            "a mean far above the median indicates a right-skewed series driven by a few large waves."
          )
        )
      )
    )
  )
}

# ---- Server -----------------------------------------------------------------
covidTabServer <- function(id, covid_daily) {
  moduleServer(id, function(input, output, session) {

    # 축 이름: 선택한 옵션에 맞게 자동으로 바뀜
    metric_label <- reactive({
      lab <- names(covid_metrics)[covid_metrics == input$metric]
      is_count <- input$metric %in% names(count_metrics)
      if (is_count && input$per_million) lab <- paste(lab, "per million")
      if (input$freq == "ma7") lab <- paste(lab, "(7-day avg)")
      if (input$freq == "monthly") {
        lab <- paste(lab, if (is_count) "(monthly total)" else "(monthly mean)")
      }
      if (input$log) lab <- paste0("log10(", lab, " + 1)")
      lab
    })

    # 위젯 값에 따라 걸러지고 변환된 자료: country, date, value
    plot_data <- reactive({
      validate(need(length(input$countries) > 0, "Select at least one country."))
      dates <- input$dates
      validate(
        need(length(dates) == 2 && !anyNA(dates), "Choose a start and an end date."),
        need(dates[1] <= dates[2], "The start date must be on or before the end date."))

      var <- input$metric
      is_count <- var %in% names(count_metrics)
      if (is_count) var <- if (input$per_million) rate_metrics[[var]] else count_metrics[[var]]

      precomputed <- input$freq == "ma7" && is_count && isTRUE(input$per_million)
      if (precomputed) var <- sub("_pm$", "_7day", var)

      d <- covid_daily |>
        filter(country %in% input$countries) |>
        transmute(country, date, value = .data[[var]])

      if (input$freq == "ma7" && !precomputed) {
        d <- d |>
          group_by(country) |>
          mutate(value = trailing_mean(value, date)) |>
          ungroup()
      }
      d <- d |> filter(date >= dates[1], date <= dates[2])

      if (input$freq == "monthly") {
        agg <- if (is_count) {
          function(x) sum(x, na.rm = TRUE)
        } else {
          function(x) mean(x, na.rm = TRUE)
        }
        d <- d |>
          mutate(date = as.Date(format(date, "%Y-%m-01"))) |>
          group_by(country, date) |>
          summarise(value = agg(value), .groups = "drop")
      }

      if (input$log) d <- d |> mutate(value = log10(value + 1))

      d |> filter(is.finite(value))   # NA/NaN 제거
    })

    # ---- 1) 시계열 ----
    output$ts_plot <- renderPlot({
      d <- plot_data()
      validate(need(nrow(d) > 0, "No data for this selection."))

      p <- ggplot(d, aes(date, value, colour = country)) +
        geom_line(linewidth = 0.7) +
        scale_colour_manual(values = exploration_country_colors) +
        scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
        labs(x = NULL, y = metric_label(), colour = NULL) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "top")

      if (input$freq == "monthly") p <- p + geom_point(size = 2)
      if (input$facet) p <- p + facet_wrap(~ country, ncol = 1, scales = "free_y")
      p
    })

    # ---- 2) 분포 비교 ----
    output$dist_plot <- renderPlot({
      d <- plot_data()
      validate(need(nrow(d) > 2, "Not enough observations for this selection."))
      lab  <- metric_label()
      base <- theme_minimal(base_size = 14)

      switch(
        input$dist_type,

        hist = ggplot(d, aes(value, fill = country)) +
          geom_histogram(aes(y = after_stat(density)),
                         bins = 30, alpha = 0.5, colour = "white") +
          geom_density(aes(colour = country), fill = NA, linewidth = 0.8) +
          geom_rug(aes(colour = country), alpha = 0.4) +
          facet_wrap(~ country, ncol = 1) +
          scale_fill_manual(values = exploration_country_colors) +
          scale_colour_manual(values = exploration_country_colors) +
          labs(x = lab, y = "Density") +
          base + theme(legend.position = "none"),

        box = ggplot(d, aes(country, value, fill = country)) +
          geom_boxplot(alpha = 0.6) +
          scale_fill_manual(values = exploration_country_colors) +
          labs(x = NULL, y = lab) +
          base + theme(legend.position = "none"),

        ecdf = ggplot(d, aes(value, colour = country)) +
          stat_ecdf(linewidth = 0.8) +
          scale_colour_manual(values = exploration_country_colors) +
          labs(x = lab, y = "Cumulative proportion", colour = NULL) +
          base + theme(legend.position = "top"),

        qq = {
          qd <- two_sample_q(d)
          ggplot(qd, aes(kr, tw)) +
            geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
            geom_point(colour = "grey20") +
            labs(x = paste("Korea quantiles:", lab),
                 y = paste("Taiwan quantiles:", lab)) +
            base
        },

        md = {
          qd <- two_sample_q(d)
          ggplot(qd, aes((kr + tw) / 2, tw - kr)) +
            geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
            geom_point(colour = "grey20") +
            labs(x = "Mean of paired quantiles",
                 y = "Taiwan minus Korea") +
            base
        }
      )
    })

    # ---- 3) 요약 통계 ----
    output$summary_tbl <- renderTable({
      d <- plot_data()
      validate(need(nrow(d) > 0, "No data for this selection."))
      date_fmt <- if (input$freq == "monthly") "%Y-%m" else "%Y-%m-%d"

      d |>
        group_by(Country = country) |>
        summarise(
          N           = n(),
          Mean        = mean(value),
          SD          = sd(value),
          Median      = median(value),
          IQR         = IQR(value),
          Min         = min(value),
          Max         = max(value),
          `Peak date` = format(date[which.max(value)], date_fmt),
          .groups = "drop"
        )
    }, digits = 2)
  })
}
