# COVID-19 timeline, distribution and summary module.

covid_metrics <- c(
  "New cases"                          = "new_cases",
  "New deaths"                         = "new_deaths",
  "Stringency Index (0-100)"           = "stringency",
  "At least one dose (%)" = "owid_people_vaccinated_per_hundred",
  "Fully vaccinated (%)" = "owid_people_fully_vaccinated_per_hundred",
  "Booster doses per 100 people"       = "owid_total_boosters_per_hundred",
  "Reproduction rate (R)"              = "owid_reproduction_rate"
)
count_metrics <- c(new_cases = "jhu_new_confirmed", new_deaths = "jhu_new_deaths")
rate_metrics <- c(new_cases = "cases_pm", new_deaths = "deaths_pm")
# 누적 변수: 월별은 월말 값을 쓰고, 7일 평균과 정점 날짜는 제공하지 않음
cumulative_metrics <- c("owid_people_vaccinated_per_hundred",
                        "owid_people_fully_vaccinated_per_hundred",
                        "owid_total_boosters_per_hundred")
covid_freq_choices <- c("Daily" = "daily", "7-day average" = "ma7", "Monthly" = "monthly")

# 변수를 고르면 보여 줄 정의와 주의점
covid_metric_notes <- c(
  new_cases = "Newly reported confirmed cases each day. Counts depend on testing and reporting practices.",
  new_deaths = "Newly reported COVID-19 deaths each day. Reporting practices differ, so compare countries with care.",
  stringency = "Index (0–100) of how strict government responses were. It does not measure compliance or effect.",
  owid_people_vaccinated_per_hundred = "Share of people with at least one vaccine dose. A cumulative measure; some days are not reported.",
  owid_people_fully_vaccinated_per_hundred = "Share of people who completed the initial vaccine course. A cumulative measure; some days are not reported.",
  owid_total_boosters_per_hundred = "Cumulative booster doses per 100 people. It counts doses, not people.",
  owid_reproduction_rate = "Estimated average number of people infected by one case. Above 1 suggests spread, below 1 decline. Early estimates are unstable."
)

# 한 달의 모든 날짜에 값이 있을 때만 합계를 냄 (아니면 NA). 결합 분석의 월별 집계와 같은 기준
complete_month_total <- function(value, date) {
  first <- as.Date(format(date[1], "%Y-%m-01"))
  expected <- as.integer(seq(first, by = "month", length.out = 2)[2] - first)
  if (sum(is.finite(value)) != expected) NA_real_ else sum(value)
}

# 그 달에 마지막으로 보고된 값 (누적 변수용)
latest_value <- function(value, date) {
  ok <- is.finite(value)
  if (!any(ok)) NA_real_ else value[ok][which.max(date[ok])]
}

# 평균과 중앙값의 위치로 치우침 방향을 설명
skew_note <- function(d) {
  parts <- vapply(split(d$value, d$country, drop = TRUE), function(x) {
    difference <- mean(x) - median(x)
    if (isTRUE(all.equal(mean(x), median(x)))) "mean and median are about equal"
    else if (difference > 0) "mean above median, suggesting a longer upper tail (right skew)"
    else "mean below median, suggesting a longer lower tail (left skew)"
  }, character(1))
  paste0(paste0(names(parts), ": ", parts, ".", collapse = " "))
}

wrap_label <- function(x, width = 55) paste(strwrap(x, width), collapse = "\n")

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

  tagList(
    tags$div(class = "filter-grid main-filters covid-filters",
      checkboxGroupInput(ns("countries"), "Country",
                         choices = unname(countries), selected = unname(countries), inline = TRUE),
      selectInput(ns("metric"), "Variable", choices = covid_metrics,
                  selected = covid_metrics[["Fully vaccinated (%)"]], selectize = FALSE),
      selectInput(ns("freq"), "Time resolution",
                  # 기본 변수가 누적 변수라 7일 평균 없이 시작
                  choices = covid_freq_choices[covid_freq_choices != "ma7"],
                  selectize = FALSE),
      htmltools::tagAppendAttributes(
        dateRangeInput(ns("dates"), "Period", start = "2020-01-01", end = "2022-12-31",
                       min = "2020-01-01", max = "2022-12-31"),
        class = "covid-date-range")),
    tags$div(class = "filter-grid analysis-options",
      conditionalPanel("['new_cases', 'new_deaths'].includes(input.metric)", ns = ns,
        checkboxInput(ns("per_million"), "Per million people", value = TRUE)),
      checkboxInput(ns("log"), "Log scale: log10(x + 1)", value = FALSE),
      checkboxInput(ns("facet"), "Separate panel for each country", value = FALSE)),
    tags$section(class = "analysis-section", "aria-labelledby" = ns("chart-title"),
      tags$h2(id = ns("chart-title"), textOutput(ns("title"), inline = TRUE)),
      tags$p(class = "chart-note", textOutput(ns("definition"), inline = TRUE))),
    tabsetPanel(id = ns("view"),
      tabPanel("Timeline",
        plotOutput(ns("ts_plot"), height = "430px")),
      tabPanel("Distribution",
        tags$div(class = "filter-grid main-filters",
          selectInput(ns("dist_type"), "Plot type",
                      choices = c("Histogram + density + rug" = "hist",
                                  "Boxplot" = "box", "ECDF" = "ecdf",
                                  "Two-sample Q-Q plot" = "qq", "Tukey mean-difference plot" = "md"),
                      selectize = FALSE)),
        plotOutput(ns("dist_plot"), height = "430px"),
        conditionalPanel("input.dist_type === 'qq'", ns = ns,
          tags$p(class = "chart-note",
            "Q-Q: points on the dashed line indicate matching distributions. A parallel shift indicates a level difference; a different slope indicates a spread difference.")),
        tags$p(class = "chart-note",
          "Observations over time are serially dependent, so read these plots descriptively.")),
      tabPanel("Summary",
        tags$div(class = "table-scroll", tabindex = "0", role = "region",
                 "aria-label" = "Scrollable exploratory COVID-19 summary table",
                 tableOutput(ns("summary_tbl"))),
        tags$p(class = "chart-note", textOutput(ns("summary_note"), inline = TRUE)))),
    tags$details(class = "methods",
      tags$summary("Data and interpretation"),
      tags$p("Vaccination data come from Our World in Data. The reproduction rate (R) is from TrackingR via Our World in Data."),
      tags$p("Cases and deaths come from Johns Hopkins University CSSE. Daily counts retain reporting corrections. The Stringency Index is from the Oxford COVID-19 Government Response Tracker via Our World in Data."),
      tags$p("Per-million rates use the same fixed 2021 populations as Cases and deaths. Seven-day averages use the current day and the previous six calendar days. Monthly case and death totals appear only for months with a report on every day inside the selected period, so partial months are left out. Monthly vaccination measures show the latest value reported in each month within the selected period, and have no 7-day average. Monthly stringency and R average the available days. Cases and deaths start on 23 January 2020 and vaccination measures start in 2021."))
  )
}

# ---- Server -----------------------------------------------------------------
covidTabServer <- function(id, covid_daily) {
  moduleServer(id, function(input, output, session) {

    is_cumulative <- reactive(req(input$metric) %in% cumulative_metrics)

    # 화면 갱신 전에도 누적 변수에는 7일 평균이 적용되지 않도록 함
    freq <- reactive(if (is_cumulative() && req(input$freq) == "ma7") "daily" else req(input$freq))

    # 변수에 따라 시간 단위 선택지를 바꿈
    observeEvent(input$metric, {
      choices <- covid_freq_choices
      if (is_cumulative()) choices <- choices[choices != "ma7"]
      selected <- if (isTRUE(input$freq %in% choices)) input$freq else "daily"
      updateSelectInput(session, "freq", choices = choices, selected = selected)
    })

    # 축 이름: 선택한 옵션에 맞게 자동으로 바뀜
    metric_label <- reactive({
      lab <- names(covid_metrics)[covid_metrics == input$metric]
      is_count <- input$metric %in% names(count_metrics)
      if (is_count && input$per_million) lab <- paste(lab, "per million")
      if (freq() == "ma7") lab <- paste(lab, "(7-day avg)")
      if (freq() == "monthly") {
        lab <- paste(lab, if (is_count) "(monthly total)"
                     else if (is_cumulative()) "(latest in month)" else "(monthly mean)")
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

      precomputed <- freq() == "ma7" && is_count && isTRUE(input$per_million)
      if (precomputed) var <- sub("_pm$", "_7day", var)

      d <- covid_daily |>
        filter(country %in% input$countries) |>
        transmute(country, date, value = .data[[var]])

      if (freq() == "ma7" && !precomputed) {
        d <- d |>
          group_by(country) |>
          mutate(value = trailing_mean(value, date)) |>
          ungroup()
      }
      d <- d |> filter(date >= dates[1], date <= dates[2])

      if (freq() == "monthly") {
        agg <- if (is_count) {
          complete_month_total
        } else if (is_cumulative()) {
          latest_value
        } else {
          function(value, date) mean(value, na.rm = TRUE)
        }
        d <- d |>
          mutate(month = as.Date(format(date, "%Y-%m-01"))) |>
          group_by(country, month) |>
          summarise(value = agg(value, date), .groups = "drop") |>
          rename(date = month)
      }

      if (input$log) d <- d |> mutate(value = log10(value + 1))

      d |> filter(is.finite(value))   # NA/NaN 제거
    })

    output$title <- renderText(metric_label())
    output$definition <- renderText(covid_metric_notes[[req(input$metric)]])

    no_data <- reactive({
      if (freq() == "monthly" && input$metric %in% names(count_metrics)) {
        "No complete month in this selection. Monthly totals need a report on every day of the month."
      } else "No data for this selection."
    })

    # ---- 1) 시계열 ----
    output$ts_plot <- renderPlot({
      d <- plot_data()
      validate(need(nrow(d) > 0, no_data()))

      narrow <- session$clientData$output_ts_plot_width < 520
      p <- ggplot(d, aes(date, value, colour = country, linetype = country)) +
        covid_country_scales() + covid_date_axis(d$date, narrow = isTRUE(narrow)) +
        labs(x = NULL, y = metric_label(), colour = NULL) +
        chart_theme + theme(legend.position = "top")

      # R = 1 기준선 (로그 변환 시에는 같은 변환을 적용한 위치)
      if (input$metric == "owid_reproduction_rate") {
        p <- p + geom_hline(yintercept = if (input$log) log10(2) else 1,
                            linetype = "dotted", colour = "grey35")
      }
      if (any(table(d$country) > 1L)) p <- p + geom_line(linewidth = 0.7)
      if (freq() == "monthly" || any(table(d$country) == 1L)) p <- p + geom_point(size = 2)
      if (input$facet) p <- p + facet_wrap(~ country, ncol = 1, scales = "free_y")
      p
    })

    # ---- 2) 분포 비교 ----
    output$dist_plot <- renderPlot({
      d <- plot_data()
      validate(need(nrow(d) > 2, "Not enough observations for this selection."))
      lab  <- metric_label()
      base <- chart_theme

      switch(
        input$dist_type,

        hist = ggplot(d, aes(value, fill = country)) +
          geom_histogram(aes(y = after_stat(density)),
                         bins = 30, alpha = 0.5, colour = "white") +
          geom_density(aes(colour = country), fill = NA, linewidth = 0.8) +
          geom_rug(aes(colour = country), alpha = 0.4) +
          facet_wrap(~ country, ncol = 1) +
          scale_fill_manual(values = covid_country_colors) +
          scale_colour_manual(values = covid_country_colors) +
          labs(x = lab, y = "Density") +
          base + theme(legend.position = "none"),

        box = ggplot(d, aes(country, value, fill = country)) +
          geom_boxplot(alpha = 0.6) +
          scale_fill_manual(values = covid_country_colors) +
          labs(x = NULL, y = lab) +
          base + theme(legend.position = "none"),

        ecdf = ggplot(d, aes(value, colour = country)) +
          stat_ecdf(linewidth = 0.8) +
          scale_colour_manual(values = covid_country_colors) +
          labs(x = lab, y = "Cumulative proportion", colour = NULL) +
          base + theme(legend.position = "top"),

        qq = {
          qd <- two_sample_q(d)
          ggplot(qd, aes(kr, tw)) +
            geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
            geom_point(colour = "grey20") +
            labs(x = wrap_label(paste("Korea quantiles:", lab)),
                 y = wrap_label(paste("Taiwan quantiles:", lab), 40)) +
            base
        },

        md = {
          qd <- two_sample_q(d)
          ggplot(qd, aes((kr + tw) / 2, tw - kr)) +
            geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
            geom_point(colour = "grey20") +
            labs(x = wrap_label(paste("Mean of paired quantiles:", lab)),
                 y = wrap_label(paste("Taiwan minus Korea:", lab), 40)) +
            base
        }
      )
    })

    # ---- 3) 요약 통계 ----
    output$summary_tbl <- renderTable({
      d <- plot_data()
      validate(need(nrow(d) > 0, no_data()))
      date_fmt <- if (freq() == "monthly") "%Y-%m" else "%Y-%m-%d"

      table <- d |>
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
          Latest      = value[which.max(date)],
          `Latest date` = format(max(date), date_fmt),
          .groups = "drop"
        )
      # 누적 변수는 정점 날짜 대신 최신값을, 나머지는 정점 날짜를 보여 줌
      if (is_cumulative()) select(table, -`Peak date`) else select(table, -Latest, -`Latest date`)
    }, digits = 2)

    output$summary_note <- renderText({
      d <- plot_data()
      validate(need(nrow(d) > 0, ""))
      if (is_cumulative()) {
        "Cumulative measure: focus on the latest value. The mean and median depend on the selected period."
      } else skew_note(d)
    })
  })
}
