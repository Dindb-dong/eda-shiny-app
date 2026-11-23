# =============================================================================
# R/tab_covid.R  —  Tab 01 · COVID-19 Timeline (한국 vs 대만)
#
# Shiny "모듈" 구조:
#   covidTabUI(id)                  -> 화면(위젯, 그림 자리)
#   covidTabServer(id, covid_daily) -> 위젯 값을 받아 그림·표를 만듦
# app.R에서 이 두 함수를 한 번씩 불러 쓰면 된다.
# (covid_daily 는 R/covid_data.R 의 load_covid_daily() 결과)
# 팀원 탭도 같은 방식(R/tab_spending.R 등)으로 만들면 서로 코드가 안 꼬인다.
# =============================================================================

library(shiny)
library(dplyr)
library(ggplot2)

# ---- 공통 설정 (다른 탭에서도 같은 색을 쓰면 발표 때 보기 좋음) --------------
country_colors <- c("South Korea" = "#2E6FBA", "Taiwan" = "#D1495B")

covid_metrics <- c(
  "New cases"                          = "new_cases",
  "New deaths"                         = "new_deaths",
  "Stringency Index (0-100)"           = "stringency",
  "At least one dose (% of population)" = "vax_one",
  "Fully vaccinated (% of population)" = "vax_full",
  "Booster doses per 100 people"       = "boosters",
  "Reproduction rate (R)"              = "reproduction_rate"
)
count_metrics <- c("new_cases", "new_deaths")   # 합계를 내는 변수

# 두 나라의 같은 분위수끼리 짝짓기 (two-sample Q-Q, Tukey mean-difference용)
two_sample_q <- function(d, probs = seq(0.01, 0.99, by = 0.01)) {
  validate(need(all(c("South Korea", "Taiwan") %in% d$country),
                "Select both countries to compare quantiles."))
  data.frame(
    p  = probs,
    kr = quantile(d$value[d$country == "South Korea"], probs, names = FALSE),
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
                         choices  = names(country_colors),
                         selected = names(country_colors)),
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
        "Per-million values adjust for South Korea's population being about twice Taiwan's."
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
      is_count <- input$metric %in% count_metrics
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
      req(input$dates)

      var <- input$metric
      if (var %in% count_metrics && input$per_million) var <- paste0(var, "_pm")

      d <- covid_daily |>
        filter(country %in% input$countries,
               date >= input$dates[1], date <= input$dates[2]) |>
        transmute(country, date, value = .data[[var]]) |>
        arrange(country, date)

      if (input$freq == "ma7") {
        d <- d |>
          group_by(country) |>
          mutate(value = as.numeric(stats::filter(value, rep(1 / 7, 7), sides = 1))) |>
          ungroup()
      } else if (input$freq == "monthly") {
        agg <- if (input$metric %in% count_metrics) {
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
        scale_colour_manual(values = country_colors) +
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
          scale_fill_manual(values = country_colors) +
          scale_colour_manual(values = country_colors) +
          labs(x = lab, y = "Density") +
          base + theme(legend.position = "none"),

        box = ggplot(d, aes(country, value, fill = country)) +
          geom_boxplot(alpha = 0.6) +
          scale_fill_manual(values = country_colors) +
          labs(x = NULL, y = lab) +
          base + theme(legend.position = "none"),

        ecdf = ggplot(d, aes(value, colour = country)) +
          stat_ecdf(linewidth = 0.8) +
          scale_colour_manual(values = country_colors) +
          labs(x = lab, y = "Cumulative proportion", colour = NULL) +
          base + theme(legend.position = "top"),

        qq = {
          qd <- two_sample_q(d)
          ggplot(qd, aes(kr, tw)) +
            geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
            geom_point(colour = "grey20") +
            labs(x = paste("South Korea quantiles:", lab),
                 y = paste("Taiwan quantiles:", lab)) +
            base
        },

        md = {
          qd <- two_sample_q(d)
          ggplot(qd, aes((kr + tw) / 2, tw - kr)) +
            geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
            geom_point(colour = "grey20") +
            labs(x = "Mean of paired quantiles",
                 y = "Taiwan minus South Korea") +
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
