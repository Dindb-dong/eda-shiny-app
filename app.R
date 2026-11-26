# COVID-19 and card spending: South Korea vs Taiwan

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)

source("src/analysis.R", local = TRUE)
source("src/plots.R", local = TRUE)
source("src/covid_data.R", local = TRUE)
source("src/covid_section.R", local = TRUE)
source("src/covid_exploration.R", local = TRUE)
source("src/card_section.R", local = TRUE)
source("src/findings.R", local = TRUE)

industry <- read_industry_crosswalk()
categories <- setNames(industry$label_en, industry$category)
spending <- read_spending(industry = industry)
covid_daily <- read_covid_daily()
covid <- summarize_covid_monthly(covid_daily)
covid_area <- covid_by_area(covid, read_zone_cases(), read_distancing_levels())
age_profile <- read_age_profile(industry = industry)
card_comparison <- read_card_comparison()
boundaries <- read_region_boundaries()
unit_spending <- read_unit_spending(spending, industry = industry)
findings <- build_findings(spending, covid, categories, unit_spending)
map_months <- seq(as.Date("2020-01-01"), as.Date("2022-12-01"), by = "month")

overview_views <- c(industry_growth = "Spending growth by area", industry_gap = "Capital–non-capital gap")
map_views <- c(industry_map = "Regional spending map")
relationship_views <- c(scatter = "Monthly scatter", aligned = "Aligned timeline",
                        lags = "Lag profile", adjusted = "Detrended and seasonally adjusted scatter",
                        conditioning = "Condition on restriction stringency",
                        outcomes = "Spending outcomes by restriction band", binary = "Logistic curve for spending declines")
spending_views <- c(timeline = "Monthly timeline", heatmap = "Calendar heatmap",
                    distribution = "Empirical distribution (ECDF)", box = "Pre/post boxplots",
                    projection = "Pre-COVID trend projection")
# Appendix: forecasting, break models and diagnostics, kept for method checks.
diagnostic_views <- c(backtest = "2019 forecast backtest",
                      forecast = "Pre-COVID forecast comparison", break_model = "Segmented trend with AR(1) errors",
                      stl = "Robust seasonal decomposition (STL)", acf = "Autocorrelation (ACF)",
                      pacf = "Partial autocorrelation (PACF)", qq = "Normal Q–Q plot",
                      quantiles = "Before vs. since onset Q–Q", quantile_diff = "Quantile mean–difference",
                      spread = "Annual location–spread", boxcox = "Box–Cox profile",
                      residuals = "Adjusted model residuals", fitted_resid = "Residuals vs. fitted values",
                      cook = "Influential months")
age_views <- c(age_observed = "Observed mean ticket by age and month",
               age_compare = "Ordinary and robust bivariate LOESS",
               age_residual = "Robust LOESS residual map")
view_names <- c(overview_views, map_views, relationship_views, spending_views, diagnostic_views, age_views)
# Chart headings say what to look for; the selector keeps the short method names.
view_titles <- c(
  industry_growth = "Which industries fell or grew, month by month",
  industry_gap = "Where capital-area growth ran ahead of the rest of the country",
  scatter = "Does spending move with the COVID-19 measure?",
  lags = "Does the COVID-19 measure lead spending by a few months?",
  adjusted = "Does the link survive once trend and season are removed?",
  timeline = "How spending moved before and since onset",
  projection = "How far spending departed from its pre-COVID trend",
  forecast = "How far spending departed from pre-COVID forecasts")
paired_diagnostics <- c("residuals", "cook", "fitted_resid")
fixed_stringency <- c("outcomes", "binary")
forecast_views <- c("forecast", "backtest")
model_views <- c(forecast_views, "break_model")
choices <- function(labels) setNames(names(labels), unname(labels))
spending_choices <- c("Year-over-year growth" = "yoy", "2019 index" = "index",
                      "12-month log difference" = "seasonal", "Pre-COVID spending gap" = "gap",
                      "Month-over-month growth" = "monthly", "First + seasonal difference" = "double_diff")

chart_notes <- c(
  industry_growth = "Each cell shows spending growth from the same month a year earlier. Orange indicates a decline and teal indicates growth. Colours stop at ±50%; larger values share the end colour. Grey Taiwan cells (Feb 2021 – Jan 2022) are masked because the source’s category mix shifts in Feb 2021.",
  industry_map = "Fill shows spending growth from the same month a year earlier for each Korean province and Taiwanese city or county, or for each capital and non-capital zone. Labels give the zone values and the capital zone has a heavier outline. Sejong is white because ECOS publishes no separate Sejong series. In March 2020 Daegu and North Gyeongsang, the centre of Korea’s first outbreak, fell most; the app has no province-level case or restriction data, so this is an association. The colour scale is fixed at ±50%, so months can be compared while the slider plays and small counties with wild swings share the end colour. Grey Taiwan areas (Feb 2021 – Jan 2022) are masked because the source’s category mix shifts in Feb 2021. Matsu (Lienchiang County, non-capital) has spending data but no boundary polygon.",
  industry_gap = "Each cell subtracts non-capital growth from capital-area growth in the same industry and month. Teal means growth was higher in the capital area. Colours stop at ±50 points. Grey Taiwan cells (Feb 2021 – Jan 2022) are masked because the source’s category mix shifts in Feb 2021.",
  scatter = "Each point is one spending month. The dashed line is linear. The purple line is a local linear fit (LOESS).",
  aligned = "Both series are standardized within their own paired window. The horizontal axis follows the spending month.",
  lags = "Each point uses a separate calendar-month join. Counts fall at longer leads as the matched window gets shorter.",
  adjusted = "Calendar trend and one annual harmonic are removed from both axes before fitting a line.",
  conditioning = "Bands use a fixed 50-point threshold in the COVID-19 month. Select cases or deaths to explore the pattern within restriction bands.",
  outcomes = "Counts use year-over-year growth below −5%, between −5% and 5%, and above 5%. The 50-point stringency cutoff is fixed across panels.",
  binary = "Dots show whether spending fell from the same month last year. The logistic curve is fitted only when both outcomes have at least four observations and overlapping stringency ranges.",
  timeline = "The dotted line marks January 2020, the common period boundary. Taiwan’s 2019 index and pre-COVID gap are level measures and still contain the February 2021 category shift.",
  heatmap = "Orange is below the reference and teal is above it. The reference is 100 for the spending index or zero for changes.",
  distribution = "The curves show the fraction of months at or below each value. Rugs show observed months.",
  projection = "The dashed line extends a pre-2020 log-linear trend with calendar-month effects. This view uses the 2019 spending index, so Taiwan’s February 2021 category shift appears as a step.",
  box = "Boxes summarize the median and central half of observed monthly values in each period.",
  backtest = "Models fit through December 2018 and forecast the 12 unseen months of 2019. MAE and RMSE below are in index points.",
  forecast = "Models fit through December 2019 and forecast 2020–2022. The summary scores the earlier 2019 holdout. Lines are point forecasts on the back-transformed log scale.",
  break_model = "The model estimates a level and monthly slope change at January 2020, with calendar-month effects and AR(1) errors. The dashed reference extends the fitted pre-break segment.",
  stl = "Robust STL splits the full monthly log-spending series into trend, a repeating annual pattern, and remainder.",
  acf = "Bars summarize how the spending measure is correlated with its own past over the full monthly series.",
  pacf = "Partial autocorrelation isolates each lag after accounting for intervening lags.",
  quantiles = "Matching empirical quantiles in the two periods. The diagonal represents equal quantiles.",
  quantile_diff = "The vertical axis is since-onset minus pre-onset at the same empirical quantile. The horizontal axis is their average.",
  spread = "Each point gives a calendar year’s median and IQR for the selected measure.",
  qq = "The points compare each period’s observed spending quantiles with a normal reference.",
  boxcox = "Compares powers of the positive 2019 index after accounting for trend and calendar month. The peak gives the best power on the plotted grid.",
  fitted_resid = "Check whether the model leaves a curved pattern or a changing spread of errors across fitted values.",
  age_observed = "Average card transaction amount by month and age-band midpoint. Open-ended age groups are excluded.",
  age_compare = "These local linear surfaces smooth log mean ticket by month and age with equal cell weights. The robust fit downweights unusual cells.",
  age_residual = "Observed minus robust fitted mean ticket on the log scale. Both fits are evaluated only at observed age-month cells.",
  residuals = "Residuals from spending regressed on COVID-19, a linear trend, and annual sine and cosine terms.",
  cook = "Cook’s distance flags months that can strongly influence the adjusted linear model."
)

uses_covid_options <- "input.tab === 'relationship' || (input.tab === 'diagnostics' && ['residuals','cook','fitted_resid'].includes(input.diagnostic_view))"

relationship_ui <- tagList(
  tabsetPanel(id = "tab", selected = "findings",
    tabPanel("Key findings", value = "findings", findings_ui()),
    tabPanel("Industry overview", value = "overview"),
    tabPanel("Relationship", value = "relationship"),
    tabPanel("Before and since onset", value = "spending"),
    tabPanel("Taiwan age profile", value = "age"),
    tabPanel("Appendix", value = "diagnostics")),
  conditionalPanel("input.tab !== 'findings'",
    tags$div(class = "filter-grid main-filters",
      conditionalPanel("input.tab !== 'age'",
        selectInput("country", "Country", c("Korea and Taiwan" = "both", "Korea", "Taiwan"), selectize = FALSE)),
      conditionalPanel("!['age','overview'].includes(input.tab)",
        selectInput("area", "Geography", c(choices(areas), "Capital and non-capital areas" = "both"), selectize = FALSE)),
      conditionalPanel("input.tab !== 'overview' || input.overview_view === 'industry_map'",
        selectInput("category", "Card category", choices(categories), selectize = FALSE)),
      conditionalPanel("!['age','overview'].includes(input.tab)",
        selectInput("measure", "Spending measure", spending_choices, selectize = FALSE)),
      conditionalPanel("input.tab === 'overview'",
        selectInput("overview_view", "View",
                    c("Growth by area" = "industry_growth", "Capital area gap" = "industry_gap",
                      "Regional map" = "industry_map"),
                    selectize = FALSE)),
      conditionalPanel("input.tab === 'overview' && input.overview_view !== 'industry_map'",
        selectInput("overview_period", "Spending period",
                    c("2020–2022" = "since_covid_onset", "2015–2019" = "pre_covid", "2015–2022" = "all"),
                    selectize = FALSE))),
    tags$div(class = "filter-grid analysis-options",
      conditionalPanel("input.tab === 'overview' && input.overview_view === 'industry_map'", class = "sensitivity-option",
        # Date slider steps are in days; 31-day steps from 1 Jan 2020 land in each of the 36 months once.
        selectInput("map_fill", "Map fill", c("Provinces, cities and counties" = "unit",
                                              "Capital and non-capital zones" = "zone"), selectize = FALSE),
        sliderInput("map_month", "Month (press play to step through 2020–2022)", min = min(map_months),
                    max = max(map_months), value = as.Date("2020-03-01"), step = 31, timeFormat = "%b %Y",
                    ticks = FALSE, width = "100%", animate = animationOptions(interval = 1200))),
      conditionalPanel("input.tab === 'relationship'",
        selectInput("relationship_view", "View", choices(relationship_views), selectize = FALSE)),
      conditionalPanel("input.tab === 'spending'",
        selectInput("spending_view", "View", choices(spending_views), selectize = FALSE)),
      conditionalPanel("input.tab === 'spending'",
        selectInput("period", "Spending period", c("Both periods" = "all", "Before onset · 2014–2019" = "pre_covid",
                    "Since onset · 2020–2022" = "since_covid_onset"), selectize = FALSE)),
      conditionalPanel("input.tab === 'diagnostics'",
        selectInput("diagnostic_view", "View", choices(diagnostic_views), selectize = FALSE)),
      conditionalPanel("input.tab === 'age'",
        selectInput("age_view", "View", choices(age_views), selectize = FALSE)),
      conditionalPanel(paste0("(", uses_covid_options, ") && (input.tab !== 'relationship' || !['outcomes','binary'].includes(input.relationship_view))"),
        selectInput("metric", "COVID-19 measure", choices(covid_measures), selected = "stringency", selectize = FALSE)),
      conditionalPanel(paste0("(", uses_covid_options, ") && (input.tab !== 'relationship' || input.relationship_view !== 'lags')"),
        selectInput("lead", "COVID-19 leads spending", c("Same month" = 0, "1 month" = 1, "2 months" = 2, "3 months" = 3),
                    selectize = FALSE))),
    tags$section(class = "analysis-section", "aria-labelledby" = "chart-title",
      tags$h2(id = "chart-title", textOutput("title", inline = TRUE)),
      tags$p(class = "chart-note", textOutput("note", inline = TRUE)),
      plotOutput("chart", height = "auto"),
      tags$p(class = "coverage", textOutput("coverage", inline = TRUE))),
    conditionalPanel("input.tab === 'overview' && input.overview_view !== 'industry_map'",
      tags$section(class = "analysis-section", "aria-labelledby" = "industry-detail-title",
        tags$h2(id = "industry-detail-title", "Explore an industry"),
        tags$div(class = "filter-grid analysis-options industry-detail-controls",
          selectInput("detail_category", "Industry for detail", choices(categories), selectize = FALSE),
          selectInput("detail_view", "Detail view", c("COVID-19 scatterplot" = "relationship",
                                                      "Spending timeline" = "spending"), selectize = FALSE),
          actionButton("open_industry_detail", "Open detail", class = "industry-detail-button")))),
    tags$section(class = "analysis-section", "aria-labelledby" = "summary-title",
      tags$div(class = "section-header",
        tags$h2(id = "summary-title", textOutput("summary_title", inline = TRUE)),
        downloadLink("download", "Download CSV")),
      tags$div(class = "table-scroll", tabindex = "0", role = "region", "aria-label" = "Scrollable summary table",
               tableOutput("summary")),
      conditionalPanel("input.tab === 'relationship'",
        tags$p(class = "coverage", "Pearson and Spearman describe the chosen pairs. Adjusted Pearson uses the two detrended series. Decline counts refer to negative year-over-year growth."))),
    tags$details(class = "methods",
      tags$summary("Data and interpretation"),
      tags$p("Cases and deaths are daily reports summed by month. Negative reporting adjustments are retained. Stringency is averaged by month. Count plots use an inverse hyperbolic sine scale, which retains zeros."),
      tags$p("The nationwide spending totals include unassigned amounts. Capital and non-capital series exclude them. Korea uses Seoul, Incheon, and Gyeonggi. Taiwan uses Taipei, New Taipei, and Keelung."),
      tags$p("Spending amounts are nominal and source coverage varies by country and category. The 2019 index uses each series’ own 2019 average. The same-month median uses 2014–2019. Growth and differences are calculated before filtering by period."),
      tags$p("Taiwan’s NCCC category mix shifts abruptly in February 2021: food falls from about 38.7 to 18.7 billion TWD between January and February while general retail rises from 29.2 to 45.9 billion. The step happens at once in every city and county, unlike a consumption shift or a Lunar New Year effect. The source has published no notice of a classification change at that date, but the pattern is what a change in merchant-to-industry mapping would produce, so every Taiwan change measure whose two months straddle the step is left missing: year-over-year growth and the 12-month log difference for February 2021 – January 2022, month-over-month growth for February 2021, and the combined difference wherever an input is missing. The 2019 index and the pre-COVID gap are level measures that cannot be repaired, so read Taiwan’s levels across February 2021 with caution."),
      tags$p("Industry categories use each source’s definitions. Lodging, vehicles and source-defined Other are best read within each country. National COVID-19 measures are shared by both spending areas."),
      tags$p("Appendix views fit forecasting, break and diagnostic models. They check the methods behind the main tabs rather than add findings."),
      tags$p("The joined series has at most 36 months (January 2020 – December 2022) per country and region; January 2020 has stringency but no complete case or death counts. Shared trends, extreme months, and serial dependence can drive the fitted patterns. These charts describe associations.")))
)

ui <- fluidPage(
  lang = "en", title = "COVID-19 and card spending",
  tags$head(
    tags$link(rel = "stylesheet", href = "dashboard.css"),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', () => {
        Shiny.addCustomMessageHandler('focus-industry-detail-tab', (target) => {
          const tabs = Array.from(document.querySelectorAll('#tab > li > a'));
          tabs.find(tab => tab.dataset.value === target)?.focus();
        });
      });
    "))
  ),
  tags$main(
    class = "dashboard",
    tags$header(class = "masthead",
      tags$p(class = "course", "STA3115 · Exploratory Data Analysis"),
      tags$h1("COVID-19 and card spending")),
    htmltools::tagAppendAttributes(
      navlistPanel(id = "section", selected = "joint", widths = c(3, 9), well = FALSE,
        tabPanel("COVID-19 comparison", value = "covid",
          tabsetPanel(id = "covid_views",
            tabPanel("Cases and deaths", value = "reports", covid_section_ui("covid_daily")),
            tabPanel("Explore metrics", value = "explore", covidTabUI("covid_timeline")))),
        tabPanel("Card spending comparison", value = "cards",
          card_section_ui("card_comparison", c(card_comparison$start, card_comparison$end))),
        tabPanel("COVID-19 ↔ Card spending", value = "joint", relationship_ui)
      ),
      class = "section-navigation"
    )
  )
)

server <- function(input, output, session) {
  covid_section_server("covid_daily", covid_daily)
  card_section_server("card_comparison", card_comparison)

  findings_server(input, output, findings)

  active_view <- reactive(switch(req(input$tab),
    findings = req(FALSE),
    overview = req(input$overview_view),
    relationship = req(input$relationship_view), spending = req(input$spending_view),
    diagnostics = req(input$diagnostic_view), age = req(input$age_view)))

  selected_countries <- reactive(if (req(input$country) == "both") unname(countries) else input$country)
  overview_data <- reactive(industry_months(spending, selected_countries(), req(input$overview_period),
                                            req(input$overview_view)))
  map_month <- reactive({
    month <- format(as.Date(req(input$map_month)), "%Y%m")
    validate(need(month %in% format(map_months, "%Y%m"), "Choose a month from January 2020 to December 2022."))
    as.integer(month)
  })
  map_values <- reactive(zone_values(spending, covid_area, selected_countries(), req(input$category), map_month()))
  unit_values <- reactive({
    if (!identical(input$map_fill, "unit")) return(NULL)
    u <- unit_spending[unit_spending$country %in% selected_countries() & unit_spending$unit_code != "other" &
                         unit_spending$category == req(input$category) & unit_spending$month == map_month(),
                       c("country", "unit_code", "unit_name", "capital_area", "yoy")]
    u$published <- TRUE
    # ECOS publishes no separate Sejong series; show it unfilled rather than estimate it.
    if ("Korea" %in% u$country) {
      u <- rbind(u, data.frame(country = "Korea", unit_code = "KR-50", unit_name = "Sejong (not published separately)",
                               capital_area = "non_capital", yoy = NA_real_, published = FALSE))
    }
    u
  })
  # Zone rows plus any city/county rows, for the summary table and CSV.
  map_table <- reactive({
    v <- map_values()
    zones <- data.frame(level = "zone", country = v$country, area = v$region, unit_code = NA_character_,
                        zone = v$capital_area, yoy_pct = v$yoy)
    u <- unit_values()
    units <- if (is.null(u)) zones[0, ] else
      data.frame(level = "unit", country = u$country, area = u$unit_name, unit_code = u$unit_code,
                 zone = u$capital_area, yoy_pct = u$yoy)
    d <- rbind(zones, units)
    d$industry <- unname(categories[[req(input$category)]])
    d$month <- map_month()
    at <- match(paste(d$country, d$zone), paste(v$country, v$capital_area))
    d$stringency <- v$stringency[match(d$country, v$country)]
    d$zone_cases_per_million <- v$cases_per_million[at]
    d$zone_distancing_level <- v$level[at]
    d
  })
  unit_extremes <- function(u) {
    u <- u[is.finite(u$yoy), ]
    if (is.null(u) || !nrow(u)) return("")
    paste0(". ", paste(vapply(split(u, u$country), function(g) {
      hi <- g[which.max(g$yoy), ]; lo <- g[which.min(g$yoy), ]
      sprintf("%s %s range from %s %+.1f%% to %s %+.1f%%", g$country[1],
              if (g$country[1] == "Korea") "provinces" else "cities and counties", lo$unit_name, lo$yoy, hi$unit_name, hi$yoy)
    }, character(1)), collapse = "; "))
  }
  map_label <- function(month) format(as.Date(paste0(month, "01"), "%Y%m%d"), "%b %Y")
  overview_unit <- reactive(if (active_view() == "industry_gap") "Gap (percentage points)" else "Growth (%)")

  observeEvent(input$open_industry_detail, {
    target <- req(input$detail_view)
    updateSelectInput(session, "category", selected = req(input$detail_category))
    updateSelectInput(session, "area", selected = "both")
    updateSelectInput(session, "measure", selected = "yoy")
    if (target == "relationship") {
      updateSelectInput(session, "relationship_view", selected = "scatter")
    } else {
      updateSelectInput(session, "spending_view", selected = "timeline")
      updateSelectInput(session, "period", selected = req(input$overview_period))
    }
    updateTabsetPanel(session, "tab", selected = target)
    session$sendCustomMessage("focus-industry-detail-tab", target)
  })

  analysis_measure <- reactive({
    view <- active_view()
    if (view %in% c(model_views, "projection", "boxcox")) return("index")
    if (view %in% fixed_stringency) return("yoy")
    req(input$measure)
  })
  analysis_metric <- reactive(if (active_view() %in% fixed_stringency) "stringency" else req(input$metric))

  selection <- reactive({
    area <- if (req(input$area) == "both") c("capital", "non_capital") else input$area
    d <- spending[spending$country %in% selected_countries() & spending$capital_area %in% area &
                    spending$category == req(input$category), , drop = FALSE]
    if (identical(area, "national")) d$panel <- d$country
    d$value <- d[[analysis_measure()]]
    d
  })

  shown_spending <- reactive({
    d <- selection()
    if (req(input$tab) != "spending" || active_view() %in% model_views || req(input$period) == "all") d
    else subset(d, covid_period == input$period)
  })

  paired <- reactive({
    # Lag-profile charts calculate all leads; their summary and download use zero.
    lead <- if (active_view() == "lags") 0L else as.integer(req(input$lead))
    d <- pair_months(selection(), covid_area, analysis_metric(), lead)
    if (analysis_metric() == "level") {
      validate(need(nrow(d) > 0, "Distancing levels are available only for Korea's capital and non-capital areas, July 2020 – October 2021."))
    }
    d
  })
  holdout <- reactive(by_series(selection(), forecast_backtest))
  predicted <- reactive({
    if (active_view() == "backtest") holdout()
    else by_series(selection(), function(g) forecast_index(g, onset))
  })
  segmented <- reactive(by_series(selection(), segmented_trend))
  uses_pairs <- reactive(req(input$tab) == "relationship" || active_view() %in% paired_diagnostics)
  age_data <- reactive(smooth_age_profile(age_profile[age_profile$category == req(input$category), ]))

  layout <- reactive({
    if (req(input$tab) == "age") {
      return(list(columns = 1L, height = if (active_view() == "age_compare") 600 else 345))
    }
    width <- session$clientData$output_chart_width %||% 1000
    if (active_view() == "industry_map") return(list(columns = 1L, height = if (width >= 590) 520L else 440L))
    columns <- if (width >= 590) 2L else 1L
    if (active_view() %in% names(overview_views)) {
      rows <- ceiling(length(unique(overview_data()$panel)) / columns)
      return(list(columns = columns, height = 252L * rows + 65L))
    }
    groups <- length(unique(selection()$panel))
    plots <- if (active_view() == "stl") groups * 3L else groups
    rows <- ceiling(plots / columns)
    list(columns = columns, height = if (active_view() == "stl") max(525, rows * 155 + 45) else 300 * rows + 40)
  })

  output$title <- renderText(if (active_view() %in% names(view_titles)) view_titles[[active_view()]] else view_names[[active_view()]])
  output$note <- renderText(chart_notes[[active_view()]])
  output$summary_title <- renderText({
    if (active_view() == "industry_gap") "Summary by country and industry" else "Summary by country and area"
  })
  output$coverage <- renderText({
    view <- active_view()
    if (view == "industry_map") {
      v <- map_values()
      cases <- ifelse(is.finite(v$cases_per_million),
                      paste(trimws(formatC(round(v$cases_per_million), format = "d", big.mark = ",")), "cases per million"),
                      "no complete case count")
      level <- ifelse(is.finite(v$level), sprintf(", distancing level %s", format(round(v$level, 1))), "")
      zones <- sprintf("%s %s %s%s", v$country, tolower(v$region), cases, level)
      national <- v[!duplicated(v$country), ]
      return(paste0("COVID-19 context, ", map_label(map_month()), " · ", paste(zones, collapse = " · "),
                    " · National stringency: ", paste(sprintf("%s %.0f", national$country, national$stringency), collapse = ", ")))
    }
    if (view %in% names(overview_views)) {
      d <- overview_data()
      return(paste(format(min(d$date), "%b %Y"), "–", format(max(d$date), "%b %Y"),
                   paste0("· ", length(categories), " industries · Monthly card spending")))
    }
    if (req(input$tab) == "age") return("Taiwan nationwide · Jan 2019 – Dec 2022 · 12 bounded age bands")
    if (view == "backtest") return("Jan–Dec 2019 · One fixed 12-month holdout")
    if (view == "forecast") return("Jan 2020 – Dec 2022 · Forecast gaps describe departures from the reference models.")
    if (uses_pairs()) {
      if (view == "lags") return("Summary and download below use the same-month pairs.")
      d <- paired()
      scope <- if (identical(input$area, "national")) "COVID-19 measures are national."
               else "Cases and Korea's distancing level are by area; deaths and stringency are national."
      return(paste("Paired monthly data,", format(min(d$date), "%b %Y"), "–", format(max(d$date), "%b %Y"), "·", scope))
    }
    d <- shown_spending()
    paste(format(min(d$date), "%b %Y"), "–", format(max(d$date), "%b %Y"), "· Monthly card spending")
  })
  output$chart <- renderPlot({
    columns <- layout()$columns
    view <- active_view()
    if (view == "industry_map") return(zone_map(boundaries, map_values(), "Growth (%)", unit_values()))
    if (view %in% names(overview_views)) return(industry_heatmap(overview_data(), categories, overview_unit(), columns))
    if (req(input$tab) == "age") return(age_plot(age_data(), view))
    if (view %in% forecast_views) return(forecast_plot(predicted(), view, columns))
    if (view == "break_model") return(break_plot(segmented(), columns))
    switch(req(input$tab),
      relationship = relationship_plot(paired(), selection(), covid_area, view, analysis_metric(), analysis_measure(),
                                       columns),
      spending = spending_plot(shown_spending(), selection(), view, analysis_measure(), columns),
      diagnostics = diagnostic_plot(selection(), if (uses_pairs()) paired() else NULL,
                                    view, input$measure, columns))
  }, alt = function() {
    if (active_view() == "industry_map") {
      v <- map_values()
      values <- ifelse(is.finite(v$yoy), sprintf("%+.1f%%", v$yoy), "masked")
      return(paste0("Map of year-over-year ", tolower(categories[[input$category]]), " spending growth in ",
                    map_label(map_month()), " by zone: ",
                    paste(sprintf("%s %s %s", v$country, tolower(v$region), values), collapse = "; "),
                    unit_extremes(unit_values()), ". Values are also in the table below."))
    }
    if (active_view() %in% names(overview_views)) {
      return(paste("Heatmap of", view_names[[active_view()]], "for",
                   paste(selected_countries(), collapse = " and "),
                   "with industries in rows and months in columns. Exact values are available as CSV."))
    }
    area_names <- if (req(input$tab) == "age") "Taiwan nationwide"
                  else paste(unique(selection()$panel), collapse = " and ")
    paste(view_names[[active_view()]], "for", area_names)
  },
     height = function() layout()$height, execOnResize = TRUE)

  output$summary <- renderTable({
    view <- active_view()
    if (view == "industry_map") {
      d <- map_table()
      return(data.frame(Country = d$country, Level = ifelse(d$level == "zone", "Zone", "Province, city or county"),
                        Area = d$area, Industry = d$industry, Month = map_label(d$month), "YoY growth (%)" = d$yoy_pct,
                        "Stringency (national)" = d$stringency, "Cases per million (zone)" = d$zone_cases_per_million,
                        "Distancing level (Korea zone)" = d$zone_distancing_level,
                        check.names = FALSE))
    }
    if (view %in% names(overview_views)) {
      d <- overview_data()
      return(by_series(d, function(g) {
        valid <- is.finite(g$value)
        data.frame(Series = g$panel[1], Industry = unname(categories[g$category[1]]),
                   Months = sum(valid), Median = median(g$value[valid]), Unit = overview_unit())
      }, keys = c("panel", "category")))
    }
    if (req(input$tab) == "age") {
      d <- age_data()
      return(data.frame(Model = c("Ordinary LOESS", "Robust LOESS"), Cells = nrow(d),
                        "Median abs. error (TWD, in-sample)" = c(median(abs(d$observed - d$ordinary)),
                                                                 median(abs(d$observed - d$robust))),
                        check.names = FALSE))
    }
    if (view %in% forecast_views) return(forecast_accuracy(holdout()))
    if (view == "break_model") {
      return(by_series(segmented(), function(g) {
        data.frame(Series = g$panel[1], Months = nrow(g), "Level change (%)" = g$level_change[1],
                   "Slope change (log points/month ×100)" = g$monthly_slope_delta[1],
                   "AR(1)" = g$phi[1], check.names = FALSE)
      }))
    }
    if (uses_pairs()) {
      return(by_series(paired(), function(g) {
        adjusted <- adjust_association(g)
        data.frame(Series = g$panel[1], Months = nrow(g),
                   Pearson = safe_correlation(g$x, g$value, "pearson"),
                   Spearman = safe_correlation(g$x, g$value),
                   "Adjusted Pearson" = safe_correlation(adjusted$x_adjusted, adjusted$y_adjusted, "pearson"),
                   "Decline months" = sum(g$yoy < 0, na.rm = TRUE), check.names = FALSE)
      }))
    }
    d <- subset(shown_spending(), is.finite(value))
    by_series(d, function(g) {
      data.frame(Series = g$panel[1],
                 Period = if (g$covid_period[1] == "pre_covid") "Before onset" else "Since onset",
                 Months = nrow(g), Median = median(g$value), IQR = IQR(g$value),
                 Mean = mean(g$value), SD = sd(g$value))
    }, keys = c("panel", "covid_period"))
  }, digits = 2, spacing = "s", width = "100%", na = "—")

  output$download <- downloadHandler(
    filename = function() {
      suffix <- if (req(input$tab) == "overview") req(input$overview_view) else input$tab
      paste0("card-spending-", suffix, ".csv")
    },
    content = function(file) {
      view <- active_view()
      if (view == "industry_map") {
        write.csv(map_table(), file, row.names = FALSE, na = "")
        return(invisible(NULL))
      }
      if (view %in% names(overview_views)) {
        d <- overview_data()
        d$industry <- unname(categories[d$category])
        names(d)[names(d) == "value"] <- if (view == "industry_gap") "gap_pp" else "yoy_pct"
        write.csv(d, file, row.names = FALSE, na = "")
        return(invisible(NULL))
      }
      if (req(input$tab) == "age") {
        write.csv(age_data(), file, row.names = FALSE, na = "")
        return(invisible(NULL))
      }
      d <- if (view %in% forecast_views) predicted()
           else if (view == "break_model") segmented()
           else if (uses_pairs()) paired()
           else shown_spending()
      d$spending_measure <- analysis_measure()
      if (uses_pairs()) d$covid_measure <- analysis_metric()
      write.csv(d, file, row.names = FALSE, na = "")
    }
  )

  covidTabServer("covid_timeline", covid_daily)
}

shinyApp(ui, server)
