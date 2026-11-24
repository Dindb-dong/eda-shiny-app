source("app.R")

stopifnot(length(categories) == 7L, nrow(spending) == 4536L, nrow(covid) == 48L,
          all(table(spending$country, spending$category, spending$capital_area) == 108L),
          all(table(age_profile$category) == 576L))
korea <- subset(spending, country == "Korea" & capital_area == "national" & category == "food")
stopifnot(sum(is.na(korea$seasonal)) == 12L, sum(is.na(korea$monthly)) == 1L,
          isTRUE(all.equal(mean(korea$index[korea$month %/% 100 == 2019]), 100)))
jan2020 <- subset(korea, month == 202001)
jan2019 <- subset(korea, month == 201901)
stopifnot(isTRUE(all.equal(jan2020$yoy, 100 * (jan2020$amount_local / jan2019$amount_local - 1))),
          isTRUE(all.equal(jan2020$seasonal, 100 * log(jan2020$amount_local / jan2019$amount_local))))

# The labelled STL rows must keep trend and seasonal values distinct.
parts <- seasonal_decomposition(korea)
expected_parts <- stl(ts(log(korea$amount_local), frequency = 12),
                      s.window = "periodic", robust = TRUE)$time.series
stopifnot(isTRUE(all.equal(parts$value[parts$component == "Trend"],
                          as.numeric(expected_parts[, "trend"]))),
          isTRUE(all.equal(parts$value[parts$component == "Seasonal"],
                          as.numeric(expected_parts[, "seasonal"]))),
          isTRUE(all.equal(rowSums(matrix(parts$value, ncol = 3)), log(korea$amount_local))))

# The overview exports the values represented by each heatmap tile.
testServer(server, {
  session$setInputs(tab = "overview", country = "both", overview_view = "industry_growth",
                    overview_period = "since_covid_onset")
  growth <- overview_data()
  stopifnot(nrow(growth) == 4L * 7L * 36L, all(growth$month >= 202001),
            length(unique(growth$panel)) == 4L, startsWith(output$chart$src, "data:image/png"),
            grepl("Median", output$summary, fixed = TRUE))
  csv <- read.csv(output$download)
  stopifnot(nrow(csv) == nrow(growth), isTRUE(all.equal(csv$yoy_pct, growth$value)))
  session$setInputs(country = "Taiwan", overview_view = "industry_gap", overview_period = "pre_covid")
  gap <- overview_data()
  capital <- subset(spending, country == "Taiwan" & capital_area == "capital")
  other <- subset(spending, country == "Taiwan" & capital_area == "non_capital")
  keys <- paste(gap$month, gap$category)
  expected <- capital$yoy[match(keys, paste(capital$month, capital$category))] -
    other$yoy[match(keys, paste(other$month, other$category))]
  stopifnot(nrow(gap) == 7L * 60L, min(gap$month) == 201501L, max(gap$month) == 201912L,
            all(gap$country == "Taiwan"), identical(gap$value, expected),
            startsWith(output$chart$src, "data:image/png"),
            grepl("percentage points", output$summary, fixed = TRUE))
  csv <- read.csv(output$download)
  stopifnot(isTRUE(all.equal(csv$gap_pp, gap$value)),
            isTRUE(all.equal(csv$gap_pp, csv$capital_yoy_pct - csv$non_capital_yoy_pct)))
})

# Render every dashboard view through the real Shiny server.
testServer(server, {
  session$setInputs(tab = "relationship", country = "both", area = "both", category = "food",
                    measure = "yoy", metric = "stringency", lead = "0", omit_feb = FALSE,
                    period = "all", relationship_view = "scatter", age_view = "age_observed",
                    spending_view = "timeline", diagnostic_view = "stl")
  assert_chart <- function() {
    stopifnot(startsWith(output$chart$src, "data:image/png"), nzchar(output$title),
              grepl("<table", output$summary, fixed = TRUE))
  }
  d <- paired()
  stopifnot(identical(sort(as.integer(table(d$panel))), rep(24L, 4)),
            all(d$month_id == month_id(d$covid_date)),
            all(d$stringency == covid$stringency[match(paste(d$country, d$covid_date),
                                                       paste(covid$country, covid$covid_date))]))
  session$setInputs(lead = "1", omit_feb = TRUE)
  d <- paired()
  stopifnot(identical(as.integer(table(d$panel)), c(23L, 23L, 22L, 22L)),
            all(d$month_id - month_id(d$covid_date) == 1),
            !any(d$country == "Taiwan" & d$month == 202102))
  for (view in names(relationship_views)) {
    session$setInputs(relationship_view = view)
    assert_chart()
  }
  session$setInputs(tab = "spending", period = "pre_covid", measure = "seasonal")
  stopifnot(all(shown_spending()$date < onset), nrow(shown_spending()) == 288L)
  for (view in names(spending_views)) {
    session$setInputs(spending_view = view)
    assert_chart()
  }
  session$setInputs(spending_view = "backtest")
  stopifnot(all(holdout()$date >= as.Date("2019-01-01") & holdout()$date < onset),
            all(forecast_accuracy(holdout())$Months == 12),
            all(is.finite(holdout()$prediction)))
  session$setInputs(spending_view = "forecast")
  stopifnot(all(predicted()$date >= onset), all(is.finite(predicted()$prediction)))
  session$setInputs(spending_view = "break_model")
  stopifnot(all(is.finite(segmented()$segmented)), all(abs(segmented()$phi) < 1))
  session$setInputs(period = "since_covid_onset", spending_view = "timeline")
  stopifnot(all(shown_spending()$date >= onset), nrow(shown_spending()) == 144L,
            all(is.finite(shown_spending()$value)))
  for (view in names(diagnostic_views)) {
    session$setInputs(tab = "diagnostics", diagnostic_view = view)
    assert_chart()
  }
  for (view in names(age_views)) {
    session$setInputs(tab = "age", age_view = view)
    assert_chart()
  }
  stopifnot(nrow(age_data()) == 576L, all(is.finite(age_data()$ordinary)),
            all(is.finite(age_data()$robust)))
  session$setInputs(tab = "relationship", relationship_view = "scatter", country = "Taiwan",
                    area = "capital", metric = "cases", category = "apparel")
  stopifnot(nrow(paired()) == 22L, all(paired()$country == "Taiwan"),
            all(paired()$capital_area == "capital"), all(paired()$category == "apparel"))
  assert_chart()
  downloaded <- read.csv(output$download)
  stopifnot(nrow(downloaded) == 22L, all(downloaded$covid_measure == "cases"),
            identical(downloaded$month, paired()$month))
  session$setInputs(country = "both", area = "both", metric = "stringency",
                    lead = "0", omit_feb = FALSE, age_view = "age_observed")
  session$setInputs(tab = "spending", spending_view = "forecast", category = "retail_broad",
                    country = "Taiwan", area = "capital")
  stopifnot(all(is.finite(predicted()$prediction)),
            setequal(predicted()$model, c("Seasonal naïve", "Holt–Winters", "SARIMA")))
  assert_chart()
  session$setInputs(country = "both", area = "both")
  for (category in setdiff(names(categories), c("food", "apparel"))) {
    session$setInputs(tab = "relationship", category = category)
    stopifnot(all(paired()$category == category), all(table(paired()$panel) == 24L),
              length(unique(paired()$panel)) == 4L)
    assert_chart()
    downloaded <- read.csv(output$download)
    stopifnot(nrow(downloaded) == 96L, all(downloaded$category == category))
    session$setInputs(tab = "age")
    stopifnot(nrow(age_data()) == 576L, all(age_data()$category == category))
    assert_chart()
  }
})
# COVID-19 comparison section (daily module).
stopifnot(nrow(covid_daily) == 1460L, all(covid_daily$cases_pm >= 0, na.rm = TRUE),
          all(covid_daily$cases_7day >= 0, na.rm = TRUE),
          sum(is.na(covid_daily$cases_7day[covid_daily$date < as.Date("2021-01-07")])) == 12L)
korea_daily <- subset(covid_daily, country == "Korea")
day <- which(korea_daily$date == as.Date("2021-03-01"))
stopifnot(isTRUE(all.equal(korea_daily$cases_7day[day], mean(korea_daily$cases_pm[(day - 6):day], na.rm = TRUE))))
# Missing calendar days must not turn a seven-day mean into a shorter average.
days <- seq(as.Date("2021-01-01"), by = "day", length.out = 15)
gap_mean <- trailing_mean((1:15)[-8], days[-8])
stopifnot(gap_mean[7] == 4, all(is.na(gap_mean[8:13])), gap_mean[14] == 12)
missing_mean <- trailing_mean(c(1:7, NA, 9:15), days)
stopifnot(all(is.na(missing_mean[8:14])), missing_mean[15] == 12)
local({
  fixture <- tempfile("covid-reports-")
  dir.create(fixture)
  on.exit(unlink(fixture, recursive = TRUE))
  raw <- read.csv("data/covid_daily.csv", fileEncoding = "UTF-8-BOM")
  corrected <- which(raw$country_code == "KOR" & raw$date == "2021-01-07")
  raw$jhu_new_confirmed[corrected] <- -100
  write.csv(raw, file.path(fixture, "covid_daily.csv"), row.names = FALSE)
  daily <- read_covid_daily(fixture)
  january <- subset(daily, country == "Korea" & date < as.Date("2021-02-01"))
  monthly <- subset(summarize_covid_monthly(daily), country == "Korea" & covid_date == as.Date("2021-01-01"))
  stopifnot(january$cases_pm[7] < 0,
            isTRUE(all.equal(sum(january$cases_pm), monthly$cases / covid_population[["KOR"]] * 1e6)))
  january$cases_pm[1] <- NA_real_
  stopifnot(is.na(covid_summary(january)[["Cases per million (total)"]]))
})
testServer(covid_section_server, args = list(data = covid_daily), {
  session$setInputs(country = "both", view = "trend", metric = "cases", compressed = FALSE,
                    events = TRUE, dates = as.Date(c("2021-01-01", "2022-12-31")))
  stopifnot(nrow(selected()) == 1460L, nrow(events()) == 4L,
            startsWith(output$chart$src, "data:image/png"),
            grepl("<table", output$summary, fixed = TRUE), nzchar(output$title))
  session$setInputs(view = "scatter", metric = "deaths", compressed = TRUE, country = "Taiwan",
                    dates = as.Date(c("2022-01-01", "2022-06-30")))
  stopifnot(nrow(selected()) == 181L, all(selected()$country == "Taiwan"),
            startsWith(output$chart$src, "data:image/png"))
  downloaded <- read.csv(output$download)
  stopifnot(nrow(downloaded) == 181L, all(downloaded$country == "Taiwan"))
  session$setInputs(view = "trend", events = FALSE, dates = as.Date(c("2021-05-01", "2021-06-15")))
  stopifnot(nrow(events()) == 0L, startsWith(output$chart$src, "data:image/png"))
})
testServer(covidTabServer, args = list(covid_daily = read_covid_daily()), {
  start <- as.Date("2021-03-01")
  session$setInputs(countries = unname(countries), metric = "new_cases", freq = "ma7",
                    per_million = TRUE, log = FALSE, facet = FALSE)
  for (days_selected in c(1L, 6L, 7L)) {
    end <- start + days_selected - 1L
    session$setInputs(dates = c(start, end))
    expected <- subset(covid_daily, date >= start & date <= end)
    stopifnot(nrow(plot_data()) == nrow(expected),
              identical(plot_data()$value, expected$cases_7day))
  }
  session$setInputs(per_million = FALSE, dates = c(start, start))
  stopifnot(nrow(plot_data()) == 2L,
            isTRUE(all.equal(plot_data()$value,
                             subset(covid_daily, date == start)$cases_7day *
                               unname(covid_population[names(countries)]) / 1e6)))
  session$setInputs(metric = covid_metrics[["Fully vaccinated (%)"]],
                    freq = "daily", dates = as.Date(c("2022-01-01", "2022-01-31")), dist_type = "qq")
  expected <- subset(covid_daily, date >= as.Date("2022-01-01") & date <= as.Date("2022-01-31") &
                       is.finite(owid_people_fully_vaccinated_per_hundred))
  stopifnot(identical(plot_data()$value, expected$owid_people_fully_vaccinated_per_hundred),
            nrow(two_sample_q(plot_data())) == 99L,
            startsWith(output$dist_plot$src, "data:image/png"))
  session$setInputs(metric = covid_metrics[["Reproduction rate (R)"]])
  stopifnot(nrow(plot_data()) > 2L, all(is.finite(plot_data()$value)))
})
cat(sprintf("All %d dashboard views rendered. All seven regional industries, geography, leads, transforms, forecasts, CSV downloads and the COVID-19 comparison section verified.\n", length(view_names)))
