source("app.R")

stopifnot(nrow(spending) == 1296L, nrow(covid) == 48L)
korea <- subset(spending, country == "Korea" & capital_area == "national" & category == "food")
stopifnot(sum(is.na(korea$seasonal)) == 12L, sum(is.na(korea$monthly)) == 1L,
          isTRUE(all.equal(mean(korea$index[korea$month %/% 100 == 2019]), 100)))
jan2020 <- subset(korea, month == 202001)
jan2019 <- subset(korea, month == 201901)
stopifnot(isTRUE(all.equal(jan2020$yoy, 100 * (jan2020$amount_local / jan2019$amount_local - 1))),
          isTRUE(all.equal(jan2020$seasonal, 100 * log(jan2020$amount_local / jan2019$amount_local))))

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
})
cat(sprintf("All %d dashboard views rendered. Period, geography, leads, transforms, forecasts and CSV download verified.\n", length(view_names)))
