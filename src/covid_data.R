# Fixed 2021 denominators from the national sources linked in README.md.
covid_population <- c(KOR = 51744876, TWN = 23375314)

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
  population <- unname(covid_population[d$country_code])
  stopifnot(all(is.finite(population) & population > 0))
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
  d
}

summarize_covid_monthly <- function(d) {
  d$month_id <- month_id(d$date)
  d <- lapply(split(d, list(d$country, d$month_id), drop = TRUE), function(g) {
    first <- as.Date(format(g$date[1], "%Y-%m-01"))
    expected <- as.integer(seq(first, by = "month", length.out = 2)[2] - first)
    daily <- function(x, statistic) {
      if (length(x) != expected || any(!is.finite(x))) return(NA_real_)
      statistic(x)
    }
    data.frame(country = g$country[1], month_id = g$month_id[1], covid_date = first,
               cases = daily(g$jhu_new_confirmed, sum), deaths = daily(g$jhu_new_deaths, sum),
               stringency = daily(g$stringency, mean))
  })
  do.call(rbind, d)
}
