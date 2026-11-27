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

# A month's cases, deaths or stringency are reported only when every calendar
# day has a value. JHU CSSE starts on 22 January 2020 (the first daily change is
# 23 January), so January 2020 cases and deaths stay NA; its stringency is complete.
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

# Capital and non-capital confirmed cases (README.md). A month's count is the change in
# cumulative cases between month ends, so it needs both month ends.
read_zone_cases <- function(path = "data") {
  d <- read.csv(file.path(path, "covid_cases_by_capital_area_2020_2022.csv"), fileEncoding = "UTF-8-BOM")
  d$date <- as.Date(d$date)
  stopifnot(!anyDuplicated(d[c("country", "date", "capital_area")]), setequal(d$country, unname(countries)),
            setequal(d$capital_area, c("capital", "non_capital", "unallocated")),
            all(table(d$country, d$capital_area) == 1096L), all(d$new_confirmed >= 0, na.rm = TRUE))
  ends <- d[d$date == as.Date(format(d$date + 1, "%Y-%m-01")) - 1, ]
  ends$month_id <- month_id(ends$date)
  ends <- ends[order(ends$country, ends$capital_area, ends$date), ]
  ends$cases <- ave(ends$cumulative_confirmed, ends$country, ends$capital_area, FUN = function(x) c(NA, diff(x)))
  ends <- ends[ends$capital_area != "unallocated", c("country", "capital_area", "month_id", "cases")]
  # Fixed December 2021 zone populations (README.md), like the national denominators.
  population <- read.csv(file.path(path, "population_by_capital_area_2021.csv"), fileEncoding = "UTF-8-BOM")
  stopifnot(nrow(population) == 4L, !anyDuplicated(population[c("country", "capital_area")]),
            all(population$population > 0))
  at <- match(paste(ends$country, ends$capital_area), paste(population$country, population$capital_area))
  stopifnot(!anyNA(at))
  ends$cases_per_million <- ends$cases / population$population[at] * 1e6
  ends
}

# Korea's published distancing level per zone (README.md), as each month's day-weighted
# mean. Months with any day outside a level scheme stay missing.
read_distancing_levels <- function(path = "data") {
  d <- read.csv(file.path(path, "korea_distancing_levels_by_capital_area_2020_2022.csv"),
                fileEncoding = "UTF-8-BOM", colClasses = "character")
  d$start_date <- as.Date(d$start_date)
  d$end_date <- as.Date(d$end_date)
  days <- do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
    data.frame(capital_area = d$zone[i], date = seq(d$start_date[i], d$end_date[i], by = "day"),
               level = suppressWarnings(as.numeric(d$level[i])))
  }))
  stopifnot(setequal(d$zone, c("capital", "non_capital")), !anyDuplicated(days[c("capital_area", "date")]),
            all(table(days$capital_area) == as.integer(max(d$end_date) - min(d$start_date)) + 1L),
            all(is.na(days$level) == !nzchar(d$level[rep(seq_len(nrow(d)), d$end_date - d$start_date + 1L)])))
  days$month_id <- month_id(days$date)
  monthly <- aggregate(level ~ capital_area + month_id, days, mean, na.action = na.pass)
  monthly$country <- "Korea"
  monthly
}

# National monthly rows plus capital and non-capital rows. Zone rows carry their own
# cases and Korea's zone distancing level; deaths and stringency stay national.
covid_by_area <- function(covid, zone_cases, levels) {
  national <- transform(covid, capital_area = "national", level = NA_real_,
                        cases_per_million = cases / unname(covid_population[ifelse(country == "Korea", "KOR", "TWN")]) * 1e6)
  zones <- merge(covid[setdiff(names(covid), "cases")], zone_cases, by = c("country", "month_id"))
  zones <- merge(zones, levels, by = c("country", "capital_area", "month_id"), all.x = TRUE)
  rbind(national, zones[names(national)])
}
