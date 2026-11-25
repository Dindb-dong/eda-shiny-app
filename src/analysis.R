countries <- c(KOR = "Korea", TWN = "Taiwan")
areas <- c(national = "Nationwide", capital = "Capital area", non_capital = "Non-capital area")
spending_measures <- c(yoy = "Year-over-year growth (%)",
                       index = "Spending index (2019 mean = 100)",
                       seasonal = "12-month log difference (×100)",
                       gap = "Gap from pre-COVID same-month median (%)",
                       monthly = "Month-over-month growth (%)",
                       double_diff = "First and seasonal log difference (×100)")
covid_measures <- c(cases = "Reported cases", deaths = "Reported deaths",
                    stringency = "Restriction stringency (0–100)")
onset <- as.Date("2020-01-01")
month_id <- function(date) as.integer(format(date, "%Y")) * 12L + as.integer(format(date, "%m"))

read_industry_crosswalk <- function(path = "data") {
  d <- read.csv(file.path(path, "industry_crosswalk.csv"), fileEncoding = "UTF-8-BOM",
                colClasses = "character")
  d <- subset(d, status != "alternative_not_additive")
  stopifnot(!anyDuplicated(d$category), !anyDuplicated(d$taiwan_code),
            all(nzchar(d$label_en)), all(nzchar(d$korea_codes)))
  d
}

read_spending <- function(path = "data", industry = read_industry_crosswalk(path)) {
  read <- function(file) read.csv(file.path(path, file), fileEncoding = "UTF-8-BOM")
  before <- read("card_spending_by_capital_area_pre_covid_2014_2019.csv")
  since <- read("card_spending_by_capital_area_since_covid_onset_2020_2022.csv")
  spending <- rbind(before, since)
  keys <- c("country", "month", "category", "capital_area")
  stopifnot(!anyDuplicated(spending[keys]),
            setequal(spending$country, unname(countries)),
            setequal(spending$category, industry$category),
            setequal(spending$capital_area, c(names(areas)[-1], "unallocated")),
            all(is.finite(spending$amount_local) & spending$amount_local >= 0),
            all((spending$month < 202001) == (spending$covid_period == "pre_covid")),
            all(before$covid_period == "pre_covid"), all(since$covid_period == "since_covid_onset"))
  total_keys <- c("country", "month", "category", "currency")
  totals <- aggregate(spending["amount_local"], spending[total_keys], sum)
  source_columns <- c("country", "month", "source_category", "currency", "amount_local")
  national_source <- rbind(read("korea_national_2019_2022.csv")[source_columns],
                           read("taiwan_national_2019_2022.csv")[source_columns])
  korea_codes <- strsplit(industry$korea_codes, "+", fixed = TRUE)
  source_map <- rbind(
    data.frame(country = "Korea", category = rep(industry$category, lengths(korea_codes)),
               source_category = unlist(korea_codes)),
    data.frame(country = "Taiwan", category = industry$category,
               source_category = industry$taiwan_code))
  since_source <- merge(source_map, subset(national_source, month >= 202001),
                        by = c("country", "source_category"))
  stopifnot(!anyDuplicated(since_source[c("country", "category", "source_category", "month")]),
            nrow(since_source) == 36L * nrow(source_map))
  national <- rbind(read("card_spending_pre_covid_2014_2019.csv")[c(total_keys, "amount_local")],
                    aggregate(since_source["amount_local"], since_source[total_keys], sum))
  checked <- merge(totals, national, by = total_keys)
  stopifnot(!anyDuplicated(national[total_keys]), nrow(checked) == nrow(totals),
            nrow(checked) == nrow(national), all(checked$amount_local.x == checked$amount_local.y))

  totals$capital_area <- "national"
  totals$covid_period <- ifelse(totals$month < 202001, "pre_covid", "since_covid_onset")
  d <- rbind(totals[names(spending)], subset(spending, capital_area != "unallocated"))
  d$date <- as.Date(paste0(d$month, "01"), "%Y%m%d")
  d$month_id <- month_id(d$date)
  d$season <- as.integer(format(d$date, "%m"))
  d$region <- unname(areas[d$capital_area])
  d$panel <- paste(d$country, d$region, sep = " · ")

  by_series(d, function(g) {
    stopifnot(all(g$amount_local > 0))
    add_measures(g)
  }, keys = c("country", "category", "capital_area"))
}

# Spending measures for one monthly series (Jan 2014 – Dec 2022), shared by the
# area series and the Taiwan city/county series so both get the same mask.
add_measures <- function(g) {
    stopifnot(identical(g$date, seq(as.Date("2014-01-01"), as.Date("2022-12-01"), by = "month")),
              length(unique(g$currency)) == 1L)
    # Small units can have zero or net-negative (refund) months; no ratio is defined there.
    amount <- ifelse(g$amount_local > 0, g$amount_local, NA_real_)
    year_ago <- amount[match(g$month_id - 12L, g$month_id)]
    last_month <- amount[match(g$month_id - 1L, g$month_id)]
    pre <- subset(g, date < onset)
    median_by_month <- tapply(amount[g$date < onset], pre$season, median, na.rm = TRUE)
    g$index <- 100 * amount / mean(amount[g$month %/% 100 == 2019], na.rm = TRUE)
    g$yoy <- 100 * (amount / year_ago - 1)
    g$seasonal <- 100 * (log(amount) - log(year_ago))
    g$monthly <- 100 * (amount / last_month - 1)
    g$double_diff <- g$seasonal - g$seasonal[match(g$month_id - 1L, g$month_id)]
    g$gap <- 100 * (amount / unname(median_by_month[as.character(g$season)]) - 1)
    if (g$country[1] == "Taiwan") {
      # NCCC's category mix shifts abruptly in February 2021 (food falls by half and
      # general retail jumps in the same month, in every city and county). The source
      # gives no notice of a change, but the pattern matches a merchant-to-industry
      # mapping change, so change measures whose endpoints straddle that month are
      # masked in every category; level measures cannot be repaired.
      straddles <- g$month >= 202102 & g$month <= 202201
      g$yoy[straddles] <- NA
      g$seasonal[straddles] <- NA
      g$monthly[g$month == 202102] <- NA
      g$double_diff <- g$seasonal - g$seasonal[match(g$month_id - 1L, g$month_id)]
    }
    # A zero month in a small unit gives an infinite or undefined change; leave it missing.
    for (measure in c("index", "yoy", "seasonal", "monthly", "double_diff", "gap")) {
      g[[measure]][!is.finite(g[[measure]])] <- NA
    }
    g
}

industry_months <- function(spending, selected_countries, period, view) {
  d <- spending[spending$country %in% selected_countries &
                  spending$capital_area %in% c("capital", "non_capital") &
                  spending$month >= 201501, ]
  if (period != "all") d <- d[d$covid_period == period, ]
  keys <- c("country", "category", "month", "date")
  if (view == "industry_growth") {
    d <- d[c(keys, "capital_area", "panel", "yoy")]
    names(d)[names(d) == "yoy"] <- "value"
    return(d)
  }

  capital <- d[d$capital_area == "capital", c(keys, "yoy")]
  non_capital <- d[d$capital_area == "non_capital", c(keys, "yoy")]
  names(capital)[names(capital) == "yoy"] <- "capital_yoy_pct"
  names(non_capital)[names(non_capital) == "yoy"] <- "non_capital_yoy_pct"
  matched <- merge(capital, non_capital, by = keys, all = TRUE)
  matched$panel <- matched$country
  matched$value <- matched$capital_yoy_pct - matched$non_capital_yoy_pct
  matched
}

by_series <- function(data, fn, keys = "panel") {
  groups <- split(data, data[keys], drop = TRUE)
  do.call(rbind, lapply(groups, function(g) fn(g[order(g$date), , drop = FALSE])))
}

pair_months <- function(spending, covid, metric, lead = 0L) {
  spending$covid_month_id <- spending$month_id - lead
  paired <- merge(spending, covid,
                  by.x = c("country", "covid_month_id"), by.y = c("country", "month_id"))
  paired$x <- paired[[metric]]
  # asinh retains zero and negative reporting corrections.
  if (metric != "stringency") paired$x <- asinh(paired$x / 100)
  paired <- subset(paired, is.finite(x) & is.finite(value))
  paired[order(paired$panel, paired$date), , drop = FALSE]
}

safe_correlation <- function(x, y, method = "spearman") {
  if (length(x) < 3L || sd(x) == 0 || sd(y) == 0) return(NA_real_)
  cor(x, y, method = method)
}

adjust_association <- function(g) {
  # One annual harmonic and a linear trend leave degrees of freedom in a 24–36 month window.
  g$sin_month <- sin(2 * pi * g$season / 12)
  g$cos_month <- cos(2 * pi * g$season / 12)
  g$x_adjusted <- residuals(lm(x ~ month_id + sin_month + cos_month, data = g))
  g$y_adjusted <- residuals(lm(value ~ month_id + sin_month + cos_month, data = g))
  fit <- lm(value ~ x + month_id + sin_month + cos_month, data = g)
  g$fitted <- fitted(fit)
  g$residual <- residuals(fit)
  g$cook <- cooks.distance(fit)
  g
}

historical_projection <- function(g) {
  pre <- g$date < onset
  fit <- lm(log(amount_local) ~ month_id + factor(season), data = g[pre, ])
  g$projected <- 100 * exp(predict(fit, newdata = g)) / mean(g$amount_local[g$month %/% 100 == 2019])
  g
}

seasonal_decomposition <- function(g) {
  parts <- stl(ts(log(g$amount_local), frequency = 12), s.window = "periodic", robust = TRUE)$time.series
  components <- c("Trend", "Seasonal", "Remainder")
  # stl() returns seasonal before trend; select by name to match the display order.
  parts <- parts[, c("trend", "seasonal", "remainder"), drop = FALSE]
  data.frame(date = rep(g$date, 3), panel = g$panel[1],
             component = factor(rep(components, each = nrow(g)), levels = components),
             value = as.vector(parts))
}

forecast_index <- function(g, cutoff) {
  train <- g$date < cutoff
  testing <- g[!train, , drop = FALSE]
  log_spending <- ts(log(g$amount_local[train]), frequency = 12)
  horizon <- nrow(testing)
  seasonal_naive <- rep(tail(log_spending, 12), length.out = horizon)
  smooth <- tryCatch(
    HoltWinters(log_spending, seasonal = "additive"),
    # The default starting values fail for Taiwan's capital-area retail series.
    error = function(e) HoltWinters(log_spending, seasonal = "additive",
                                    optim.start = c(alpha = .4, beta = .2, gamma = .2))
  )
  sarima <- arima(log_spending, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
  if (sarima$code != 0L) stop("Seasonal ARIMA failed to converge for ", g$panel[1])
  forecasts <- list("Seasonal naïve" = seasonal_naive,
                    "Holt–Winters" = as.numeric(predict(smooth, n.ahead = horizon)),
                    "SARIMA" = as.numeric(predict(sarima, n.ahead = horizon)$pred))
  base <- mean(g$amount_local[g$month %/% 100 == 2019])
  do.call(rbind, lapply(names(forecasts), function(model) {
    data.frame(panel = testing$panel, date = testing$date, actual = testing$index,
               model = model, prediction = 100 * exp(forecasts[[model]]) / base)
  }))
}

forecast_backtest <- function(g) {
  training_and_holdout <- subset(g, date < onset)
  forecast_index(training_and_holdout, cutoff = as.Date("2019-01-01"))
}

forecast_accuracy <- function(backtest) {
  groups <- split(backtest, backtest[c("panel", "model")], drop = TRUE)
  do.call(rbind, lapply(groups, function(g) {
    errors <- g$prediction - g$actual
    data.frame(Series = g$panel[1], Model = g$model[1], Months = nrow(g),
               MAE = mean(abs(errors)), RMSE = sqrt(mean(errors^2)))
  }))
}

segmented_trend <- function(g) {
  g$t <- g$month_id - month_id(onset)
  g$post <- as.integer(g$t >= 0)
  g$after <- pmax(g$t, 0)
  design <- model.matrix(~ t + post + after + factor(season), data = g)[, -1, drop = FALSE]
  model <- arima(log(g$amount_local), order = c(1, 0, 0), xreg = design, method = "ML")
  if (model$code != 0L) stop("Segmented regression failed to converge for ", g$panel[1])
  estimates <- coef(model)
  fitted_log <- drop(design %*% estimates[colnames(design)]) + unname(estimates["intercept"])
  no_break <- fitted_log - estimates["post"] * g$post - estimates["after"] * g$after
  base <- mean(g$amount_local[g$month %/% 100 == 2019])
  g$segmented <- 100 * exp(fitted_log) / base
  g$continued <- 100 * exp(no_break) / base
  g$level_change <- 100 * expm1(unname(estimates["post"]))
  g$monthly_slope_delta <- 100 * unname(estimates["after"])
  g$phi <- unname(estimates["ar1"])
  g
}

quantile_comparison <- function(g) {
  pre <- g$value[g$date < onset & is.finite(g$value)]
  since <- g$value[g$date >= onset & is.finite(g$value)]
  p <- ppoints(min(length(pre), length(since)))
  x <- unname(quantile(pre, p))
  y <- unname(quantile(since, p))
  data.frame(panel = g$panel[1], before = x, since = y, difference = y - x,
             average = (x + y) / 2)
}

location_spread <- function(g) {
  by_series(subset(g, is.finite(value)), function(h) {
    data.frame(panel = h$panel[1], year = h$year[1], median = median(h$value), iqr = IQR(h$value))
  }, keys = "year")
}

binary_curve <- function(g) {
  x <- g$stringency
  status <- g$yoy < 0
  # Overlapping ranges and minimum counts guard against one-variable separation.
  if (min(sum(status), sum(!status)) < 4 ||
      max(min(x[status]), min(x[!status])) >= min(max(x[status]), max(x[!status]))) {
    return(data.frame(panel = character(), x = numeric(), probability = numeric()))
  }
  model <- glm(status ~ x, family = binomial())
  grid <- seq(min(x), max(x), length.out = 60)
  data.frame(panel = g$panel[1], x = grid,
             probability = unname(predict(model, newdata = data.frame(x = grid), type = "response")))
}

boxcox_profile <- function(g) {
  # The Jacobian makes likelihoods comparable across transformed responses.
  basis <- model.matrix(~ month_id + factor(season), g)
  values <- g$index
  log_values <- log(values)
  lambda <- seq(-1, 2, by = .05)
  log_likelihood <- vapply(lambda, function(power) {
    y <- if (abs(power) < 1e-8) log_values else expm1(power * log_values) / power
    error <- lm.fit(basis, y)$residuals
    -length(y) / 2 * log(mean(error^2)) + (power - 1) * sum(log_values)
  }, numeric(1))
  data.frame(panel = g$panel[1], lambda = lambda, likelihood = log_likelihood - max(log_likelihood))
}

read_age_profile <- function(path = "data", industry = read_industry_crosswalk(path)) {
  d <- read.csv(file.path(path, "taiwan_demographics_2019_2022.csv"), fileEncoding = "UTF-8-BOM")
  d <- subset(d, dimension == "age" & source_category %in% industry$taiwan_code & is.finite(age_midpoint))
  stopifnot(!anyDuplicated(d[c("month", "age_midpoint", "source_category")]),
            all(d$transactions > 0 & d$mean_ticket_local > 0),
            all(abs(d$mean_ticket_local - d$amount_local / d$transactions) < 1e-6))
  data.frame(date = as.Date(paste0(d$month, "01"), "%Y%m%d"),
             category = industry$category[match(d$source_category, industry$taiwan_code)],
             age = d$age_midpoint, age_band = d$age_band, transactions = d$transactions,
             observed = d$mean_ticket_local)
}

smooth_age_profile <- function(d) {
  d$time <- month_id(d$date)
  formula <- log(observed) ~ age + time
  ordinary <- loess(formula, data = d, degree = 1, span = .5)
  robust <- loess(formula, data = d, degree = 1, span = .5, family = "symmetric")
  # Predict at observed age-month pairs only, with equal weight per cell.
  d$ordinary <- exp(predict(ordinary, newdata = d))
  d$robust <- exp(predict(robust, newdata = d))
  d$residual <- 100 * log(d$observed / d$robust)
  d
}

# Natural Earth admin-1 units tagged with the two card-spending zones (README.md).
# Matsu (Lienchiang County, non-capital) is absent from Natural Earth, so Taiwan has
# 21 drawn units although its spending totals include all 22.
read_region_boundaries <- function(path = "data") {
  d <- read.csv(file.path(path, "region_boundaries.csv"), fileEncoding = "UTF-8-BOM")
  units <- unique(d[c("country", "unit_code", "zone")])
  capital <- list(Korea = c("KR-11", "KR-28", "KR-41"), Taiwan = c("TW-TPE", "TW-TPQ", "TW-KEE"))
  stopifnot(!anyDuplicated(units$unit_code), sum(units$country == "Korea") == 17L,
            sum(units$country == "Taiwan") == 21L, !"TW-LIE" %in% units$unit_code,
            all(vapply(names(capital), function(k) setequal(units$unit_code[units$country == k & units$zone == "capital"],
                                                            capital[[k]]), logical(1))),
            all(units$zone %in% c("capital", "non_capital")), all(is.finite(d$lon) & is.finite(d$lat)))
  d[order(d$piece, d$order), ]
}

# Taiwan's 22 cities and counties from NCCC C02 (README.md). Unit totals must
# reproduce the national, capital and non-capital series exactly.
taiwan_capital_units <- c("TW-TPE", "TW-TPQ", "TW-KEE")
read_unit_spending <- function(spending, path = "data", industry = read_industry_crosswalk(path)) {
  d <- read.csv(file.path(path, "card_spending_by_city_county_taiwan_2014_2022.csv"), fileEncoding = "UTF-8-BOM")
  stopifnot(!anyDuplicated(d[c("month", "category", "unit_code")]), length(unique(d$unit_code)) == 22L,
            setequal(d$category, industry$category), all(table(d$unit_code, d$category) == 108L),
            all(is.finite(d$amount_local)), all(d$currency == "TWD"))
  d$capital_area <- ifelse(d$unit_code %in% taiwan_capital_units, "capital", "non_capital")
  for (zone in c("national", "capital", "non_capital")) {
    units <- if (zone == "national") d else d[d$capital_area == zone, ]
    total <- aggregate(amount_local ~ month + category, units, sum)
    series <- spending[spending$country == "Taiwan" & spending$capital_area == zone, ]
    matched <- merge(total, series[c("month", "category", "amount_local")], by = c("month", "category"))
    stopifnot(nrow(matched) == 108L * 7L, all(matched$amount_local.x == matched$amount_local.y))
  }
  d$date <- as.Date(paste0(d$month, "01"), "%Y%m%d")
  d$month_id <- month_id(d$date)
  d$season <- as.integer(format(d$date, "%m"))
  by_series(d, add_measures, keys = c("category", "unit_code"))
}

# One row per country and zone: year-over-year growth for the chosen industry and month,
# with that month's national COVID-19 context.
zone_values <- function(spending, covid, selected_countries, category, month) {
  d <- spending[spending$country %in% selected_countries & spending$category == category &
                  spending$month == month & spending$capital_area %in% c("capital", "non_capital"),
                c("country", "category", "month", "capital_area", "region", "yoy")]
  context <- covid[covid$month_id == month_id(as.Date(paste0(month, "01"), "%Y%m%d")), ]
  d$stringency <- context$stringency[match(d$country, context$country)]
  population <- c(Korea = covid_population[["KOR"]], Taiwan = covid_population[["TWN"]])
  d$cases_per_million <- context$cases[match(d$country, context$country)] / population[d$country] * 1e6
  d[order(d$country, d$capital_area), ]
}
