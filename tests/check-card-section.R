source("app.R")

# The attachment has a wider Taiwan extract; comparisons stop at the common
# card/CPI endpoint and do not replace the joint analysis's data or baseline.
stopifnot(card_comparison$start == as.Date("2014-01-01"),
          card_comparison$end == as.Date("2023-08-01"),
          nrow(card_comparison$data) == 116L * 22L,
          nrow(subset(spending, category %in% c("food", "apparel"))) == 1296L)
raw <- card_comparison$data
all_categories <- names(card_category_labels)
total <- card_aggregate(raw)
category <- card_aggregate(raw, "category", all_categories)
for (country in c("Korea", "Taiwan")) {
  for (date_text in c("2014-01-01", "2020-06-01", "2023-08-01")) {
    date <- as.Date(date_text)
    keep <- total$country == country & total$date == date
    pieces <- category$country == country & category$date == date
    stopifnot(isTRUE(all.equal(total$amount_local[keep], sum(category$amount_local[pieces]))))
  }
}
nominal <- card_transform(category, "index", "raw")
real <- card_transform(category, "real_index", "raw")
for (d in list(nominal, real)) {
  baseline <- d[format(d$date, "%Y") == "2014", ]
  means <- tapply(baseline$value, interaction(baseline$country, baseline$category), mean)
  stopifnot(all(abs(means - 100) < 1e-10))
}
kr <- subset(total, country == "Korea")
local <- card_transform(kr, "local", "raw")
stopifnot(isTRUE(all.equal(local$value, kr$amount_local / 1e12)))
yoy <- card_transform(kr, "real_index", "yoy")
real_kr <- card_transform(kr, "real_index", "raw")
jan2020 <- which(kr$date == as.Date("2020-01-01"))
jan2019 <- which(kr$date == as.Date("2019-01-01"))
stopifnot(isTRUE(all.equal(yoy$value[jan2020], 100 * (real_kr$value[jan2020] / real_kr$value[jan2019] - 1))),
          sum(is.na(yoy$value)) == 12L)
ma <- card_transform(kr, "index", "ma12")
index_kr <- card_transform(kr, "index", "raw")
stopifnot(sum(is.na(ma$value)) == 11L,
          isTRUE(all.equal(ma$value[jan2020], mean(index_kr$value[(jan2020 - 11L):jan2020]))))

# Missing months must not turn into a twelve-row lag, or a partial moving average.
gap <- kr[kr$date != as.Date("2019-01-01"), ]
gap_yoy <- card_transform(gap, "real_index", "yoy")
gap_ma <- card_transform(gap, "real_index", "ma12")
stopifnot(is.na(gap_yoy$value[gap_yoy$date == as.Date("2020-01-01")]),
          all(is.na(gap_ma$value[gap_ma$date >= as.Date("2019-02-01") & gap_ma$date <= as.Date("2019-12-01")])))

# Other exclusion intentionally differs for e-commerce because Korea has no
# online industry split. Taiwan EC must lose exactly its online Other amount.
with_other <- card_aggregate(raw, "category", "EC")
without_other <- card_aggregate(raw, "category", "EC", drop_other = TRUE)
stopifnot(identical(subset(with_other, country == "Korea")$amount_local,
                    subset(without_other, country == "Korea")$amount_local))
tw_ec <- subset(with_other, country == "Taiwan")
tw_ec_drop <- subset(without_other, country == "Taiwan")
tw_online_other <- subset(raw, country == "Taiwan" & category == "EC" & origin == "OT")
stopifnot(isTRUE(all.equal(tw_ec$amount_local - tw_ec_drop$amount_local,
  tw_online_other$amount_local[match(tw_ec$date, tw_online_other$date)])))

# Check namespaced server outputs, unit/series combinations,
# selected date ranges, categories and the data downloaded by the displayed view.
testServer(card_section_server, args = list(data = card_comparison), {
  session$setInputs(country = "both", scope = "total", unit = "real_index", series = "raw",
    dates = as.Date(c("2014-01-01", "2023-08-01")), categories = c("FD", "TR", "EC"),
    drop_other = FALSE, free_y = FALSE, shade = TRUE,
    covid_range = as.Date(c("2020-01-01", "2022-12-01")), restrictions = FALSE)
  assert_chart <- function() {
    stopifnot(startsWith(output$chart$src, "data:image/png"), nzchar(output$title),
              grepl("<table", output$summary, fixed = TRUE))
  }
  stopifnot(nrow(selected()) == 232L)
  for (unit in c("local", "index", "real_index")) {
    for (series in c("raw", "ma12", "yoy")) {
      session$setInputs(unit = unit, series = series)
      assert_chart()
    }
  }
  session$setInputs(scope = "category", unit = "real_index", series = "yoy",
    dates = as.Date(c("2020-01-01", "2020-12-01")), restrictions = TRUE, free_y = TRUE)
  stopifnot(nrow(selected()) == 72L, all(selected()$category %in% c("FD", "TR", "EC")))
  assert_chart()
  # Date clipping follows transformations; a January value still has its prior year.
  fd <- subset(selected(), country == "Korea" & category == "FD" & date == as.Date("2020-01-01"))
  full <- card_transform(card_aggregate(raw, "category", "FD"), "real_index", "yoy")
  reference <- subset(full, country == "Korea" & date == as.Date("2020-01-01"))
  stopifnot(isTRUE(all.equal(fd$value, reference$value)))
  downloaded <- read.csv(output$download)
  stopifnot(nrow(downloaded) == nrow(selected()), all(downloaded$display_unit == "real_index"),
    all(downloaded$series == "yoy"), isTRUE(all.equal(downloaded$value, selected()$value)))
  session$setInputs(country = "Taiwan", categories = "EC", drop_other = TRUE, shade = FALSE,
    dates = as.Date(c("2020-01-15", "2020-12-20")))
  stopifnot(nrow(selected()) == 12L, all(selected()$country == "Taiwan"),
    min(selected()$date) == as.Date("2020-01-01"))
  assert_chart()
})
cat("Card comparison: source totals, normalization, inflation, calendar lags, Other exclusion, chart options and CSV verified.\n")
