# Key findings: a fixed, scrolling story built from the final data.
# Every headline number is computed here, so the text cannot drift from the data.
# Depends on src/analysis.R, src/plots.R and src/covid_section.R (country colours).

finding_ids <- c("paths", "cases", "restrictions", "industries", "detrended", "break")
research_question <- "During COVID-19, what moved card spending: case counts or government restrictions? South Korea and Taiwan, 2020–2022."
pct <- function(x) sprintf("%+.1f%%", x)
# Locale-independent month names for generated sentences.
month_year <- function(date) paste(month.name[as.integer(format(date, "%m"))], format(date, "%Y"))
month_short <- function(month) month.abb[month %% 100]
grey_bar <- "#bdbdbd"

build_findings <- function(spending, covid, categories) {
  national <- subset(spending, capital_area == "national")
  label <- function(category) unname(categories[category])
  population <- c(Korea = covid_population[["KOR"]], Taiwan = covid_population[["TWN"]])
  covid$year <- as.integer(format(covid$covid_date, "%Y"))
  covid$cases_pm <- covid$cases / population[covid$country] * 1e6
  by_country <- function(x, f) vapply(split(x, x$country), f, numeric(1))

  # (a) Restriction and case paths --------------------------------------------
  early <- subset(covid, year <= 2021)
  mean_stringency <- tapply(early$stringency, early$country, mean)
  strict_months <- tapply(covid$stringency >= 50, covid$country, sum)
  strict_years <- tapply(covid$year[covid$stringency >= 50], covid$country[covid$stringency >= 50],
                         function(y) paste(unique(y), collapse = ", "))
  cases_by_year <- tapply(covid$cases, list(covid$country, covid$year), sum, na.rm = TRUE)
  share_2022 <- 100 * cases_by_year[, "2022"] / rowSums(cases_by_year)
  stringency_by_year <- tapply(covid$stringency, list(covid$country, covid$year), mean)
  paths <- list(
    headline = sprintf("Korea restricted early and long, Taiwan only briefly in 2021, yet both countries' case waves came in 2022 (%.1f%% and %.1f%% of all 2020–2022 cases)",
                       share_2022[["Korea"]], share_2022[["Taiwan"]]),
    takeaway = sprintf(paste(
      "Korea spent %d months at a stringency of 50 or more; Taiwan spent %d, all in %s, during its Level 3 alert.",
      "Average stringency over 2020–2021 was %.0f in Korea and %.0f in Taiwan, and fell to %.0f and %.0f in 2022, the year cases exploded."),
      strict_months[["Korea"]], strict_months[["Taiwan"]], strict_years[["Taiwan"]],
      mean_stringency[["Korea"]], mean_stringency[["Taiwan"]],
      stringency_by_year["Korea", "2022"], stringency_by_year["Taiwan", "2022"]),
    checks = list(korea_stricter = mean_stringency[["Korea"]] > mean_stringency[["Taiwan"]],
                  waves_2022 = all(share_2022 > 50),
                  eased_2022 = all(stringency_by_year[, "2022"] < stringency_by_year[, "2021"]))
  )
  long <- rbind(data.frame(country = covid$country, date = covid$covid_date, value = covid$stringency,
                           panel = "Restriction stringency (0–100)"),
                data.frame(country = covid$country, date = covid$covid_date, value = covid$cases_pm / 1000,
                           panel = "Reported cases per 1,000 residents (monthly)"))
  long$panel <- factor(long$panel, levels = unique(long$panel))
  paths$plot <- ggplot(long, aes(date, value, colour = country, linetype = country)) +
    geom_line(linewidth = .85, na.rm = TRUE) +
    scale_colour_manual(values = covid_country_colors, name = NULL) +
    scale_linetype_manual(values = covid_country_lines, name = NULL) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    facet_wrap(vars(panel), ncol = 1, scales = "free_y") +
    labs(x = NULL, y = NULL) +
    chart_theme + theme(legend.position = "top", legend.justification = "left")
  paths$alt <- sprintf("Two stacked line charts, January 2020 to December 2022, for Korea and Taiwan: monthly restriction stringency above and reported cases per 1,000 residents below. Korea is above 50 stringency in %d months, Taiwan in %d (all in %s); %.0f%% of Korea's and %.0f%% of Taiwan's cases fall in 2022.",
                       strict_months[["Korea"]], strict_months[["Taiwan"]], strict_years[["Taiwan"]],
                       share_2022[["Korea"]], share_2022[["Taiwan"]])
  paths$table <- data.frame(Country = rownames(cases_by_year),
                            "Mean stringency 2020" = stringency_by_year[, "2020"],
                            "Mean stringency 2021" = stringency_by_year[, "2021"],
                            "Mean stringency 2022" = stringency_by_year[, "2022"],
                            "Months with stringency ≥ 50" = as.integer(strict_months[rownames(cases_by_year)]),
                            "Cases 2020" = cases_by_year[, "2020"], "Cases 2021" = cases_by_year[, "2021"],
                            "Cases 2022" = cases_by_year[, "2022"], check.names = FALSE)

  # (b) Case waves and spending ----------------------------------------------
  peaks <- do.call(rbind, lapply(split(covid, covid$country), function(g) head(g[order(-g$cases), ], 3)))
  peaks$month <- as.integer(format(peaks$covid_date, "%Y%m"))
  peak_growth <- merge(national[c("country", "category", "month", "yoy")], peaks[c("country", "month", "cases")],
                       by = c("country", "month"))
  # Taiwan's May–July 2022 growth compares with its May–July 2021 alert, a weak base.
  peak_growth$alert_base <- peak_growth$country == "Taiwan" & peak_growth$month %in% 202205:202207
  grew <- tapply(peak_growth$yoy > 0, peak_growth$country, sum)
  cells <- tapply(peak_growth$yoy, peak_growth$country, length)
  food <- subset(national, category == "food")
  food$value <- food$yoy
  food_cases <- pair_months(food, covid, "cases")
  rho <- by_country(food_cases, function(g) cor(g$x, g$value, method = "spearman"))
  food_2022 <- subset(food, month %/% 100 == 2022 & is.finite(yoy))
  food_up <- tapply(food_2022$yoy > 0, food_2022$country, sum)
  food_n <- tapply(food_2022$yoy, food_2022$country, length)
  peak_months <- tapply(peaks$month, peaks$country, function(m) paste(month_short(sort(m)), collapse = ", "))
  top <- peaks[!duplicated(peaks$country), ]
  top_grew <- tapply(peak_growth$yoy[peak_growth$month %in% top$month] > 0,
                     peak_growth$country[peak_growth$month %in% top$month], sum)
  cases_story <- list(
    headline = sprintf("Not case counts: in each country's three biggest case months, spending grew in %d of %d industry-months in Korea and %d of %d in Taiwan",
                       grew[["Korea"]], cells[["Korea"]], grew[["Taiwan"]], cells[["Taiwan"]]),
    takeaway = sprintf(paste(
      "The peaks were %s 2022 in Korea and %s 2022 in Taiwan. Food and dining grew year over year in %d of 12 months of 2022 in Korea and in all %d unmasked months in Taiwan, and moved with cases rather than against them (Spearman %+.2f and %+.2f).",
      "Growth was not universal: in Korea's single largest month (%s) only %d of 7 industries grew. Taiwan's May–June 2022 figures (open circles) compare with its 2021 alert, so they are partly a rebound from a weak base."),
      peak_months[["Korea"]], peak_months[["Taiwan"]], food_up[["Korea"]], food_n[["Taiwan"]],
      rho[["Korea"]], rho[["Taiwan"]], month_year(top$covid_date[top$country == "Korea"]), top_grew[["Korea"]]),
    checks = list(mostly_grew = all(grew / cells > .7), food_with_cases = all(rho > 0),
                  taiwan_food_all = food_up[["Taiwan"]] == food_n[["Taiwan"]],
                  korea_food_mostly = food_up[["Korea"]] >= 10)
  )
  peak_growth$industry <- factor(label(peak_growth$category), levels = rev(unname(categories)))
  peak_growth$panel <- paste(peak_growth$country, "·", peak_months[peak_growth$country], "2022")
  peak_growth$shown <- pmax(pmin(peak_growth$yoy, 75), -75)
  cases_story$plot <- ggplot(peak_growth, aes(shown, industry)) +
    geom_vline(xintercept = 0, colour = quiet) +
    geom_point(aes(colour = yoy > 0, shape = alert_base), size = 3, stroke = 1) +
    scale_colour_manual(values = c("TRUE" = teal, "FALSE" = orange), guide = "none") +
    scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 1), guide = "none") +
    scale_x_continuous(limits = c(-75, 75)) +
    labs(x = "Year-over-year growth in the peak case months (%, clipped at ±75)", y = NULL) +
    panel_grid(2L) + chart_theme + theme(panel.grid.major.y = element_blank())
  cases_story$alt <- sprintf("Dot plot of year-over-year spending growth by industry in each country's three largest case months of 2022 (Korea %s; Taiwan %s). %d of %d dots are above zero in Korea and %d of %d in Taiwan.",
                             peak_months[["Korea"]], peak_months[["Taiwan"]], grew[["Korea"]], cells[["Korea"]],
                             grew[["Taiwan"]], cells[["Taiwan"]])
  cases_story$table <- data.frame(Country = peak_growth$country, Month = peak_growth$month,
                                  Cases = peak_growth$cases, Industry = as.character(peak_growth$industry),
                                  "YoY growth (%)" = peak_growth$yoy,
                                  "Base month in Taiwan's 2021 alert" = peak_growth$alert_base, check.names = FALSE)

  # (c) Restrictions and spending, each country's strict period ------------------
  korea_food <- subset(food, country == "Korea")
  food_pairs <- pair_months(korea_food, covid, "stringency")
  r_food <- cor(food_pairs$x, food_pairs$value)
  strict <- food_pairs$month[food_pairs$stringency >= 50]
  strict_mean <- mean(food_pairs$value[food_pairs$stringency >= 50])
  relaxed_mean <- mean(food_pairs$value[food_pairs$stringency < 50])
  taiwan <- subset(national, country == "Taiwan")
  apr_jun <- function(category, year) {
    g <- taiwan[taiwan$category == category, ]
    100 * (g$amount_local[g$month == year * 100 + 6] / g$amount_local[g$month == year * 100 + 4] - 1)
  }
  alert <- data.frame(category = names(categories), industry = unname(categories),
                      y2019 = vapply(names(categories), apr_jun, numeric(1), 2019),
                      y2021 = vapply(names(categories), apr_jun, numeric(1), 2021))
  alert$difference <- alert$y2021 - alert$y2019
  hit <- alert[alert$difference <= -20, ]
  hit <- hit[order(hit$difference), ]
  restrictions <- list(
    headline = sprintf("Restrictions: spending dipped while rules were strict, through 2020–2021 in Korea (food %s year over year in strict months) and in Taiwan's 2021 alert (%s %s from April to June)",
                       pct(strict_mean), tolower(hit$industry[1]), pct(hit$y2021[1])),
    takeaway = sprintf(paste(
      "Korea (top): food and dining growth against stringency, with strict months (stringency of 50 or more) shaded; r = %.2f over %d months, and growth averaged %s in the other months, partly a 2022 rebound.",
      "Taiwan (bottom): year-over-year growth is masked during its alert, so the chart compares the April-to-June change in 2021 with the same months of 2019, both within one side of the February 2021 shift.",
      "In Taiwan, %s fell at least 20 points more than in 2019. Source-defined Other moved the other way (%s against %s)."),
      r_food, nrow(food_pairs), pct(relaxed_mean),
      paste(sprintf("%s (%s against %s)", tolower(hit$industry), pct(hit$y2021), pct(hit$y2019)), collapse = ", "),
      pct(alert$y2021[alert$category == "other"]), pct(alert$y2019[alert$category == "other"])),
    checks = list(korea_negative = r_food < -.5, strict_lower = strict_mean < relaxed_mean - 10,
                  taiwan_apparel = "apparel" %in% hit$category)
  )
  bands <- data.frame(start = as.Date(paste0(strict, "01"), "%Y%m%d"))
  bands$end <- as.Date(vapply(bands$start, function(d) as.numeric(seq(d, by = "month", length.out = 2)[2]), numeric(1)))
  restrictions$plot <- ggplot(food_pairs, aes(date, value)) +
    geom_rect(data = bands, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), inherit.aes = FALSE,
              fill = "#ececec") +
    geom_hline(yintercept = 0, colour = grey_bar) +
    geom_line(colour = orange, linewidth = .9) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(x = NULL, y = "Korea food and dining, YoY (%)") + chart_theme
  alert_long <- rbind(data.frame(industry = alert$industry, year = "Apr–Jun 2019", change = alert$y2019),
                      data.frame(industry = alert$industry, year = "Apr–Jun 2021 (alert)", change = alert$y2021))
  alert_long$industry <- factor(alert_long$industry, levels = rev(unname(categories)))
  restrictions$plot2 <- ggplot(alert_long, aes(change, industry, colour = year)) +
    geom_vline(xintercept = 0, colour = quiet) +
    geom_point(size = 3.2) +
    scale_colour_manual(values = c("Apr–Jun 2019" = grey_bar, "Apr–Jun 2021 (alert)" = orange), name = NULL) +
    labs(x = "Taiwan: change in spending from April to June (%)", y = NULL) +
    chart_theme + theme(legend.position = "top", legend.justification = "left", panel.grid.major.y = element_blank())
  restrictions$alt <- sprintf("Line chart of Korean food and dining year-over-year growth, 2020 to 2022, with %d strict months shaded; growth averages %s in strict months and %s otherwise.",
                              length(strict), pct(strict_mean), pct(relaxed_mean))
  restrictions$alt2 <- paste("Dot plot of Taiwan's April-to-June spending change by industry, 2019 against the 2021 alert.",
                             paste(sprintf("%s %s in 2021, %s in 2019", alert$industry, pct(alert$y2021), pct(alert$y2019)),
                                   collapse = "; "))
  restrictions$table <- rbind(
    data.frame(Country = "Korea", Item = paste("Food and dining YoY,", format(food_pairs$date, "%Y-%m")),
               Value = food_pairs$value, Stringency = food_pairs$stringency),
    data.frame(Country = "Taiwan", Item = paste(alert$industry, "Apr–Jun change 2021"),
               Value = alert$y2021, Stringency = NA_real_),
    data.frame(Country = "Taiwan", Item = paste(alert$industry, "Apr–Jun change 2019"), Value = alert$y2019,
               Stringency = NA_real_))

  # (d) Industries in 2020 ---------------------------------------------------
  annual <- aggregate(amount_local ~ country + category + year,
                      transform(subset(national, month %/% 100 %in% 2019:2020), year = month %/% 100), sum)
  change <- merge(subset(annual, year == 2019), subset(annual, year == 2020), by = c("country", "category"))
  change$change <- 100 * (change$amount_local.y / change$amount_local.x - 1)
  change$industry <- label(change$category)
  change <- change[c("country", "category", "industry", "change")]
  pick <- function(k, f) change[change$country == k, ][f(change$change[change$country == k]), ]
  korea_low <- pick("Korea", which.min); taiwan_low <- pick("Taiwan", which.min)
  taiwan_high <- pick("Taiwan", which.max)
  taiwan_second <- pick("Taiwan", function(x) order(x)[2])
  korea_second <- pick("Korea", function(x) order(x)[2])
  industries <- list(
    headline = sprintf("Uneven hit: in 2020, %s fell %.1f%% in Korea, and in barely restricted Taiwan %s fell %.1f%% while %s rose %s",
                       tolower(korea_low$industry), abs(korea_low$change),
                       tolower(taiwan_low$industry), abs(taiwan_low$change),
                       tolower(taiwan_high$industry), pct(taiwan_high$change)),
    takeaway = sprintf(paste(
      "Annual 2020 spending against 2019 in each country's own currency. Korea's next largest fall was %s (%s); Taiwan's was %s (%s).",
      "Taiwan's 2020 stringency averaged %.0f, so restrictions cannot explain all of its falls. These are levels before February 2021, so Taiwan's category shift does not affect them."),
      tolower(korea_second$industry), pct(korea_second$change), tolower(taiwan_second$industry),
      pct(taiwan_second$change), stringency_by_year["Taiwan", "2020"]),
    data = change
  )
  change$highlight <- paste(change$country, change$category) %in%
    c(paste("Korea", korea_low$category), paste("Taiwan", taiwan_low$category))
  change$industry <- factor(change$industry, levels = rev(unname(categories)))
  change$panel <- change$country
  industries$plot <- ggplot(change, aes(change, industry, fill = highlight)) +
    geom_vline(xintercept = 0, colour = quiet) +
    geom_col(width = .7) +
    geom_text(aes(label = sprintf("%+.1f", change), hjust = ifelse(change < 0, 1.15, -.15)),
              size = 3.3, colour = ink) +
    scale_fill_manual(values = c("TRUE" = orange, "FALSE" = grey_bar), guide = "none") +
    scale_x_continuous(expand = expansion(mult = .25)) +
    labs(x = "Change in annual spending, 2020 vs 2019 (%)", y = NULL) +
    panel_grid(2L) + chart_theme + theme(panel.grid.major.y = element_blank())
  industries$alt <- paste("Bar chart of the 2020 change in annual card spending against 2019 by industry, one panel per country.",
                          paste(sprintf("%s %s %s", change$country, change$industry, pct(change$change)), collapse = "; "))
  industries$table <- data.frame(Country = industries$data$country, Industry = industries$data$industry,
                                 "Change 2020 vs 2019 (%)" = industries$data$change, check.names = FALSE)

  # (e) Raw vs detrended association, Korea ------------------------------------
  korea <- subset(national, country == "Korea")
  korea$value <- korea$yoy
  assoc <- by_series(pair_months(korea, covid, "stringency"), function(g) {
    adjusted <- adjust_association(g)
    data.frame(category = g$category[1], months = nrow(g), raw = cor(g$x, g$value),
               adjusted = cor(adjusted$x_adjusted, adjusted$y_adjusted))
  }, keys = "category")
  assoc$industry <- label(assoc$category)
  survives <- assoc[abs(assoc$adjusted) >= .3 & abs(assoc$raw) >= .3 & sign(assoc$adjusted) == sign(assoc$raw), ]
  fades <- assoc[abs(assoc$raw) >= .3 & abs(assoc$adjusted) < .3, ]
  detrended <- list(
    headline = if (nrow(survives)) {
      sprintf("After removing trend and seasonality, only %s keeps a clear link to restrictions (r %.2f → %.2f)",
              paste(tolower(survives$industry), collapse = " and "), survives$raw[1], survives$adjusted[1])
    } else "After removing trend and seasonality, no Korean industry keeps a clear link to restrictions",
    takeaway = sprintf(paste(
      "Grey dots are raw Pearson correlations of Korean year-over-year growth with stringency; orange dots remove a linear trend and one annual cycle from both series (%d months).",
      "%s %s",
      "Taiwan is left out: its strict months (May–July 2021) fall inside the masked window, so its year-over-year growth cannot be compared there."),
      assoc$months[1],
      if (nrow(fades)) sprintf("Raw links for %s shrink below 0.3 in absolute value.", paste(tolower(fades$industry), collapse = "; ")) else "",
      "A shared 2020–2022 slump and recovery explains much of the raw pattern."),
    data = assoc[c("category", "industry", "months", "raw", "adjusted")]
  )
  assoc$industry <- factor(assoc$industry, levels = rev(unname(categories)))
  assoc$key <- assoc$category %in% survives$category
  detrended$plot <- ggplot(assoc, aes(y = industry)) +
    geom_vline(xintercept = 0, colour = quiet) +
    geom_segment(aes(x = raw, xend = adjusted, yend = industry), colour = grey_bar, linewidth = 1) +
    geom_point(aes(x = raw), colour = grey_bar, size = 3.2) +
    geom_point(aes(x = adjusted, colour = key), size = 3.2) +
    scale_colour_manual(values = c("TRUE" = orange, "FALSE" = "#d99a74"), guide = "none") +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .5)) +
    labs(x = "Correlation with restriction stringency (grey raw, orange detrended)", y = NULL) +
    chart_theme + theme(panel.grid.major.y = element_blank())
  detrended$alt <- paste("Dot plot of correlations between Korean year-over-year spending growth and restriction stringency by industry, raw and after detrending.",
                         paste(sprintf("%s raw %.2f, detrended %.2f", assoc$industry, assoc$raw, assoc$adjusted), collapse = "; "))
  detrended$table <- data.frame(Industry = detrended$data$industry, Months = detrended$data$months,
                                "Raw Pearson" = detrended$data$raw, "Detrended Pearson" = detrended$data$adjusted,
                                check.names = FALSE)

  # (f) Taiwan's February 2021 category step -----------------------------------
  feb_ratio <- function(category, year) {
    g <- taiwan[taiwan$category == category, ]
    g$amount_local[g$month == year * 100 + 2] / g$amount_local[g$month == year * 100 + 1]
  }
  years <- 2015:2022
  ratios <- data.frame(year = rep(years, 2), industry = rep(c("Food and dining", "General retail"), each = length(years)),
                       ratio = c(vapply(years, feb_ratio, numeric(1), category = "food"),
                                 vapply(years, feb_ratio, numeric(1), category = "retail_broad")))
  food_r <- ratios$ratio[ratios$industry == "Food and dining"]
  retail_r <- ratios$ratio[ratios$industry == "General retail"]
  together <- sign(food_r - 1) == sign(retail_r - 1)
  taiwan_share <- subset(taiwan, month >= 202002 & month <= 202201)
  taiwan_share$share <- 100 * taiwan_share$amount_local / ave(taiwan_share$amount_local, taiwan_share$month, FUN = sum)
  share <- function(category, after) mean(taiwan_share$share[taiwan_share$category == category &
                                                                (taiwan_share$month >= 202102) == after])
  shift_story <- list(
    headline = sprintf("Data caveat: in February 2021 Taiwan's food spending fell to %.2f× January while general retail rose to %.2f×; in every other year since 2015 the two moved together",
                       food_r[years == 2021], retail_r[years == 2021]),
    takeaway = sprintf(paste(
      "January-to-February spending ratios; Lunar New Year shifts move both in the same direction in other years.",
      "From February 2021 the industry mix steps at once in every city and county (food falls in all 22; general retail rises in all but Matsu), and food's share of the seven-industry total drops from %.1f%% to %.1f%% while general retail's rises from %.1f%% to %.1f%% (12-month means).",
      "The source has published no notice of a classification change at that date, but the pattern is what a change in merchant-to-industry mapping would produce and is unlike a consumption shift, so Taiwan comparisons across that month are not shown."),
      share("food", FALSE), share("food", TRUE), share("retail_broad", FALSE), share("retail_broad", TRUE)),
    checks = list(opposite_2021 = !together[years == 2021], together_otherwise = all(together[years != 2021])),
    data = ratios
  )
  ratios$highlight <- ratios$year == 2021
  shift_story$plot <- ggplot(ratios, aes(year, ratio, colour = industry, shape = industry)) +
    geom_hline(yintercept = 1, colour = quiet) +
    geom_vline(xintercept = 2021, colour = "#ececec", linewidth = 10) +
    geom_line(linewidth = .7) + geom_point(size = 3) +
    scale_colour_manual(values = c("Food and dining" = orange, "General retail" = blue), name = NULL) +
    scale_shape_manual(values = c("Food and dining" = 16, "General retail" = 17), name = NULL) +
    scale_x_continuous(breaks = years) + scale_y_log10(breaks = c(.5, .75, 1, 1.25, 1.5)) +
    labs(x = NULL, y = "Taiwan, February ÷ January spending (log scale)") +
    chart_theme + theme(legend.position = "top", legend.justification = "left", panel.grid.minor.x = element_blank())
  shift_story$alt <- paste("Line chart of Taiwan's February-to-January spending ratio for food and general retail, 2015 to 2022.",
                           paste(sprintf("%d food %.2f, retail %.2f", years, food_r, retail_r), collapse = "; "))
  shift_story$table <- data.frame(Year = years, "Food and dining Feb ÷ Jan" = food_r,
                                  "General retail Feb ÷ Jan" = retail_r, check.names = FALSE)

  conclusion <- sprintf(paste(
    "Answer, in three parts. First, not case counts: both countries' largest waves came in 2022, when spending mostly grew (Korean food and dining in %d of 12 months).",
    "Second, restrictions: spending dipped when rules were strict, and the timing followed each country's rules, through 2020–2021 in Korea (%s %s in 2020) and in Taiwan's short 2021 alert (%s %s from April to June).",
    "Third, the hit was uneven and restrictions do not explain everything: barely restricted Taiwan saw %s fall %.1f%% in 2020, and after detrending only %s keeps a link to stringency.",
    "Limits: 36 national months per country, shared trends and seasonality, nominal amounts, approximate industry matches, and a Taiwan source break that hides its alert from year-over-year comparisons. These are associations, not causal effects."),
    food_up[["Korea"]], tolower(korea_low$industry), pct(korea_low$change), tolower(hit$industry[1]), pct(hit$y2021[1]),
    tolower(taiwan_low$industry), abs(taiwan_low$change),
    if (nrow(survives)) sprintf("Korean %s (r %.2f → %.2f)", paste(tolower(survives$industry), collapse = " and "),
                                survives$raw[1], survives$adjusted[1]) else "no Korean industry")

  list(paths = paths, cases = cases_story, restrictions = restrictions, industries = industries,
       detrended = detrended, "break" = shift_story, conclusion = conclusion, survivors = survives$category,
       alert = alert, peak_growth = peak_growth)
}

findings_ui <- function() {
  finding <- function(id) {
    plots <- list(plotOutput(paste0("finding_", id), height = "auto"))
    if (id == "restrictions") plots <- c(plots, list(plotOutput("finding_restrictions_2", height = "auto")))
    tags$section(class = "analysis-section finding",
      tags$h2(textOutput(paste0("finding_", id, "_headline"), inline = TRUE)),
      tags$figure(plots,
        tags$figcaption(class = "chart-note", textOutput(paste0("finding_", id, "_takeaway"), inline = TRUE))),
      tags$details(class = "methods finding-data",
        tags$summary("Figure data"),
        tags$div(class = "table-scroll", tabindex = "0", role = "region", "aria-label" = "Scrollable figure data",
                 tableOutput(paste0("finding_", id, "_table")))))
  }
  tagList(
    tags$p(class = "findings-question", research_question),
    tags$p(class = "chart-note findings-intro",
           "Six figures, one idea each: the restriction and case paths, then cases against spending, restrictions against spending, the industries hit, what survives detrending, and one data caveat. COVID-19 measures are national; card spending is nationwide, January 2020 – December 2022."),
    lapply(finding_ids, finding),
    tags$section(class = "analysis-section finding",
      tags$h2("What this means, and its limits"),
      tags$p(textOutput("findings_conclusion", inline = TRUE)),
      downloadLink("findings_download", "Download all figure data (CSV)")))
}

findings_server <- function(input, output, findings) {
  heights <- c(paths = 420, cases = 380, restrictions = 260, industries = 380, detrended = 340, "break" = 340)
  lapply(finding_ids, function(id) {
    f <- findings[[id]]
    output[[paste0("finding_", id, "_headline")]] <- renderText(f$headline)
    output[[paste0("finding_", id, "_takeaway")]] <- renderText(f$takeaway)
    output[[paste0("finding_", id)]] <- renderPlot(f$plot, height = heights[[id]], alt = f$alt)
    output[[paste0("finding_", id, "_table")]] <- renderTable(f$table, digits = 1, spacing = "s", width = "100%",
                                                              na = "—", format.args = list(big.mark = ","))
  })
  output$finding_restrictions_2 <- renderPlot(findings$restrictions$plot2, height = 340,
                                              alt = findings$restrictions$alt2)
  output$findings_conclusion <- renderText(findings$conclusion)
  output$findings_download <- downloadHandler(
    filename = "key-findings-data.csv",
    content = function(file) {
      rows <- do.call(rbind, lapply(finding_ids, function(id) {
        t <- findings[[id]]$table
        data.frame(finding = id, row = rep(seq_len(nrow(t)), ncol(t)), column = rep(names(t), each = nrow(t)),
                   value = unlist(lapply(t, as.character), use.names = FALSE))
      }))
      write.csv(rows, file, row.names = FALSE, na = "")
    }
  )
}
