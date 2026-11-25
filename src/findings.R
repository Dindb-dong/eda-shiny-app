# Key findings: a fixed, scrolling story built from the final data.
# Every headline number is computed here, so the text cannot drift from the data.
# Depends on src/analysis.R, src/plots.R and src/covid_section.R (country colours).

finding_ids <- c("paths", "industries", "food", "detrended", "break")
pct <- function(x) sprintf("%+.1f%%", x)
# Locale-independent month names for generated sentences.
month_year <- function(date) paste(month.name[as.integer(format(date, "%m"))], format(date, "%Y"))
grey_bar <- "#bdbdbd"

build_findings <- function(spending, covid, categories) {
  national <- subset(spending, capital_area == "national")
  label <- function(category) unname(categories[category])
  population <- c(Korea = covid_population[["KOR"]], Taiwan = covid_population[["TWN"]])
  covid$year <- as.integer(format(covid$covid_date, "%Y"))

  # 1. Epidemic and restriction paths ------------------------------------------
  early <- subset(covid, year <= 2021)
  mean_stringency <- tapply(early$stringency, early$country, mean)
  strict_months <- tapply(covid$stringency >= 50, covid$country, sum)
  strict_years <- tapply(covid$year[covid$stringency >= 50], covid$country[covid$stringency >= 50],
                         function(y) paste(unique(y), collapse = ", "))
  early_cases <- tapply(early$cases, early$country, sum, na.rm = TRUE)
  cases_by_year <- tapply(covid$cases, list(covid$country, covid$year), sum, na.rm = TRUE)
  apparel <- subset(national, country == "Taiwan" & category == "apparel")
  apr_jun <- function(y) 100 * (apparel$amount_local[apparel$month == y * 100 + 6] /
                                  apparel$amount_local[apparel$month == y * 100 + 4] - 1)
  paths <- list(
    headline = sprintf("Korea kept restrictions stricter for longer: average stringency %.0f against %.0f in Taiwan over 2020–2021",
                       mean_stringency[["Korea"]], mean_stringency[["Taiwan"]]),
    takeaway = sprintf(paste(
      "Korea spent %d months at a stringency of 50 or more; Taiwan spent %d, all in %s, around its Level 3 alert.",
      "Taiwan reported %s cases in 2020–2021 against Korea's %s. In that alert, Taiwan's apparel spending fell %.0f%% from April to June 2021, compared with %.0f%% over the same months of 2019.",
      "Both countries' largest waves came in 2022, after restrictions eased."),
      strict_months[["Korea"]], strict_months[["Taiwan"]], strict_years[["Taiwan"]],
      format(early_cases[["Taiwan"]], big.mark = ","), format(early_cases[["Korea"]], big.mark = ","),
      abs(apr_jun(2021)), abs(apr_jun(2019))),
    data = covid[c("country", "covid_date", "stringency", "cases")],
    checks = list(korea_stricter = mean_stringency[["Korea"]] > mean_stringency[["Taiwan"]],
                  waves_2022 = all(cases_by_year[, "2022"] > cases_by_year[, "2021"]),
                  eased_2022 = all(tapply(covid$stringency[covid$year == 2022], covid$country[covid$year == 2022], mean) <
                                     tapply(covid$stringency[covid$year == 2021], covid$country[covid$year == 2021], mean)),
                  alert_drop = apr_jun(2021) < apr_jun(2019) - 20)
  )
  paths$plot <- ggplot(paths$data, aes(covid_date, stringency, colour = country, linetype = country)) +
    geom_hline(yintercept = 50, colour = grey_bar, linewidth = .4) +
    geom_step(linewidth = .85, direction = "mid") +
    scale_colour_manual(values = covid_country_colors, name = NULL) +
    scale_linetype_manual(values = covid_country_lines, name = NULL) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
    labs(x = NULL, y = "Monthly mean stringency") +
    chart_theme + theme(legend.position = "top", legend.justification = "left")
  paths$alt <- sprintf("Line chart of monthly restriction stringency, January 2020 to December 2022. Korea averages %.0f and Taiwan %.0f over 2020–2021; Taiwan is above 50 only in %s.",
                       mean_stringency[["Korea"]], mean_stringency[["Taiwan"]], strict_years[["Taiwan"]])
  paths$table <- data.frame(Country = names(mean_stringency),
                            "Mean stringency 2020–2021" = as.numeric(mean_stringency),
                            "Months with stringency ≥ 50" = as.integer(strict_months[names(mean_stringency)]),
                            "Cases 2020–2021" = as.numeric(early_cases[names(mean_stringency)]),
                            "Cases 2022" = as.numeric(cases_by_year[names(mean_stringency), "2022"]),
                            "Cases per million 2022" = as.numeric(cases_by_year[names(mean_stringency), "2022"]) /
                              population[names(mean_stringency)] * 1e6,
                            check.names = FALSE)

  # 2. Spending in 2020 by industry --------------------------------------------
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
  korea_falls <- change[change$country == "Korea" & change$change < 0, ]
  korea_falls <- korea_falls[order(korea_falls$change), ]
  industries <- list(
    headline = sprintf("In 2020, %s fell most in Korea (%s) and %s fell most in Taiwan (%s)",
                       tolower(korea_low$industry), pct(korea_low$change),
                       tolower(taiwan_low$industry), pct(taiwan_low$change)),
    takeaway = sprintf(paste(
      "Annual 2020 spending against 2019 in each country's own currency. Korea's falls were in %s.",
      "In Taiwan, with few restrictions, %s rose %s. These are levels before February 2021, so Taiwan's category shift does not affect them."),
      paste(sprintf("%s (%s)", tolower(korea_falls$industry), pct(korea_falls$change)), collapse = ", "),
      tolower(taiwan_high$industry), pct(taiwan_high$change)),
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

  # 3. Korea food and dining against stringency ----------------------------------
  food <- subset(national, country == "Korea" & category == "food")
  food$value <- food$yoy
  food_pairs <- pair_months(food, covid, "stringency")
  food_cases <- pair_months(food, covid, "cases")
  r_food <- cor(food_pairs$x, food_pairs$value)
  rho_cases <- cor(food_cases$x, food_cases$value, method = "spearman")
  deepest <- food_pairs[which.min(food_pairs$value), ]
  positive_2022 <- sum(food_pairs$value[food_pairs$month %/% 100 == 2022] > 0)
  food_story <- list(
    headline = sprintf("Korean food and dining spending moved opposite to restrictions (r = %.2f over %d months)",
                       r_food, nrow(food_pairs)),
    takeaway = sprintf(paste(
      "The deepest fall, %s year over year in %s, came at a stringency of %.0f.",
      "Spending grew in %d of 12 months of 2022 as restrictions eased, partly a rebound from a weak 2021, even while Omicron cases surged, so cases and spending rose together (Spearman %+.2f).",
      "Cases are a poor stand-in for restrictions."),
      pct(deepest$value), month_year(deepest$date), deepest$stringency, positive_2022, rho_cases),
    checks = list(negative = r_food < -.5, cases_positive = rho_cases > 0)
  )
  long <- rbind(data.frame(date = food_pairs$date, value = food_pairs$stringency,
                           panel = "Restriction stringency (0–100)"),
                data.frame(date = food_pairs$date, value = food_pairs$value,
                           panel = "Food and dining spending, year-over-year growth (%)"))
  long$panel <- factor(long$panel, levels = unique(long$panel))
  references <- data.frame(panel = factor(levels(long$panel), levels = levels(long$panel)), y = c(50, 0))
  food_story$plot <- ggplot(long, aes(date, value)) +
    geom_hline(data = references, aes(yintercept = y), colour = grey_bar) +
    geom_line(aes(colour = panel), linewidth = .9) +
    geom_point(data = transform(deepest, panel = factor(levels(long$panel)[2], levels = levels(long$panel))),
               aes(date, value), colour = orange, size = 3) +
    scale_colour_manual(values = setNames(c(quiet, orange), levels(long$panel)), guide = "none") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    facet_wrap(vars(panel), ncol = 1, scales = "free_y") +
    labs(x = NULL, y = NULL) + chart_theme
  food_story$alt <- sprintf("Two stacked line charts for Korea, January 2020 to December 2022: restriction stringency above, food and dining year-over-year spending growth below. Pearson correlation %.2f; the deepest fall is %s in %s.",
                            r_food, pct(deepest$value), month_year(deepest$date))
  food_story$table <- data.frame(Month = format(food_pairs$date, "%Y-%m"), Stringency = food_pairs$stringency,
                                 "Food and dining YoY (%)" = food_pairs$value,
                                 Cases = food_cases$cases[match(food_pairs$month, food_cases$month)],
                                 check.names = FALSE)

  # 4. Raw vs detrended association, Korea ----------------------------------------
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
      "Grey dots are raw Pearson correlations of year-over-year growth with stringency; orange dots remove a linear trend and one annual cycle from both series (%d months).",
      "%s %s",
      "Taiwan is left out: its strict months (May–July 2021) fall inside the masked break window, so its year-over-year growth cannot be compared there."),
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

  # 5. Taiwan's category mix shift ---------------------------------------------
  taiwan <- subset(national, country == "Taiwan" & month >= 201901)
  taiwan$share <- 100 * taiwan$amount_local / ave(taiwan$amount_local, taiwan$month, FUN = sum)
  window_share <- function(category, from, to) mean(taiwan$share[taiwan$category == category & taiwan$month >= from & taiwan$month <= to])
  shift <- data.frame(category = names(categories), industry = unname(categories),
                      before = vapply(names(categories), window_share, numeric(1), 202002, 202101),
                      after = vapply(names(categories), window_share, numeric(1), 202102, 202201))
  shift_story <- list(
    headline = sprintf("Taiwan's category mix jumps in February 2021: food falls from %.1f%% to %.1f%% of spending while general retail rises from %.1f%% to %.1f%%",
                       shift$before[shift$category == "food"], shift$after[shift$category == "food"],
                       shift$before[shift$category == "retail_broad"], shift$after[shift$category == "retail_broad"]),
    takeaway = paste("Shares are 12-month means before (Feb 2020 – Jan 2021) and after (Feb 2021 – Jan 2022) the shift.",
                     "A one-month step this large is consistent with a reclassification of merchants by the source rather than a COVID-19 effect.",
                     "The dashboard therefore leaves Taiwan's year-over-year growth missing from February 2021 to January 2022 in every industry."),
    data = shift
  )
  taiwan$highlight <- ifelse(taiwan$category == "food", "Food and dining",
                             ifelse(taiwan$category == "retail_broad", "General retail", "Other industries"))
  shift_story$plot <- ggplot(taiwan, aes(date, share, group = category, colour = highlight)) +
    geom_vline(xintercept = as.Date("2021-02-01"), linetype = 3, colour = quiet) +
    geom_line(data = subset(taiwan, highlight == "Other industries"), linewidth = .6) +
    geom_line(data = subset(taiwan, highlight != "Other industries"), linewidth = 1.1) +
    scale_colour_manual(values = c("Food and dining" = orange, "General retail" = blue, "Other industries" = grey_bar),
                        name = NULL) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(x = NULL, y = "Share of Taiwan's seven-industry total (%)") +
    chart_theme + theme(legend.position = "top", legend.justification = "left")
  shift_story$alt <- sprintf("Line chart of each industry's share of Taiwan's card spending, January 2019 to December 2022, with a dotted line at February 2021. Food falls from %.1f%% to %.1f%% and general retail rises from %.1f%% to %.1f%%.",
                             shift$before[1], shift$after[1], shift$before[shift$category == "retail_broad"],
                             shift$after[shift$category == "retail_broad"])
  shift_story$table <- data.frame(Industry = shift$industry, "Share Feb 2020 – Jan 2021 (%)" = shift$before,
                                  "Share Feb 2021 – Jan 2022 (%)" = shift$after, check.names = FALSE)

  conclusion <- sprintf(paste(
    "%s, and %s posted Korea's largest 2020 fall (%s).",
    "In Taiwan, 2020 hit %s (%s) and %s (%s) without strict rules, and apparel dropped sharply during the 2021 alert.",
    "For planning, restriction levels, not case counts, track food and dining spending.",
    "Limits: 36 national months per country, shared trends and seasonality, nominal amounts, approximate industry matches, and a Taiwan source break that hides its 2021 alert from year-over-year comparisons. These are associations, not causal effects."),
    if (nrow(survives)) {
      sprintf("Restriction-sensitive spending, in this data, means Korean %s: the only link to stringency that survives detrending (r = %.2f)",
              paste(tolower(survives$industry), collapse = " and "), survives$adjusted[1])
    } else "No Korean industry keeps a clear link to stringency after detrending",
    tolower(korea_low$industry), pct(korea_low$change),
    tolower(taiwan_low$industry), pct(taiwan_low$change),
    tolower(taiwan_second$industry), pct(taiwan_second$change))

  list(paths = paths, industries = industries, food = food_story, detrended = detrended, "break" = shift_story,
       conclusion = conclusion, survivors = survives$category)
}

findings_ui <- function() {
  finding <- function(id) {
    tags$section(class = "analysis-section finding",
      tags$h2(textOutput(paste0("finding_", id, "_headline"), inline = TRUE)),
      tags$figure(
        plotOutput(paste0("finding_", id), height = "auto"),
        tags$figcaption(class = "chart-note", textOutput(paste0("finding_", id, "_takeaway"), inline = TRUE))),
      tags$details(class = "methods finding-data",
        tags$summary("Figure data"),
        tags$div(class = "table-scroll", tabindex = "0", role = "region", "aria-label" = "Scrollable figure data",
                 tableOutput(paste0("finding_", id, "_table")))))
  }
  tagList(
    tags$p(class = "chart-note findings-intro",
           "Five figures, one idea each, from the restriction paths to the industries that moved with them. Monthly COVID-19 measures are national; card spending is nationwide, January 2020 – December 2022."),
    lapply(finding_ids, finding),
    tags$section(class = "analysis-section finding",
      tags$h2("What this means, and its limits"),
      tags$p(textOutput("findings_conclusion", inline = TRUE)),
      downloadLink("findings_download", "Download all figure data (CSV)")))
}

findings_server <- function(input, output, findings) {
  heights <- c(paths = 340, industries = 380, food = 460, detrended = 340, "break" = 380)
  lapply(finding_ids, function(id) {
    f <- findings[[id]]
    output[[paste0("finding_", id, "_headline")]] <- renderText(f$headline)
    output[[paste0("finding_", id, "_takeaway")]] <- renderText(f$takeaway)
    output[[paste0("finding_", id)]] <- renderPlot(f$plot, height = heights[[id]], alt = f$alt)
    output[[paste0("finding_", id, "_table")]] <- renderTable(f$table, digits = 1, spacing = "s", width = "100%",
                                                              na = "—", format.args = list(big.mark = ","))
  })
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
