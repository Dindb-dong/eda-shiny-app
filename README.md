<div align="center">

<h1>COVID-19 and Consumer Spending: A Comparative Exploratory Analysis of South Korea and Taiwan (2021–2022)</h1>

<p><strong>Two economies. Three lenses. One question about how spending changed.</strong></p>

<img alt="Stack: R and Shiny" src="https://img.shields.io/badge/R-Shiny-276DC3?style=flat-square">
<img alt="Course: STA3115.01-00" src="https://img.shields.io/badge/STA3115.01--00-Final%20Project-0F766E?style=flat-square">

<p><strong>EXPLORATORY DATA ANALYSIS (STA3115.01-00)</strong></p>

<p><a href="#research-question">Research Question</a> · <a href="#dashboard-tour">Dashboard Tour</a> · <a href="#course-methods">Course Methods</a> · <a href="#data-sources">Data Sources</a> · <a href="#analysis-approach">Analysis Approach</a></p>

</div>

## Research Question

> How are COVID-19 cases, deaths, and government restriction stringency associated with changes in credit card spending in South Korea and Taiwan during 2021–2022?

The R Shiny dashboard joins each country's card spending to its own COVID-19 series. It covers seven spending categories and uses national COVID-19 measures for each selected card-spending area.

## Run locally

Run from the repository root with R installed.

```r
install.packages(c("shiny", "ggplot2", "dplyr", "readr"))
shiny::runApp()
```

Run `Rscript tests/check-app.R` from the same directory to render the documented dashboard views and check the monthly joins, date cutoffs, forecast inputs, and CSV download. Run `Rscript tests/check-card-section.R` for the nationwide card-comparison calculations, chart options, and CSV export.

## Dashboard Tour

Select **COVID-19 comparison** in the left navigation to compare daily cases, deaths, and restriction stringency in Korea and Taiwan. It shows 7-day averages per million residents above the daily Stringency Index, with optional policy-date markers, a same-day scatter against stringency, a summary by country, and a CSV download.

The daily rates use fixed 2021 denominators: Korea's projected population of 51,744,876 ([Statistics Korea population table](https://mods.go.kr/boardDownload.es?bid=5110&list_no=420972&seq=1)) and Taiwan's year-end registered population of 23,375,314 ([Ministry of the Interior](https://www.ris.gov.tw/info-liferay/app/channel/newsEnglishDetail/22010345)). These national population definitions differ and are held constant across 2021–2022. Seven-day means require all seven calendar days, and negative reporting corrections are retained in both daily rates and monthly totals. A selected-period total is unavailable if any daily report is missing.

Policy markers follow official announcements: [Taiwan's nationwide Level 3 alert, 19 May 2021](https://www.cdc.gov.tw/Category/ListContent/DORF6Hp-BCcnAc7p4PDSGQ?uaid=VN_6yeoBTKhRKoSy2d0hJQ), [Korea's gradual recovery phase, 1 November 2021](https://www.mohw.go.kr/gallery.es?act=view&bid=0003&list_no=368423&mid=a10605040000), [Korea's distancing relaxation, 18 April 2022](https://www.korea.net/Government/Briefing-Room/Press-Releases/view?articleId=371146&insttCode=A260111&type=N), and [Taiwan's end of arrival quarantine, 13 October 2022](https://www.cdc.gov.tw/En/Bulletin/Detail/nEBLnOPooDYFy_CBJqfWvg?typeid=158). They provide context rather than estimates of policy effects.

Select **Card spending comparison** to open the nationwide comparison described below. Select **COVID-19 ↔ Card spending** to open the five views below.

| Tab | Question | Views |
| :-- | :-- | :-- |
| **Industry overview** | Which industries rose or fell, and where did capital and non-capital trends diverge? | Monthly heatmaps of year-over-year growth and the same-month capital minus non-capital gap in percentage points. All seven industries appear together. Select a period and country, then open an industry’s scatterplot or spending timeline. |
| **Relationship** | Within each country, how did spending move with reported cases, deaths, or restrictions? | Linear and LOESS scatter fits, aligned series, 0–3 month lag profile, trend- and season-adjusted association, stringency bands, and descriptive ordinal and logistic spending outcomes. |
| **Before and since onset** | How did spending change in the capital area and elsewhere? | Timeline, calendar heatmap, ECDF and boxplots, pre-COVID trend projection, three forecasting methods, and segmented regression with AR(1) errors. |
| **Diagnostics** | Do seasonality, autocorrelation, distribution shape, or influential months change the interpretation? | Robust STL, ACF and PACF, normal and two-period Q–Q, quantile mean–difference, location–spread, Box–Cox, residuals, and Cook’s distance. |
| **Taiwan age profile** | How did the typical transaction amount vary by age and month? | Observed, ordinary and robust bivariate LOESS, and residual surfaces evaluated within the observed age-month grid. |

The spending filters use the 2019 index, year-over-year and month-over-month growth, a gap from the 2014–2019 calendar-month median, a 12-month log difference, or a combined first and 12-month log difference. Each transformation is computed before applying any period filter.

**Date coverage.** The card-only views use Jan 2014–Dec 2022, with Jan 2020 as the common cutoff. The relationship views can pair only Jan 2021–Dec 2022. A positive lead means the COVID-19 measure is from an earlier calendar month than the spending observation. No COVID-19 values are filled in for earlier years.

**Forecast and break models.** The fixed 2019 holdout trains through 2018 and compares seasonal naïve, additive Holt–Winters on log spending, and log-scale SARIMA (0,1,1)(0,1,1)[12] using MAE and RMSE in index points. A separate 2020–2022 projection refits through 2019. The segmented model estimates the January 2020 level break and change in monthly log slope with calendar-month effects and AR(1) errors. Both serve as descriptive historical references. Concurrent shocks and coverage changes can affect the apparent break.

### Nationwide card spending comparison

**Card spending comparison** integrates the standalone app supplied in [PR #5](https://github.com/Dindb-dong/eda-shiny-app/pull/5) into the existing navigation and report layout. The shared card/CPI window is January 2014–August 2023. Choose the total or any of eight categories, either country or both, and monthly values, a trailing 12-month average, or year-over-year growth. The default is a CPI-adjusted index with each country-category's 2014 monthly average equal to 100. Nominal indices and local currency amounts are also available; different currencies have separate panels.

The seven non-EC categories use offline spending. Taiwan e-commerce sums the online components of those seven categories; Korea supplies a separate EC series. Excluding Other also removes Taiwan's online Other but cannot do so for Korea's pre-aggregated EC series. The UI explains this asymmetry. Category labels and source coverage are approximate matches; this separate nationwide extract and its 2014 baseline differ from the joint analysis’s regional extract and 2019 baseline.

All indices, moving averages, and calendar-year comparisons are computed before clipping the displayed dates. Missing months remain missing rather than shifting the year-over-year lag or being counted as zero. The summary and CSV expose the selected monthly data, including original amounts, CPI, unit treatment and the Other-exclusion setting. Optional COVID shading and supplied reference restriction windows are chart annotations, not estimated treatment effects.

The original data files and their available provenance are in [`data/card_comparison/`](data/card_comparison/README.md). The contributor identifies Taiwan's card source as NCCC but has not yet supplied the Korean card and CPI source references.

## Course Methods

The original STA3115 course map below records candidate extensions and their interpretation. See the dashboard tour for the methods available in the app.

| Course topic | Planned plot or analysis | What to look for |
| :-- | :-- | :-- |
| **Tidy data and reproducibility** · Units 1–2 | Prepare tidy country–date and country–month–category tables. Use an R Markdown analysis to document the transformations and regenerate the figures. | Trace each chart back to its observations, units, and missing values. |
| **Univariate EDA and robust summaries** · Units 1, 3 | Use a stem-and-leaf display for a selected monthly series, histograms and densities with rug marks for daily COVID-19 metrics, and ECDFs, quantile functions, and boxplots for spending growth. Compare the mean and SD with the median and IQR. | See skew, tail behavior, and how unusual months affect summaries. |
| **Distribution comparison and transformation** · Units 3–6 | Use two-sample Q–Q and Tukey mean–difference plots to compare spending growth across countries. Use a normal Q–Q plot to diagnose shape and a location–spread plot to check whether variability grows with typical spending. Try log or Box–Cox transformations on strictly positive spending amounts. | Distinguish shifts in level from changes in spread, and see what changes after transformation. |
| **Bivariate EDA and flexible fits** · Units 5–8 | Overlay linear and LOESS fits on spending-versus-COVID scatterplots by country and category. Compare LOESS spans and degrees, and try a low-degree polynomial or a prespecified period indicator as a reference. Inspect residuals against fitted values and over time. | Check for curvature, isolated months, and temporal structure that the fitted lines miss. |
| **Trivariate EDA and modified LOESS** · Units 9–11 | Use coplots of spending growth versus cases across overlapping Stringency Index ranges, where enough observations are available. Compare ordinary and modified two-predictor LOESS for Taiwan's mean transaction amount by month and age, using the observed age bands with midpoints. Display fitted cross-sections, level plots, contour plots, and residuals. | Check whether a relationship changes with a third variable and where the fitted surface has observation support. |
| **Binary response** · Units 12–13 | Explore months with negative year-over-year growth using observed proportions and a simple logistic curve against stringency. | See whether the pattern for spending declines agrees with the continuous growth analysis. |
| **Categorical association** · Units 14–15 | Cross-tabulate decline versus non-decline by a prespecified stringency band. Compare the pooled mosaic with country-conditioned cotab plots and a doubledecker plot for a selected category. Inspect cell counts, Pearson residuals, and the chi-squared statistic where cell counts support it. | See whether the pooled association changes after conditioning on country. |
| **Ordered categorical response** · Unit 16 | Try prespecified bands for decline, near-flat growth, and growth. Explore their distribution across stringency with a filled density plot. Consider a proportional odds model if the bands have enough observations and the assumption is reasonable. | See how the balance of growth outcomes shifts with stringency and whether conclusions depend on the band cutoffs. |

The binary, categorical, and ordinal panels are candidates for the joint tab. Keep the continuous outcome as the main comparison. Show sample and cell counts, and treat contingency tables as descriptive because months can be serially dependent. Restrict fitted surfaces to regions supported by observations.

## Data Sources

| Dataset | Source | Coverage and role |
| :-- | :-- | :-- |
| Reported COVID-19 cases and deaths | [Johns Hopkins University CSSE](https://github.com/CSSEGISandData/COVID-19) | Daily, 2021–2022. Compare when waves in reported cases and deaths occurred. |
| Government restriction stringency | [Our World in Data](https://docs.owid.io/projects/etl/api/covid/) · Oxford COVID-19 Government Response Tracker | Daily, 2021–2022. The 0–100 Stringency Index describes policy strictness. |
| South Korean card spending | [Bank of Korea ECOS](https://ecos.bok.or.kr/) · series `601Y002` | Monthly, 2019–2022. Personal credit card spending by category. |
| Taiwanese card spending | [National Credit Card Center (NCCC)](https://www.nccc.com.tw/wps/wcm/connect/zh/home/openinformation/PageIntroduce) · series `C02` | Monthly, 2019–2022. Published spending categories include credit and debit card transactions. |

- [`data/covid_daily.csv`](data/covid_daily.csv) contains 1,460 country-day observations for South Korea and Taiwan in 2021–2022. The cases and deaths are from [JHU CSSE COVID-19 Data](https://github.com/CSSEGISandData/COVID-19) (Johns Hopkins University, 2020). The Stringency Index is from the [Oxford COVID-19 Government Response Tracker](https://github.com/OxCGRT/covid-policy-dataset), Blavatnik School of Government, University of Oxford, via [Our World in Data](https://docs.owid.io/projects/etl/api/covid/). Both sources are licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). We standardized country names and codes and calculated daily case and death counts by differencing cumulative totals. Negative reporting corrections are retained.
- [`data/common_panel.csv`](data/common_panel.csv) contains 192 observations of monthly food and apparel spending proxies in 2019–2022. The Korean data come from the Bank of Korea's ECOS table `601Y002`, published under its [Payment and Settlement Statistics API terms](https://www.data.go.kr/data/15059632/openapi.do). The Taiwanese data come from NCCC `C02`, published by Taiwan's Financial Supervisory Commission Banking Bureau (2016) as [card spending by age group and industry, dataset 38321](https://data.gov.tw/en/datasets/38321), under the [Open Government Data License, version 1.0](https://data.gov.tw/en/license). For food we combine Korean grocery and restaurant spending (`1300`, `2220`) and compare it with Taiwan's `FD` category. For apparel we combine Korean clothing/textiles and apparel accessories (`1410`, `1420`) and compare them with Taiwan's `CT` category. The Taiwan series sum the published age-group amounts. Each country-category series is indexed to its own 2019 monthly mean of 100, with a separate same-month 2019 index and year-over-year growth rate.

Additional CSVs from the project bundle are available in [`data/`](data/).

| Files | Contents and provenance |
| :-- | :-- |
| [`raw/`](data/raw/) | The 30 separate NCCC downloads, including the two website cross-checks. Taiwan Financial Supervisory Commission Banking Bureau datasets [38321 (2016), spending by age](https://data.gov.tw/en/datasets/38321), [38316 (2016), spending by income](https://data.gov.tw/dataset/38316), [79289 (2018), men's spending by age](https://data.gov.tw/en/datasets/79289), and [79290 (2018), women's spending by age](https://data.gov.tw/en/datasets/79290) are available under the [Open Government Data License, version 1.0](https://data.gov.tw/en/license). |
| [`korea_national_2019_2022.csv`](data/korea_national_2019_2022.csv), [`taiwan_national_2019_2022.csv`](data/taiwan_national_2019_2022.csv), [`taiwan_demographics_2019_2022.csv`](data/taiwan_demographics_2019_2022.csv) | Country-specific monthly card spending from ECOS and NCCC under the terms above. The demographic file adds mean transaction amounts and age-band midpoints where available. |
| [`card_spending_pre_covid_2014_2019.csv`](data/card_spending_pre_covid_2014_2019.csv) | National monthly spending before COVID-19 for seven categories. Uses ECOS national totals and NCCC C02 Taiwan totals. The 2019 food and apparel amounts match `common_panel.csv`. |
| [`card_spending_by_capital_area_pre_covid_2014_2019.csv`](data/card_spending_by_capital_area_pre_covid_2014_2019.csv), [`card_spending_by_capital_area_since_covid_onset_2020_2022.csv`](data/card_spending_by_capital_area_since_covid_onset_2020_2022.csv) | Monthly local-currency spending by capital area for seven categories before and since the onset of COVID-19. See the definitions below. |
| [`industry_crosswalk.csv`](data/industry_crosswalk.csv), [`candidate_panel.csv`](data/candidate_panel.csv) | Category mappings and candidate comparisons derived from the ECOS and NCCC series. The narrower food and apparel comparison is in `common_panel.csv`. |
| [`annual_changes.csv`](data/annual_changes.csv), [`analysis_metrics.csv`](data/analysis_metrics.csv), [`reconciliation.csv`](data/reconciliation.csv), [`robust_summary.csv`](data/robust_summary.csv) | Project summaries and data checks derived from the card spending inputs. |
| [`excess_mortality.csv`](data/excess_mortality.csv) | South Korea and Taiwan observations from [Our World in Data](https://ourworldindata.org/grapher/excess-mortality-p-scores), based on the [Human Mortality Database Short-term Mortality Fluctuations series](https://www.mortality.org/Data/STMF) (CC BY 4.0), [Karlinsky and Kobak's World Mortality Dataset](https://github.com/akarlinsky/world_mortality) (MIT) and [baseline estimates](https://github.com/dkobak/excess-mortality), and [OWID population estimates](https://ourworldindata.org/population-sources) (CC BY 4.0). |
| [`taiwan_retail_indices_monthly.csv`](data/taiwan_retail_indices_monthly.csv) | Taiwan Ministry of Economic Affairs, Department of Statistics (2015), [Wholesale, Retail and Food Services Turnover Index, dataset 16365](https://data.gov.tw/dataset/16365), under the [Open Government Data License, version 1.0](https://data.gov.tw/en/license). This extract covers 2021–2022 and adds an ISO date column. |
| [`macro_annual_adb.csv`](data/macro_annual_adb.csv) | 28 annual observations for South Korea and Taiwan in 2021–2022, covering GDP growth, consumer prices, unemployment, and household consumption. Asian Development Bank: [Key Indicators Database Online](https://kidb.adb.org). Accessed on 5 November 2026. The original providers are the [Bank of Korea](https://www.bok.or.kr/portal/main/contents.do?menuNo=200613), [Korea's Ministry of Data and Statistics](https://kosis.kr/), and [Taiwan's Directorate-General of Budget, Accounting and Statistics](https://eng.dgbas.gov.tw/). Reuse is subject to the [KIDB Terms of Use](https://kidb.adb.org/terms), including source attribution and any applicable provider terms. |
| [`mobility_daily_long.csv`](data/mobility_daily_long.csv) | Google LLC, [Google COVID-19 Community Mobility Reports](https://www.google.com/covid19/mobility/), accessed via [Our World in Data](https://catalog.ourworldindata.org/garden/covid/latest/google_mobility/google_mobility.csv) on 5 November 2026. The project extract covers South Korea and Taiwan from 1 January 2021 through 15 October 2022. OWID applies a centered seven-day rolling average and identifies the [Google Terms of Service](https://policies.google.com/terms) as the applicable source terms. |

The original `covid_daily.csv` also includes vaccination rates from [Our World in Data](https://ourworldindata.org/covid-vaccinations), the [WHO COVID-19 Dashboard](https://data.who.int/dashboards/covid19/data) and [OWID population estimates](https://ourworldindata.org/population-sources) under CC BY 4.0 and the linked source terms. Effective reproduction estimates are from [TrackingR](https://github.com/crondonm/TrackingR) by Arroyo-Marioli, Bullano, Kucinskas, and Rondón-Moreno (MIT), via [OWID's compact dataset](https://catalog.ourworldindata.org/garden/covid/latest/compact/compact.meta.json). See [third-party notices](data/THIRD_PARTY_NOTICES.txt) for the MIT copyright and permission notices.

### Capital-area card spending

- **Korea.** `capital` sums Seoul (`A`), Incheon (`D`), and Gyeonggi (`L`) under [Article 2 of the enforcement decree](https://www.law.go.kr/LSW/lsInfoP.do?lsiSeq=277967). In ECOS `601Y002`, use monthly totals (`TOT`) and convert million won to won. `unallocated` is the source's Other (`Z`) series, and `non_capital` is the national total (`X`) minus `capital` and `unallocated`.
- **Taiwan.** `capital` sums Taipei (`TPE`), New Taipei (`NTP`), and Keelung (`KLC`). This city-level definition includes all of New Taipei, including Yingge, and excludes Taoyuan. `non_capital` sums the other 19 cities and counties. Amounts sum the published age bands for `FD`, `CT`, `EE`, `DP`, `LG`, `TR`, and `OT` from the [NCCC C02 API](https://www.nccc.com.tw/wps/wcm/connect/zh/home/openinformation/Open%20API). The 22 city and county totals reconcile to the published Taiwan series for every month and proxy, so `unallocated` is zero throughout. [Taiwan transport institute reference for the Taipei metropolitan area](https://www.iot.gov.tw/uploads/asset/data/66195d70367376304acd4ca6/%E4%BA%A4%E9%80%9A%E8%A1%8C%E5%8B%95%E6%9C%8D%E5%8B%99%28MaaS%29%E5%BE%8C%E7%BA%8C%E6%93%B4%E5%85%85%E8%88%87%E6%8E%A8%E5%BB%A3%E7%AD%96%E7%95%A5%E8%A6%8F%E5%8A%83.pdf)

The [industry crosswalk](data/industry_crosswalk.csv) lists the source codes for food and dining, apparel and accessories, health/education/recreation, general retail, lodging/furniture/appliances, vehicles/air travel, and source-defined Other. The first two retain the original definitions. Health/education/recreation and general retail are approximate. Lodging and vehicles cover narrower Korean proxies, and Other retains each source’s own definition. Interpret these three series within each country. E-commerce in the nationwide tab is a separate channel with different source coverage.

The added monthly amounts were retrieved from ECOS and NCCC C02 on 23 November 2026. For every added Taiwan series, the sum of all 22 city and county amounts matches the published national figure. For 2019–2022, both countries’ added national totals also match the corresponding source codes in the repository’s national extracts. The regions describe where card spending is recorded. The COVID-19 extract still has country-level geographic resolution.

Both regional files include `covid_period` and use January 2020 as the cutoff for both countries. `pre_covid` covers January 2014 through December 2019, and `since_covid_onset` covers January 2020 through December 2022.

## Analysis Approach

1. **Compare the pandemic and spending series at a common frequency.** For the joint analysis, sum daily reported cases and deaths by month, average the daily Stringency Index, and align the results with spending by country and calendar month.
2. **Compare relative spending changes.** Use each country and category's own 2019 average as the index baseline and use earlier card data to calculate year-over-year growth. Use the industry crosswalk to check comparability before interpreting cross-country differences.
3. **Check how much the interpretation depends on the comparison.** Compare patterns with cases, deaths, and stringency within each country. Show how the association changes with the spending measure, 0–3 month leads, or the exclusion of months affected by unusual shifts such as Taiwan's February 2021 changes.

The joint window covers up to 24 months per country. Seasonality, shared trends, and differences in card coverage can all shape the apparent relationships. The synthesis will describe exploratory associations with those limits in view.

## Project Status

- [x] Define the research question and identify the source datasets
- [x] Map the course methods to candidate plots and diagnostics
- [x] Build and validate the monthly analysis panel
- [x] Implement the Shiny views and add local run instructions
