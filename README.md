<div align="center">

<h1>COVID-19 and Consumer Spending: A Comparative Exploratory Analysis of South Korea and Taiwan (2021–2022)</h1>

<p><strong>Two economies. Three lenses. One question about how spending changed.</strong></p>

<img alt="Planned stack: R and Shiny" src="https://img.shields.io/badge/R-Shiny-276DC3?style=flat-square">
<img alt="Course: STA3115.01-00" src="https://img.shields.io/badge/STA3115.01--00-Final%20Project-0F766E?style=flat-square">
<img alt="Status: Dashboard planned" src="https://img.shields.io/badge/Status-Dashboard%20Planned-64748B?style=flat-square">

<p><strong>EXPLORATORY DATA ANALYSIS (STA3115.01-00)</strong></p>

<p><a href="#research-question">Research Question</a> · <a href="#dashboard-tour">Dashboard Tour</a> · <a href="#course-methods">Course Methods</a> · <a href="#data-sources">Data Sources</a> · <a href="#analysis-approach">Analysis Approach</a></p>

</div>

## Research Question

> How are COVID-19 cases, deaths, and government restriction stringency associated with changes in credit card spending in South Korea and Taiwan during 2021–2022?

The planned R Shiny dashboard lets readers follow the pandemic timeline, compare spending within each country's card series, and then examine where the two patterns move together or diverge.

## Dashboard Tour

| Tab | Question | Planned panels |
| :-- | :-- | :-- |
| **01 · COVID-19 Timeline** | When did reported cases, deaths, and restrictions rise or fall in each country? | Daily and monthly timelines, distribution comparisons, and summaries of typical values and extremes. |
| **02 · Card Spending** | Which categories changed most, and did the two countries move in the same direction? | Indexed spending trends, year-over-year growth, seasonal heatmaps, distribution diagnostics, and Taiwan's age-by-month spending patterns. |
| **03 · COVID-19 × Spending** | Did spending move with cases, deaths, or restrictions, and did the pattern differ by country? | Aligned timelines, scatterplots and smoothers, conditional views, model diagnostics, and a synthesis of which patterns hold under alternative comparisons. |

## Course Methods

This provisional plot plan follows the STA3115 lecture slides and R exercises for Units 1–16.

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

## Analysis Approach

1. **Compare the pandemic and spending series at a common frequency.** For the joint analysis, sum daily reported cases and deaths by month, average the daily Stringency Index, and align the results with spending by country and calendar month.
2. **Compare relative spending changes.** Use each country and category's own 2019 average as the index baseline and use earlier card data to calculate year-over-year growth. Treat food and apparel as approximate matches because card coverage and category definitions differ.
3. **Check how much the interpretation depends on the comparison.** Compare patterns with cases, deaths, and stringency within each country. Show how the association changes with the spending measure, a one-month lag, or the exclusion of months affected by unusual shifts such as Taiwan's February 2021 changes.

The joint window covers up to 24 months per country. Seasonality, shared trends, and differences in card coverage can all shape the apparent relationships. The synthesis will describe exploratory associations with those limits in view.

## Project Status

- [x] Define the research question and identify the source datasets
- [x] Map the course methods to candidate plots and diagnostics
- [ ] Build and validate the monthly analysis panel
- [ ] Implement the three Shiny tabs
- [ ] Add screenshots and local run instructions once the dashboard is runnable
