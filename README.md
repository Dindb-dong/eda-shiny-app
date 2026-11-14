<div align="center">

<h1>COVID-19 and Consumer Spending: A Comparative Exploratory Analysis of South Korea and Taiwan (2021–2022)</h1>

<p><strong>Two economies. Three lenses. One question about how spending changed.</strong></p>

<img alt="Planned stack: R and Shiny" src="https://img.shields.io/badge/R-Shiny-276DC3?style=flat-square">
<img alt="Course: STA3115.01-00" src="https://img.shields.io/badge/STA3115.01--00-Final%20Project-0F766E?style=flat-square">
<img alt="Status: Dashboard planned" src="https://img.shields.io/badge/Status-Dashboard%20Planned-64748B?style=flat-square">

<p><strong>EXPLORATORY DATA ANALYSIS (STA3115.01-00)</strong></p>

<p><a href="#research-question">Research Question</a> · <a href="#dashboard-tour">Dashboard Tour</a> · <a href="#data-sources">Data Sources</a> · <a href="#analysis-approach">Analysis Approach</a></p>

</div>

## Research Question

> How are COVID-19 cases, deaths, and government restriction stringency associated with changes in credit card spending in South Korea and Taiwan during 2021–2022?

The planned R Shiny dashboard lets readers follow the pandemic timeline, compare spending within each country's card series, and then examine where the two patterns move together or diverge.

## Dashboard Tour

| Tab | What it asks | Planned panels |
| :-- | :-- | :-- |
| **01 · COVID-19 Timeline** | When did reported cases, deaths, and restrictions rise or fall in each country? | Reported case trends, reported death trends, and side-by-side Stringency Index timelines. |
| **02 · Card Spending** | Which spending categories changed most, and did the two countries move in the same direction? | Spending indices (2019 monthly average = 100), year-over-year growth by category, and heatmaps of monthly changes. |
| **03 · COVID-19 × Spending** | Did spending move with cases, deaths, or restrictions, and did any pattern differ across countries? | Aligned monthly timelines, country-by-country scatterplots, same-month and one-month lag comparisons, and a synthesis panel that summarizes which patterns hold up across spending measures and when unusual months are excluded. |

## Data Sources

| Dataset | Source | Coverage and role |
| :-- | :-- | :-- |
| Reported COVID-19 cases and deaths | [Johns Hopkins University CSSE](https://github.com/CSSEGISandData/COVID-19) | Daily, 2021–2022. Compare when waves in reported cases and deaths occurred. |
| Government restriction stringency | [Our World in Data](https://docs.owid.io/projects/etl/api/covid/) · Oxford COVID-19 Government Response Tracker | Daily, 2021–2022. The 0–100 Stringency Index describes policy strictness. |
| South Korean card spending | [Bank of Korea ECOS](https://ecos.bok.or.kr/) · series `601Y002` | Monthly, 2019–2022. Personal credit card spending by category. |
| Taiwanese card spending | [National Credit Card Center (NCCC)](https://www.nccc.com.tw/wps/wcm/connect/zh/home/openinformation/PageIntroduce) · series `C02` | Monthly, 2019–2022. Published spending categories include credit and debit card transactions. |

The course team's `Final Project` data bundle contains `korea_taiwan_2021_2022/covid_daily.csv` and the harmonized card spending proxies in `Korea_Taiwan_2019_2022/data/common_panel.csv`. The data files are kept outside this repository.

## Analysis Approach

1. **Compare the pandemic and spending series at a common frequency.** For the joint analysis, sum daily reported cases and deaths by month, average the daily Stringency Index, and align the results with spending by country and calendar month.
2. **Compare relative spending changes.** Use each country and category's own 2019 average as the index baseline and use earlier card data to calculate year-over-year growth. Treat food and apparel as approximate matches because card coverage and category definitions differ.
3. **Check how much the interpretation depends on the comparison.** Compare patterns with cases, deaths, and stringency within each country. Show how the association changes with the spending measure, a one-month lag, or the exclusion of months affected by unusual shifts such as Taiwan's February 2021 changes.

The joint window covers up to 24 months per country. Seasonality, shared trends, and differences in card coverage can all shape the apparent relationships. The synthesis will report exploratory associations; it cannot establish how much restrictions or outbreaks caused spending to change.

## Project Status

- [x] Define the research question and identify the source datasets
- [ ] Build and validate the monthly analysis panel
- [ ] Implement the three Shiny tabs
- [ ] Add screenshots and local run instructions once the dashboard is runnable
