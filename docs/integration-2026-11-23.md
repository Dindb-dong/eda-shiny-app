# PR integration — 23 November 2026

## Scope and design

This integration combines [PR #4](https://github.com/Dindb-dong/eda-shiny-app/pull/4) and [PR #5](https://github.com/Dindb-dong/eda-shiny-app/pull/5). PR #3 was already merged before this work. The review baseline was `5a6462ce640eac1aaedf1bc66ecdedf07b67feee` and the supplied screenshot of its Diagnostics tab.

The existing `DESIGN.md` remains the visual specification. The white page, Georgia headings, system-font labels, left navigation, underlined joint-analysis tabs, four-column desktop filters, thin rules, chart palette, summary table and default joint section are preserved. The joint section's UI definition is unchanged. CSS additions style the new date controls and namespaced download links using the existing design tokens. No mobile-specific changes or validation were requested.

## Integration decisions

- **COVID comparison:** Use PR #4's namespaced daily module. Retain the existing monthly joint analysis and its input/output names. The daily module provides population-scaled cases/deaths, seven-day averages, stringency, policy markers, scatter plots, summary and CSV.
- **Card comparison:** PR #5's original branch had no tree changes relative to the baseline. Its implementation was supplied in `shiny_card_app.4.zip` attached to the PR. Copy its three CSVs unchanged into `data/card_comparison/` and adapt its analysis into `src/card_section.R`; keep the existing app shell instead of copying the standalone app's layout.
- **Conflicts:** Preserve both modules' source calls, loaded data, UI entries and server calls in `app.R`. Update the README to describe both sections as available.
- **STL labels:** The original code assigned the `stl()` output columns in seasonal/trend order to Trend/Seasonal labels. Select the columns by name so the labels match their values. The six-panel arrangement remains unchanged.
- **Calendar integrity:** Require complete seven-day COVID windows, retain negative reporting corrections, and keep incomplete selected-period totals unavailable. Card growth uses calendar-month matching, and moving averages require twelve complete monthly values. Compute transformations before filtering the displayed date range.

## Verification

Verification used R 4.5.0, Shiny 1.10.0 and ggplot2 3.5.1. From the repository root:

```sh
Rscript tests/check-app.R
Rscript tests/check-card-section.R
```

Both scripts passed against the combined app after conflict resolution:

- All 29 existing joint-analysis views rendered through the real Shiny server, including forecasts, diagnostics and age surfaces.
- Period, geography, leads, monthly joins, transformations, forecasts and CSV downloads passed.
- COVID trends, scatter, selected periods, CSV, complete-day smoothing, reporting corrections and STL component reconstruction passed. Additional renders covered the initial six-day window with no available average.
- All nine card amount-unit/series combinations rendered. Source totals, nominal and real 2014 normalization, inflation-adjusted growth, calendar lags, missing values, Other exclusion, midmonth date selection and CSV passed.
- The integrated `app.R` diff preserves the masthead, navigation configuration and joint UI. Existing CSS rules change only by extending the download-link selector; new date-input rules use the established tokens.
- Desktop-sized R plot images were inspected for STL, COVID trends/scatter and card totals/categories. The card axis titles wrap at semantic breaks to avoid clipping in the default plot height.

Live browser screenshot comparison was unavailable in this execution environment because local preview/browser access was blocked. These checks establish R/Shiny rendering and source-level layout preservation; they do not claim a browser pixel comparison.

## Data limits retained in the app

The new nationwide card comparison uses the common January 2014–August 2023 window and its own 2014 baseline. The existing joint analysis still uses its original food/apparel proxies and 2019 baseline. See [the card input notes](../data/card_comparison/README.md) for the supplied archive and exact coverage.

The archive identifies the Taiwan card source as NCCC but leaves the Korean card and CPI source references unspecified. These references still need to be completed by the data contributor. Other exclusion is asymmetric because Korea's e-commerce series has no industry breakdown; the UI explains this. COVID rates retain fixed national population denominators with different national definitions and links in the README.
