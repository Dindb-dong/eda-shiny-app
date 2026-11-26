# Nationwide card comparison inputs

These three CSVs come from the `shiny_card_app.4.zip` attachment to [PR #5](https://github.com/Dindb-dong/eda-shiny-app/pull/5), supplied on 23 November 2026. `korea_en_on_off.csv` has one later change, described below; the other two files are unaltered.

| File | Contents | Supplied coverage |
| :-- | :-- | :-- |
| `korea_en_on_off.csv` | Seven offline categories and a separate e-commerce series in KRW | January 2014–August 2023 |
| `taiwan_all_en_on_off.csv` | Seven categories split into offline and online amounts in NT$ | January 2014–August 2026 |
| `korea_taiwan_cpi_monthly.csv` | Monthly CPI for Korea and Taiwan, labeled 2020 = 100 in the supplied app | January 2014–August 2023 |

The supplied README identifies Taiwan card spending as National Credit Card Center (NCCC) national and e-commerce data. It leaves the Korea card and CPI source references unspecified; those still need to be added by the data contributor.

The supplied Korea categories matched sums of ECOS `601Y002` consumption-type codes in [`korea_national_2019_2022.csv`](../korea_national_2019_2022.csv) for every month from January 2019 to December 2022 (largest difference 1 million won, from rounding). The contributor confirmed that e-commerce was separated on purpose because Taiwan's category amounts include online spending.

On 26 November 2026 the team agreed to count supermarkets (1130) and convenience stores (1140) as department stores rather than food. The supplied data already counted supermarkets in `DP`, so only convenience stores moved. The `FD` and `DP` rows (total and offline amounts) were updated by subtracting and adding the national 1140 monthly total retrieved from the ECOS Open API (`StatisticSearch/601Y002/M/X/1140/TOT`, million won converted to won) for January 2014 – August 2023. The retrieved series equals the repository's 2019–2022 1140 values exactly, and the new `FD` and `DP` equal the national `food` and `retail_broad` series in `card_spending_by_province_korea_2014_2022.csv` for 2014–2022 within 1 million won. The table shows the current codes.

| Code | Korea consumption types (ECOS code) |
| :-- | :-- |
| `FD` | Groceries (1300) + restaurants (2220) |
| `CT` | Apparel and accessories (1400, including watches, jewelry, eyewear and cosmetics) |
| `LG` | Furniture and appliances (1600) + lodging (2210) |
| `TR` | Fuel (1500) + vehicles (1800) + travel and transport (1900) |
| `EE` | Medical and health (1700) + recreation and culture (2000) + education (2100) |
| `DP` | General retail (1100): department stores, marts and specialty retail, supermarkets, convenience stores and duty-free |
| `EC` | E-commerce and mail order (1200) |
| `OT` | Utilities and personal services (2300) + finance and insurance (2400) + other (2500); the total (1000) minus the codes above |

`src/card_section.R` uses the shared calendar-month window with both countries' card observations and CPI. It constructs Taiwan e-commerce by summing the online parts of the seven categories and uses offline amounts for the other categories. Korea e-commerce is the pre-aggregated `EC` series. The resulting eight categories are comparison proxies rather than a guarantee of identical source coverage. This separate extract does not replace the food/apparel proxies or 2019 baseline used in the joint COVID-19 analysis.

The optional Other exclusion removes Taiwan's online Other component as well as both countries' offline Other. Korea e-commerce has no industry breakdown, so its online Other component cannot be removed. The app states this asymmetry. Seasonal explanations for annual Other spikes in the supplied app have not been independently established, so the integrated UI does not assert their cause.
