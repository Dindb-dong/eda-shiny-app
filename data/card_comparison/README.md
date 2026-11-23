# Nationwide card comparison inputs

These three CSVs are copied without alteration from the `shiny_card_app.4.zip` attachment to [PR #5](https://github.com/Dindb-dong/eda-shiny-app/pull/5), supplied on 23 November 2026.

| File | Contents | Supplied coverage |
| :-- | :-- | :-- |
| `korea_en_on_off.csv` | Seven offline categories and a separate e-commerce series in KRW | January 2014–August 2023 |
| `taiwan_all_en_on_off.csv` | Seven categories split into offline and online amounts in NT$ | January 2014–August 2026 |
| `korea_taiwan_cpi_monthly.csv` | Monthly CPI for Korea and Taiwan, labeled 2020 = 100 in the supplied app | January 2014–August 2023 |

The supplied README identifies Taiwan card spending as National Credit Card Center (NCCC) national and e-commerce data. It leaves the Korea card and CPI source references unspecified. Those references and category-construction details still need to be added by the data contributor; the integration does not infer them from the repository's other extracts.

`src/card_section.R` uses the shared calendar-month window with both countries' card observations and CPI. It constructs Taiwan e-commerce by summing the online parts of the seven categories and uses offline amounts for the other categories. Korea e-commerce is the pre-aggregated `EC` series. The resulting eight categories are comparison proxies rather than a guarantee of identical source coverage. This separate extract does not replace the food/apparel proxies or 2019 baseline used in the joint COVID-19 analysis.

The optional Other exclusion removes Taiwan's online Other component as well as both countries' offline Other. Korea e-commerce has no industry breakdown, so its online Other component cannot be removed. The app states this asymmetry. Seasonal explanations for annual Other spikes in the supplied app have not been independently established, so the integrated UI does not assert their cause.
