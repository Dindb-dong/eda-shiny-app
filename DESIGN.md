# COVID-19 and card spending

The Oh My Canvas document **EDA Shiny · COVID-19 and Card Spending** contains the current desktop and mobile **Research layout** boards.

- The page uses a white background, near-black text, gray rules and no tinted surfaces. Georgia headings give the analysis a report-style hierarchy. Native controls and tab labels use the system sans-serif.
- The current section uses a thin black rule at the left. The current subtab uses an underline. The menu stacks above the content on mobile, followed by headings, filters and charts in reading order.
- Filters use four columns on wide screens, two at intermediate widths and one on narrow screens. Chart facets stack when the plot area is narrow. The data table can scroll within its own focusable region.
- The **Native select spacing** component board sets a 13px inset for the select chevron. The chevron is decorative and the browser still opens the native picker.
- Chart marks use the **Chart color scales** board. Years use purple squares (2020), blue circles (2021) and orange triangles (2022). Forecast models use distinct hues and line types. Diverging heatmaps use orange below the reference and teal above it. Preserve accessible labels, keyboard focus and the summary and CSV paths.

## Industry overview

The **EDA Shiny · Industry overview** canvas contains desktop and mobile boards at 1160 and 390 pixels. The desktop board ID is `node_d9f53a7b09c14e1f82822a422e8b1ce6`. The mobile board ID is `node_98412dae10874e3f9f7da700358932ac`. The implementation uses the research layout’s native selects, tab underline, chart palette, and focusable summary region.

The industry overview subtab shows seven industry rows and calendar-month columns. The growth view facets by country and area, and the gap view facets by country. Both views share a zero-centered color scale across their visible panels, clamped at ±50 so a few extreme months do not wash out the rest. Masked Taiwan months show the gray missing-value color. Narrow charts stack panels and wrap the facet headings. The detail controls open the chosen industry and move focus to the selected subtab. The summary accompanies the image, with exact plotted values available in the CSV.

## COVID-19 comparison

The **EDA Shiny · COVID-19 comparison and exploration** canvas shows the desktop filter panel and its 320 px version. The desktop board ID is `node_ad502f30511d42a9af85f997c9aa8b43`. The 320 px board ID is `node_4f3b7d726e4744b69345000498d7dd67`.

Cases and deaths and Explore metrics share the same navigation underline and filter controls. Explore metrics opens on daily vaccination coverage. The date fields stack when the filter cell is narrow and sit side by side when that cell has room for both dates. The Shiny date picker keeps the start-to-end keyboard order. The layout follows the common chart theme and color scale, with separate line styles for Korea and Taiwan.

## Regional map

The **Regional map** is a third view in Industry overview. It reuses the Card category select, hides the period select and the industry-detail controls, and adds a Map fill select (Taiwan cities and counties with Korea zones, or zones only) and a full-width month slider with Shiny’s play button. One panel draws both countries at the same scale (Taiwan placed left of Korea, each projected around its own centre) with every unit filled by its own year-over-year growth where city or county data exist (Taiwan) and by its zone’s growth otherwise (Korea) on the heatmap’s orange–neutral–teal scale, fixed at ±50 so colours stay comparable while the slider plays. White hairlines separate units, a heavier near-black outline surrounds the capital zone, and white labels name each zone with its value or “masked”. Masked Taiwan months use the gray missing-value color. The coverage line under the map gives the month’s national stringency and cases per million, and the summary table and CSV hold the zone values plus any city or county values. The alt text gives the zone values and the lowest and highest Taiwan unit rather than every unit.

## Key findings

The joint section opens on **Key findings**, a single scrolling page of five findings followed by a conclusion. Each finding is an `.analysis-section` with an `<h2>` that states the finding as a sentence, a `<figure>` holding one plot and a `<figcaption>` takeaway, and a closed **Figure data** disclosure with the plotted numbers as a table. Sections carry no `aria-labelledby`, so they do not become separate landmarks. Headlines use a smaller Georgia size than chart titles and balanced wrapping. Figures use grey for context and the existing orange (or blue for a second series) to highlight the one element each heading names; country lines keep the blue solid and orange dashed styles. The shared filters are hidden on this tab. Forecasting, break and diagnostic views live under **Appendix**, the last subtab. The six subtab labels are short enough to fit on one row at desktop widths; each label stays on one line (`white-space: nowrap`) and the row still wraps between tabs on narrow screens.

No Oh My Canvas board was updated for this tab, because the canvas tools were unavailable when it was built; it reuses the existing tokens and components.
