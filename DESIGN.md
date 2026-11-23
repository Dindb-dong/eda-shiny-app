# COVID-19 and card spending

The Oh My Canvas document **EDA Shiny · COVID-19 and Card Spending** contains the current desktop and mobile **Research layout** boards.

- The page uses a white background, near-black text, gray rules and no tinted surfaces. Georgia headings give the analysis a report-style hierarchy. Native controls and tab labels use the system sans-serif.
- The current section uses a thin black rule at the left. The current subtab uses an underline. The menu stacks above the content on mobile, followed by headings, filters and charts in reading order.
- Filters use four columns on wide screens, two at intermediate widths and one on narrow screens. Chart facets stack when the plot area is narrow. The data table can scroll within its own focusable region.
- The **Native select spacing** component board sets a 13px inset for the select chevron. The chevron is decorative and the browser still opens the native picker.
- Chart marks use the **Chart color scales** board. Years use blue circles and orange triangles. Forecast models use distinct hues and line types. Diverging heatmaps use orange below the reference and teal above it. Preserve accessible labels, keyboard focus and the summary and CSV paths.

## Industry overview

The **EDA Shiny · Industry overview** canvas contains desktop and mobile boards at 1160 and 390 pixels. The desktop board ID is `node_d9f53a7b09c14e1f82822a422e8b1ce6`. The mobile board ID is `node_98412dae10874e3f9f7da700358932ac`. The implementation uses the research layout’s native selects, tab underline, chart palette, and focusable summary region.

The default joint-analysis subtab shows seven industry rows and calendar-month columns. The growth view facets by country and area, and the gap view facets by country. Both views share a zero-centered color scale across their visible panels. Narrow charts stack panels and wrap the facet headings. The detail controls open the chosen industry and move focus to the selected subtab. The summary accompanies the image, with exact plotted values available in the CSV.
