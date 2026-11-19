# COVID-19 and card spending

The editable Oh My Canvas document is **EDA Shiny · COVID-19 and Card Spending**. Its desktop and mobile Relationship boards define the masthead, filter grid, view tabs and analysis card. The **Analysis card** main component and its screen instances map to the chart and summary sections in app.R.

- Desktop target is 1216 CSS pixels with two chart columns when space permits. Mobile target is 390 CSS pixels with stacked filters and facets. The summary table can scroll within its own focusable region.
- The page uses a pale green background, white cards, teal controls, dark ink and subdued notes. Chart treatments pair year colors with point shapes and model colors with line types.
- Use native labeled Shiny selects and tabs. Keep keyboard focus visible, expose chart alt text, and provide the summary and analysis rows outside the plotted image.
- The browser implementation includes the real tab states, diagnostics, forecasts and source-aware controls. The analysis card appears as a placeholder in the design board.
