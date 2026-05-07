# LLM Log

This file documents every use of a large language model (LLM) in the production of this project,
per the course policy (Section 3.2, Constraint 4).

---

## Entry 1 — Project Setup & Data Exploration

**Date:** May 4, 2026
**Tool:** Claude 
**Prompt summary:** After identifying the CDC PLACES dataset as our data source and defining
the three research questions (geographic clustering, prevention vs. outcomes, behaviors vs.
outcomes), I asked the AI to help set up the initial project structure (folder layout, README)
and write the Socrata API query to download county-level crude prevalence data.
**Output used:** The AI generated the folder scaffolding (`app/`, `report/`, `data/`) and the
initial `read_csv()` call with the correct API endpoint and query parameters.
**What we changed / verified:** We manually tested the API response in the R console and
discovered that the CDC returns data in **long format** (one row per county × measure), not
the wide format we expected. We identified the correct column names (`stateabbr`, `measureid`,
`data_value`, `datavaluetypeid`) by inspecting the raw download with `names()` and `head()`.

---

## Entry 2 — Data Wrangling Pipeline

**Date:** May 4, 2026
**Tool:** Claude
**Prompt summary:** After discovering the long-format issue, I described the desired wide-format
structure to the AI (one row per county, columns like `diabetes_crudeprev`, `obesity_crudeprev`,
etc.) and asked it to help write the pivot logic.
**Output used:** The AI helped write the `filter() → mutate() → group_by() → pivot_wider()`
pipeline that transforms the raw API data into the wide format needed by both the dashboard
and the report.
**What we changed / verified:** We ran the pipeline step-by-step in the R console, checking
`unique(long$col_name)` to confirm all 9 measures mapped correctly, and verified that
`nrow(places)` matched the expected ~3,100 counties. We also added `values_fn = mean` to
handle duplicate rows and `drop_na(state_abbr)` to remove invalid entries.

---

## Entry 3 — Shiny Dashboard Layout & Charts

**Date:** May 4–5, 2026
**Tool:** Claude 
**Prompt summary:** I designed the dashboard layout around the three research questions
(Q1: choropleth + top-10 bar, Q2: scatter + boxplot, Q3: small multiples + lollipop) and
asked the AI to help implement each chart in Shiny/plotly. I specified which chart types to
use for each question and how the global filters (state, measure) should work.
**Output used:** The AI helped translate my chart specifications into working `renderPlotly()`
blocks, including the choropleth map using `plot_ly(type="choropleth")`, the boxplot that
splits counties at the median care rate, and the lollipop chart showing deviation from
state average.
**What we changed / verified:** We iterated on the CSS styling (KPI cards, chart boxes,
question labels) to get the single-page layout looking clean. We tested every chart by
switching measures and state filters to ensure reactivity worked correctly. We also added
the Census region lookup table ourselves based on standard US Census definitions.

---

## Entry 4 — Report Writing & Chart Alignment

**Date:** May 6, 2026
**Tool:** Claude
**Prompt summary:** After finishing the dashboard, I asked the AI to help fix the report's
data loading (it was reading the CSV with wrong column names because the cached file was in
long format) and to check that every chart in the report had a corresponding visualization
in the dashboard.
**Output used:** The AI rewrote the report's data loading chunk to use the same
`filter → pivot_wider` logic as the dashboard. It also identified that the original
histogram (fig-distributions) had no dashboard equivalent.
**What we changed / verified:** We decided to replace the histogram with a **boxplot by
Census region** (showing smoking and inactivity distributions across regions), which mirrors
Dashboard Chart 5 (small multiples). We also updated Section 5.1 of the report to accurately
describe the current 6-chart single-page layout instead of the old 5-tab description.
We re-rendered the report with `quarto render` and verified all 4 figures compiled without
errors.

---

## Entry 5 — Debugging & Iteration

**Date:** May 4–6, 2026
**Tool:** Claude
**Prompt summary:** Throughout the project, I used the AI to help debug specific errors:
`select()` failing because the API column was `stateabbr` not `state_abbr`, `NA` values
appearing in KPI boxes because of missing `as.numeric()` conversions, and the `references.bib`
file not existing when the report tried to render.
**Output used:** The AI suggested targeted fixes (column renaming, type coercion, creating the
missing .bib file). Each fix was a few lines of code.
**What we changed / verified:** Every fix was tested immediately in the R console or by
re-running the app. We used `names()`, `head()`, and `str()` to verify data shapes before
and after each change.

---
