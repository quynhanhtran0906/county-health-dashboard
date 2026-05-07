# ============================================================
#  CDC PLACES Health Dashboard — Shiny App
#  6-Chart Rubric Layout:
#
#  Q1 (Where?)        — Choropleth map  +  Horizontal Bar (Top 10)
#  Q2 (Prevention?)   — Scatter + Trend Line  +  Boxplot (High/Low Care)
#  Q3 (Behaviors?)    — Faceted Bar (Small Multiples)  +  Lollipop Chart
# ============================================================

library(shiny)
library(tidyverse)
library(scales)
library(plotly)
library(DT)
library(janitor)

# ── Census region lookup ──────────────────────────────────────
region_map <- c(
  CT="Northeast", ME="Northeast", MA="Northeast", NH="Northeast",
  RI="Northeast", VT="Northeast", NJ="Northeast", NY="Northeast",
  PA="Northeast",
  IL="Midwest",  IN="Midwest",  MI="Midwest",  OH="Midwest",
  WI="Midwest",  IA="Midwest",  KS="Midwest",  MN="Midwest",
  MO="Midwest",  NE="Midwest",  ND="Midwest",  SD="Midwest",
  DE="South",    FL="South",    GA="South",    MD="South",
  NC="South",    SC="South",    VA="South",    DC="South",
  WV="South",    AL="South",    KY="South",    MS="South",
  TN="South",    AR="South",    LA="South",    OK="South",
  TX="South",
  AZ="West",     CO="West",     ID="West",     MT="West",
  NV="West",     NM="West",     UT="West",     WY="West",
  AK="West",     CA="West",     HI="West",     OR="West",
  WA="West"
)

# ── 1. LOAD DATA ──────────────────────────────────────────────
measure_id_map <- c(
  DIABETES   = "diabetes_crudeprev",
  OBESITY    = "obesity_crudeprev",
  BPHIGH     = "highbp_crudeprev",
  CHOLSCREEN = "cholscreen_crudeprev",
  CHECKUP    = "checkup_crudeprev",
  CSMOKING   = "csmoking_crudeprev",
  LPA        = "lpa_crudeprev",
  MHLTH      = "mhlth_crudeprev",
  PHLTH      = "phlth_crudeprev"
)

measure_filter <- paste0(
  "measureid IN(",
  paste0("'", names(measure_id_map), "'", collapse = ","), ")"
)
data_url <- paste0(
  "https://data.cdc.gov/resource/swc5-untb.csv",
  "?$where=", URLencode(paste0(measure_filter,
                               " AND datavaluetypeid='CrdPrv'")),
  "&$limit=50000",
  "&$select=stateabbr,statedesc,locationname,locationid,",
  "totalpopulation,measureid,data_value"
)

places_raw <- tryCatch(
  read_csv(data_url, show_col_types = FALSE),
  error = function(e) {
    message("Live API failed — reading local cache")
    read_csv("places_county_long.csv", show_col_types = FALSE)
  }
)

# Cache locally (only works when running locally, silently skipped on shinyapps.io)
tryCatch(write_csv(places_raw, "places_county_long.csv"), error = function(e) NULL)

places <- places_raw |>
  clean_names() |>
  filter(measureid %in% names(measure_id_map), !is.na(data_value)) |>
  mutate(
    data_value      = as.numeric(data_value),
    totalpopulation = as.numeric(totalpopulation),
    col_name        = measure_id_map[measureid]
  ) |>
  group_by(locationid) |>
  mutate(
    state_abbr       = first(stateabbr),
    state_desc       = first(statedesc),
    county_name      = first(locationname),
    total_population = first(totalpopulation)
  ) |>
  ungroup() |>
  select(locationid, state_abbr, state_desc, county_name,
         total_population, col_name, data_value) |>
  pivot_wider(names_from = col_name, values_from = data_value,
              values_fn = mean) |>
  rename(county_fips = locationid) |>
  filter(!is.na(state_abbr)) |>
  mutate(region = region_map[state_abbr],
         region = replace_na(region, "Other"))

measure_labels <- c(
  diabetes_crudeprev   = "Diabetes (%)",
  obesity_crudeprev    = "Obesity (%)",
  highbp_crudeprev     = "High Blood Pressure (%)",
  csmoking_crudeprev   = "Current Smoking (%)",
  lpa_crudeprev        = "Physical Inactivity (%)",
  cholscreen_crudeprev = "Cholesterol Screening (%)",
  checkup_crudeprev    = "Annual Checkup (%)",
  mhlth_crudeprev      = "Poor Mental Health (%)",
  phlth_crudeprev      = "Poor Physical Health (%)"
)
measure_choices  <- setNames(names(measure_labels), unname(measure_labels))
outcome_vars     <- c("diabetes_crudeprev","obesity_crudeprev",
                      "highbp_crudeprev","mhlth_crudeprev","phlth_crudeprev")
behavior_vars    <- c("csmoking_crudeprev","lpa_crudeprev")
prevention_vars  <- c("cholscreen_crudeprev","checkup_crudeprev")
all_states       <- sort(unique(places$state_abbr))

# ── 2. CSS ────────────────────────────────────────────────────
app_css <- "
.content-wrapper, .right-side { background: #f0f2f5 !important; }

/* ── Business question banner ── */
.biz-banner {
  background: #1a3a5c; color: #e8f4fd;
  border-radius: 10px; padding: 10px 18px;
  margin-bottom: 12px; font-size: 13px; line-height: 1.5;
}
.biz-banner strong { color: #fff; font-size: 14px; }
.biz-source { font-size: 11px; color: #8ab4d4; margin-top: 2px; }

/* ── KPI strip ── */
.kpi-strip { display: flex; gap: 10px; margin-bottom: 12px; }
.kpi-card {
  flex: 1; background:#fff; border-radius:10px;
  padding:10px 14px 8px;
  box-shadow:0 1px 4px rgba(0,0,0,.07);
  border-top:3px solid #bdc3c7;
}
.kpi-card.kpi-blue  { border-top-color:#2980b9; }
.kpi-card.kpi-red   { border-top-color:#c0392b; }
.kpi-card.kpi-green { border-top-color:#27ae60; }
.kpi-label { font-size:10px; color:#999; text-transform:uppercase;
             letter-spacing:.05em; margin-bottom:1px; }
.kpi-value { font-size:22px; font-weight:700; color:#2c3e50; line-height:1.1; }
.kpi-sub   { font-size:10px; color:#aaa; margin-top:1px;
             white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }

/* ── Section labels ── */
.q-label {
  font-size:10px; font-weight:700; text-transform:uppercase;
  letter-spacing:.08em; padding:3px 8px; border-radius:4px;
  display:inline-block; margin-bottom:4px;
}
.q1 { background:#d6eaf8; color:#1a5276; }
.q2 { background:#d5f5e3; color:#1e8449; }
.q3 { background:#fdebd0; color:#a04000; }

/* ── Chart boxes ── */
.chart-box {
  background:#fff; border-radius:10px;
  padding:10px 12px 6px;
  box-shadow:0 1px 4px rgba(0,0,0,.07);
  margin-bottom:10px;
}
.chart-title {
  font-size:12px; font-weight:600; color:#2c3e50;
  margin-bottom:2px;
}
.chart-note {
  font-size:10px; color:#888; font-style:italic;
  margin-bottom:4px;
}

/* ── Controls inline row ── */
.ctrl-row {
  display:flex; gap:8px; align-items:center;
  flex-wrap:wrap; margin-bottom:6px;
}
.ctrl-row label { font-size:11px; color:#555; margin-bottom:0; }
.ctrl-row .form-group { margin-bottom:0; }
.ctrl-row select { font-size:11px; height:28px; padding:2px 6px; }
.ctrl-row .checkbox label { font-size:11px; }
"

# ── 3. UI ─────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),
  title = "US County Health Dashboard",

  # ── Top controls bar ────────────────────────────────────────
  div(style = "background:#1a3a5c; padding:8px 16px; margin:-8px -15px 10px;
               display:flex; align-items:center; gap:16px;",
    span(style = "color:#fff; font-size:15px; font-weight:700;
                  margin-right:8px; white-space:nowrap;",
         "US County Health"),
    div(style = "display:flex; gap:12px; align-items:center; flex-wrap:wrap;",
      div(style = "color:#ccc; font-size:11px; white-space:nowrap;",
          "Filter by State:"),
      div(style = "width:130px;",
        selectInput("state_filter", NULL,
                    choices  = c("All States" = "ALL", all_states),
                    selected = "ALL")
      ),
      div(style = "color:#ccc; font-size:11px; white-space:nowrap;",
          "Health Measure:"),
      div(style = "width:200px;",
        selectInput("main_measure", NULL,
                    choices  = measure_choices,
                    selected = "diabetes_crudeprev")
      )
    )
  ),

  # ── Business question banner ──────────────────────────────
  div(class = "biz-banner",
    strong("Business question: "),
    "Which US counties face the highest chronic disease burden,
     and do preventive care access and risk behaviors explain the variation?",
    div(class = "biz-source",
        "CDC PLACES 2023 | County-level crude prevalence estimates |
         3 research questions answered below")
  ),

  # ── KPI strip ─────────────────────────────────────────────
  div(class = "kpi-strip",
    div(class = "kpi-card kpi-blue",
        div(class = "kpi-label", "National Avg"),
        div(class = "kpi-value", textOutput("kpi_mean", inline = TRUE))),
    div(class = "kpi-card kpi-red",
        div(class = "kpi-label", "Highest County"),
        div(class = "kpi-value", textOutput("kpi_max",  inline = TRUE)),
        div(class = "kpi-sub",   textOutput("kpi_max_name", inline = TRUE))),
    div(class = "kpi-card kpi-green",
        div(class = "kpi-label", "Lowest County"),
        div(class = "kpi-value", textOutput("kpi_min",  inline = TRUE)),
        div(class = "kpi-sub",   textOutput("kpi_min_name", inline = TRUE))),
    div(class = "kpi-card",
        div(class = "kpi-label", "Counties in View"),
        div(class = "kpi-value", textOutput("kpi_n",     inline = TRUE)),
        div(class = "kpi-sub",   textOutput("kpi_states",inline = TRUE)))
  ),

  # ══ ROW 1 — Q1: Geography ══════════════════════════════════
  fluidRow(
    column(7,
      div(class = "chart-box",
        span(class = "q-label q1", "Q1 — Where are the hotspots?"),
        div(class = "chart-title", "Chart 1: State choropleth map"),
        div(class = "chart-note",
            "Darker red = higher prevalence. Map shows spatial clustering."),
        plotlyOutput("choropleth", height = "260px")
      )
    ),
    column(5,
      div(class = "chart-box",
        span(class = "q-label q1", "Q1 — Precise ranking"),
        div(class = "chart-title", "Chart 2: Top 10 counties"),
        div(class = "chart-note",
            "Bar length is easier to compare than map shading."),
        plotlyOutput("geo_top10", height = "260px")
      )
    )
  ),

  # ══ ROW 2 — Q2: Prevention ════════════════════════════════
  fluidRow(
    column(6,
      div(class = "chart-box",
        span(class = "q-label q2", "Q2 — Does preventive care help?"),
        div(class = "chart-title", "Chart 3: Scatter + trend line"),
        div(class = "chart-note",
            "Negative slope = more checkups → fewer cases. Hover to identify counties."),
        div(class = "ctrl-row",
          div(style = "width:155px;",
              selectInput("prev_x", NULL,
                choices  = setNames(prevention_vars,
                                    unname(measure_labels[prevention_vars])),
                selected = "checkup_crudeprev")),
          span(style = "font-size:11px; color:#888;", "vs"),
          div(style = "width:175px;",
              selectInput("prev_y", NULL,
                choices  = setNames(outcome_vars,
                                    unname(measure_labels[outcome_vars])),
                selected = "diabetes_crudeprev")),
          checkboxInput("prev_smooth", "Trend line", TRUE)
        ),
        plotlyOutput("scatter_trend", height = "220px")
      )
    ),
    column(6,
      div(class = "chart-box",
        span(class = "q-label q2", "Q2 — High vs low access"),
        div(class = "chart-title", "Chart 4: Boxplot by care access group"),
        div(class = "chart-note",
            "Counties split at median care rate. Box = IQR; line = median."),
        plotlyOutput("boxplot_care", height = "265px")
      )
    )
  ),

  # ══ ROW 3 — Q3: Behaviors ═════════════════════════════════
  fluidRow(
    column(6,
      div(class = "chart-box",
        span(class = "q-label q3", "Q3 — Which behaviors drive outcomes?"),
        div(class = "chart-title", "Chart 5: Small multiples by Census region"),
        div(class = "chart-note",
            "Each panel = one region. Compare smoking vs inactivity rates."),
        plotlyOutput("small_multiples", height = "265px")
      )
    ),
    column(6,
      div(class = "chart-box",
        span(class = "q-label q3", "Q3 — County benchmark"),
        div(class = "chart-title", "Chart 6: Lollipop — county vs state average"),
        div(class = "chart-note",
            "Stem length = deviation from state avg. Red = above average."),
        div(class = "ctrl-row",
          div(style = "width:80px;",
              selectInput("lollipop_state", NULL,
                          choices = all_states, selected = "TX")),
          uiOutput("lollipop_county_ui_inline"),
          div(style = "width:160px;",
              selectInput("lollipop_measure", NULL,
                          choices  = measure_choices,
                          selected = "lpa_crudeprev"))
        ),
        plotlyOutput("lollipop", height = "225px")
      )
    )
  ),

  # ── Footer / data table toggle ────────────────────────────
  div(style = "margin-top:6px;",
    tags$details(
      tags$summary(style = "font-size:12px; color:#2980b9; cursor:pointer;",
                   "Show full data table"),
      DTOutput("data_table")
    )
  )
)

# ── 4. SERVER ─────────────────────────────────────────────────
server <- function(input, output, session) {

  filtered <- reactive({
    df <- places
    if (input$state_filter != "ALL")
      df <- df |> filter(state_abbr == input$state_filter)
    df
  })

  measure_name <- reactive({ unname(measure_labels[input$main_measure]) })

  # ── Overview KPIs ────────────────────────────────────────
  output$kpi_mean <- renderText({
    paste0(round(mean(filtered()[[input$main_measure]], na.rm = TRUE), 1), "%")
  })
  output$kpi_max <- renderText({
    paste0(round(max(filtered()[[input$main_measure]], na.rm = TRUE), 1), "%")
  })
  output$kpi_max_name <- renderText({
    df <- filtered() |> drop_na(all_of(input$main_measure))
    r  <- df[which.max(df[[input$main_measure]]), ]
    paste0(r$county_name, ", ", r$state_abbr)
  })
  output$kpi_min <- renderText({
    paste0(round(min(filtered()[[input$main_measure]], na.rm = TRUE), 1), "%")
  })
  output$kpi_min_name <- renderText({
    df <- filtered() |> drop_na(all_of(input$main_measure))
    r  <- df[which.min(df[[input$main_measure]]), ]
    paste0(r$county_name, ", ", r$state_abbr)
  })
  output$kpi_n      <- renderText(format(nrow(filtered()), big.mark = ","))
  output$kpi_states <- renderText({
    n <- n_distinct(filtered()$state_abbr)
    paste0(n, " state", if (n != 1) "s")
  })

  output$ov_hist <- renderPlotly({
    vals <- filtered() |> drop_na(all_of(input$main_measure)) |>
      pull(input$main_measure)
    plot_ly(x = ~vals, type = "histogram", nbinsx = 40,
            marker = list(color = "#2980b9",
                          line  = list(color = "white", width = 0.4)),
            hovertemplate = "%{x:.1f}%: %{y} counties<extra></extra>") |>
      layout(margin = list(l = 40, r = 8, t = 10, b = 40),
             xaxis  = list(title = measure_name(), ticksuffix = "%",
                           tickfont = list(size = 10)),
             yaxis  = list(title = "# Counties", tickfont = list(size = 10)),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor  = "rgba(0,0,0,0)") |>
      config(displayModeBar = FALSE)
  })

  output$ov_mini_scatter <- renderPlotly({
    df <- filtered() |>
      drop_na(lpa_crudeprev, all_of(input$main_measure)) |>
      mutate(label = paste0(county_name, ", ", state_abbr))
    plot_ly(df, x = ~lpa_crudeprev, y = ~.data[[input$main_measure]],
            type = "scatter", mode = "markers",
            marker = list(color = "#2980b9", opacity = 0.3, size = 4),
            text = ~paste0(label,
                           "\nInactivity: ", round(lpa_crudeprev, 1), "%",
                           "\n", measure_name(), ": ",
                           round(.data[[input$main_measure]], 1), "%"),
            hoverinfo = "text") |>
      layout(margin = list(l = 48, r = 8, t = 10, b = 40),
             xaxis  = list(title = "Physical Inactivity (%)", ticksuffix = "%",
                           tickfont = list(size = 10)),
             yaxis  = list(title = measure_name(), ticksuffix = "%",
                           tickfont = list(size = 10)),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor  = "rgba(0,0,0,0)") |>
      config(displayModeBar = FALSE)
  })

  # ── Q1: Choropleth (Chart 1) ──────────────────────────────
  output$choropleth <- renderPlotly({
    df <- places |>
      group_by(state_abbr) |>
      summarise(avg       = mean(.data[[input$main_measure]], na.rm = TRUE),
                n_counties = n(), .groups = "drop")

    plot_ly(df,
      type         = "choropleth",
      locations    = ~state_abbr,
      locationmode = "USA-states",
      z            = ~round(avg, 1),
      text         = ~paste0(state_abbr,
                             "\nAvg: ", round(avg, 1), "%",
                             "\nCounties: ", n_counties),
      hoverinfo    = "text",
      colorscale   = list(
        c(0,    "#d4e8f5"),
        c(0.25, "#74add1"),
        c(0.5,  "#f7f7c0"),
        c(0.75, "#f4a55a"),
        c(1,    "#d73027")
      ),
      colorbar = list(title = measure_name(), ticksuffix = "%")
    ) |>
    layout(
      geo    = list(scope = "usa", projection = list(type = "albers usa"),
                    showlakes = TRUE,
                    lakecolor = "rgba(200,230,255,0.5)"),
      margin = list(l = 0, r = 0, t = 10, b = 0)
    )
  })

  # ── Q1: Horizontal Bar — Top / Bottom 10 (Chart 2) ───────
  output$geo_top10 <- renderPlotly({
    df <- filtered() |>
      drop_na(all_of(input$main_measure)) |>
      slice_max(.data[[input$main_measure]], n = 10) |>
      mutate(label = paste0(county_name, ", ", state_abbr))

    p <- df |>
      ggplot(aes(
        x    = reorder(label, .data[[input$main_measure]]),
        y    = .data[[input$main_measure]],
        fill = .data[[input$main_measure]],
        text = paste0(label, ": ",
                      round(.data[[input$main_measure]], 1), "%")
      )) +
      geom_col(alpha = 0.9) +
      coord_flip() +
      scale_fill_gradient(low = "#f4a55a", high = "#d73027", guide = "none") +
      scale_y_continuous(labels = label_number(suffix = "%")) +
      labs(x = NULL, y = measure_name()) +
      theme_minimal(base_size = 11)

    ggplotly(p, tooltip = "text") |>
      layout(margin = list(l = 5, r = 10, t = 10, b = 30))
  })

  output$geo_bot10 <- renderPlotly({
    df <- filtered() |>
      drop_na(all_of(input$main_measure)) |>
      slice_min(.data[[input$main_measure]], n = 10) |>
      mutate(label = paste0(county_name, ", ", state_abbr))

    p <- df |>
      ggplot(aes(
        x    = reorder(label, .data[[input$main_measure]]),
        y    = .data[[input$main_measure]],
        fill = .data[[input$main_measure]],
        text = paste0(label, ": ",
                      round(.data[[input$main_measure]], 1), "%")
      )) +
      geom_col(alpha = 0.9) +
      coord_flip() +
      scale_fill_gradient(low = "#74add1", high = "#d4e8f5", guide = "none") +
      scale_y_continuous(labels = label_number(suffix = "%")) +
      labs(x = NULL, y = measure_name()) +
      theme_minimal(base_size = 11)

    ggplotly(p, tooltip = "text") |>
      layout(margin = list(l = 5, r = 10, t = 10, b = 30))
  })

  # ── Q2: Scatter + Trend Line (Chart 3) ───────────────────
  output$scatter_trend <- renderPlotly({
    df <- filtered() |>
      drop_na(all_of(c(input$prev_x, input$prev_y))) |>
      mutate(label = paste0(county_name, ", ", state_abbr))

    x_lab <- unname(measure_labels[input$prev_x])
    y_lab <- unname(measure_labels[input$prev_y])

    p <- df |>
      ggplot(aes(
        x    = .data[[input$prev_x]],
        y    = .data[[input$prev_y]],
        color = region,
        text = paste0(label,
                      "\n", x_lab, ": ", round(.data[[input$prev_x]], 1), "%",
                      "\n", y_lab, ": ", round(.data[[input$prev_y]], 1), "%")
      )) +
      geom_point(alpha = 0.35, size = 1.6) +
      labs(x = x_lab, y = y_lab, color = "Region") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "right")

    if (input$prev_smooth)
      p <- p + geom_smooth(aes(group = 1), method = "lm", se = TRUE,
                           color = "#2c3e50", linewidth = 1.1,
                           fill = "grey80")

    ggplotly(p, tooltip = "text") |>
      layout(margin = list(l = 50, r = 10, t = 10, b = 40))
  })

  # ── Q2: Boxplot — High vs Low Care (Chart 4) ─────────────
  output$boxplot_care <- renderPlotly({
    x_var <- input$prev_x
    y_var <- input$prev_y
    x_lab <- unname(measure_labels[x_var])
    y_lab <- unname(measure_labels[y_var])

    df <- filtered() |>
      drop_na(all_of(c(x_var, y_var))) |>
      mutate(
        care_group = if_else(
          .data[[x_var]] >= median(.data[[x_var]], na.rm = TRUE),
          paste0("High Access\n(", x_lab, " ≥ median)"),
          paste0("Low Access\n(", x_lab, " < median)")
        )
      )

    p <- df |>
      ggplot(aes(
        x    = care_group,
        y    = .data[[y_var]],
        fill = care_group,
        text = paste0(care_group,
                      "\n", y_lab, ": ", round(.data[[y_var]], 1), "%")
      )) +
      geom_boxplot(alpha = 0.75, outlier.size = 1, outlier.alpha = 0.3) +
      scale_fill_manual(
        values = c(
          setNames("#2980b9",
                   paste0("High Access\n(", x_lab, " ≥ median)")),
          setNames("#c0392b",
                   paste0("Low Access\n(", x_lab, " < median)"))
        ),
        guide = "none"
      ) +
      scale_y_continuous(labels = label_number(suffix = "%")) +
      labs(x = NULL, y = y_lab) +
      theme_minimal(base_size = 12)

    ggplotly(p, tooltip = "text") |>
      layout(margin = list(l = 55, r = 10, t = 10, b = 40))
  })

  # ── Q3: Small Multiples — Faceted Bar by Region (Chart 5) ─
  output$small_multiples <- renderPlotly({
    df <- places |>
      filter(region != "Other") |>
      select(region, all_of(c(behavior_vars, prevention_vars))) |>
      drop_na() |>
      group_by(region) |>
      summarise(across(everything(), ~ mean(., na.rm = TRUE)),
                .groups = "drop") |>
      pivot_longer(-region, names_to = "measure", values_to = "avg") |>
      mutate(
        label = unname(measure_labels[measure]),
        label = str_remove(label, " \\(%\\)")
      )

    region_colors <- c(
      Northeast = "#2980b9", South = "#c0392b",
      Midwest   = "#27ae60", West  = "#8e44ad"
    )

    p <- df |>
      ggplot(aes(
        x    = reorder(label, avg),
        y    = avg,
        fill = region,
        text = paste0(region, "\n", label, ": ", round(avg, 1), "%")
      )) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      facet_wrap(~ region, ncol = 2) +
      scale_fill_manual(values = region_colors) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(x = NULL, y = "Avg Prevalence (%)") +
      theme_minimal(base_size = 10) +
      theme(strip.text  = element_text(face = "bold", size = 10),
            axis.text.y = element_text(size = 9))

    ggplotly(p, tooltip = "text") |>
      layout(margin = list(l = 5, r = 5, t = 10, b = 5))
  })

  # ── Q3: Lollipop county dropdown (used in inline ctrl-row) ──
  output$lollipop_county_ui_inline <- renderUI({
    counties <- places |>
      filter(state_abbr == input$lollipop_state) |>
      arrange(county_name) |>
      pull(county_name) |>
      unique()
    div(style = "width:160px;",
        selectInput("lollipop_county", NULL,
                    choices  = counties,
                    selected = counties[1]))
  })

  output$lollipop <- renderPlotly({
    req(input$lollipop_county)

    m    <- input$lollipop_measure
    m_lb <- unname(measure_labels[m])

    # All counties in the selected state
    state_df <- places |>
      filter(state_abbr == input$lollipop_state) |>
      drop_na(all_of(m)) |>
      arrange(county_name)

    state_avg <- mean(state_df[[m]], na.rm = TRUE)

    # Build lollipop data: deviation from state average
    lollipop_df <- state_df |>
      mutate(
        value     = .data[[m]],
        deviation = value - state_avg,
        direction = if_else(deviation >= 0, "Above average", "Below average"),
        highlight = county_name == input$lollipop_county,
        alpha_val = if_else(highlight, 1, 0.45)
      ) |>
      arrange(deviation)

    p <- lollipop_df |>
      ggplot(aes(
        x     = reorder(county_name, deviation),
        y     = deviation,
        color = direction,
        size  = highlight,
        text  = paste0(county_name, ", ", input$lollipop_state,
                       "\n", m_lb, ": ", round(value, 1), "%",
                       "\nVs state avg (", round(state_avg, 1), "%): ",
                       if_else(deviation >= 0, "+", ""),
                       round(deviation, 1), " pp")
      )) +
      geom_hline(yintercept = 0, linetype = "dashed",
                 color = "#555", linewidth = 0.7) +
      geom_segment(aes(xend = reorder(county_name, deviation), y = 0,
                       yend = deviation),
                   linewidth = 0.6, alpha = 0.5) +
      geom_point(alpha = 0.75) +
      scale_color_manual(
        values = c("Above average" = "#c0392b",
                   "Below average" = "#2980b9"),
        name = NULL
      ) +
      scale_size_manual(values = c("TRUE" = 4, "FALSE" = 2),
                        guide  = "none") +
      coord_flip() +
      labs(x = NULL,
           y = paste0("Deviation from State Avg (", round(state_avg, 1), "%)")) +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.y    = element_text(
          size   = if (nrow(lollipop_df) > 40) 7 else 9,
          face   = if_else(lollipop_df$county_name == input$lollipop_county,
                           "bold", "plain")  # ggplot workaround
        ),
        legend.position = "top"
      )

    ggplotly(p, tooltip = "text") |>
      layout(margin = list(l = 5, r = 10, t = 30, b = 40),
             legend = list(orientation = "h", y = 1.08))
  })

  # ── Data Table ───────────────────────────────────────────
  output$data_table <- renderDT({
    filtered() |>
      mutate(across(ends_with("_crudeprev"),
                    ~ paste0(round(., 1), "%"))) |>
      rename_with(~ unname(measure_labels[.]),
                  ends_with("_crudeprev")) |>
      select(-county_fips) |>
      rename(State       = state_abbr,
             `State Name` = state_desc,
             County      = county_name,
             Population  = total_population,
             Region      = region) |>
      datatable(
        filter  = "top",
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      )
  })
}

# ── 5. RUN ────────────────────────────────────────────────────
shinyApp(ui, server)