library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(DT)
library(plotly)

# ================================================================
#  REAL DATA — SCREENER.IN (Consolidated ₹ Crores)
# ================================================================
sail_history <- data.frame(
  FY           = c("FY14","FY15","FY16","FY17","FY18","FY19",
                   "FY20","FY21","FY22","FY23","FY24","FY25"),
  Revenue      = c(46804,46032,38793,44210,57496,66973,
                   61664,69114,103477,104448,105378,102479),
  Expenses     = c(42646,41217,41628,44094,52788,57167,
                   51400,56337,82114,96410,94229,91789),
  Op_Profit    = c(4157,4815,-2834,115,4708,9807,
                   10264,12776,21363,8038,11149,10690),
  OPM_pct      = c(9,10,-7,0,8,15,17,18,21,8,11,10),
  Interest     = c(1047,1555,2300,2528,2823,3155,
                   3487,2817,1698,2037,2474,2793),
  Depreciation = c(1836,1907,2404,2682,3066,3385,
                   3756,4103,4275,4964,5278,5651),
  Net_Profit   = c(2652,1939,-4176,-2756,-281,2349,
                   2121,4148,12243,2177,3067,2372),
  ROCE_pct     = c(5,5,-6,-2,3,9,8,11,24,6,8,7),
  Borrowings   = c(26020,32146,35141,41396,45409,45170,
                   54127,37677,17284,30773,36323,36934),
  EPS          = c(6.42,4.93,-10.11,-6.67,-0.68,5.69,
                   5.13,10.04,29.64,5.27,7.42,5.74),
  CFO          = c(6313,2579,4043,2160,6164,7215,
                   -618,23430,30987,-5290,2911,9914),
  Capex        = c(7488,6292,4805,5467,6480,3694,
                   4261,3295,3976,3371,4261,5268)
)

sail_quarterly <- data.frame(
  Quarter    = c("Dec22","Mar23","Jun23","Sep23","Dec23",
                 "Mar24","Jun24","Sep24","Dec24","Mar25",
                 "Jun25","Sep25","Dec25"),
  Revenue    = c(25042,29131,24359,29712,23349,27959,
                 23998,24675,24490,29316,25922,26704,27371),
  Op_Profit  = c(2079,2924,1649,3875,2142,3483,
                 2220,2913,2030,3484,2769,2528,2294),
  OPM_pct    = c(8,10,7,13,9,12,9,12,8,12,11,9,8),
  Net_Profit = c(542,1159,212,1306,423,1126,82,897,142,1251,745,419,374)
)

# Key constants
SHARES  <- 413.1     # crore shares
NET_DEBT <- 36934    # FY25 ₹ Cr
CMP     <- 159       # current market price ₹
BV      <- 141       # book value ₹

# Derived defaults
recent       <- tail(sail_history, 3)
def_rev_mean <- round(mean(recent$Revenue))
def_rev_min  <- round(min(recent$Revenue) * 0.85)
def_rev_max  <- round(max(recent$Revenue) * 1.20)
def_opm_mean <- round(mean(recent$OPM_pct), 1)
def_opm_sd   <- round(sd(recent$OPM_pct), 1)
def_int_mean <- round(mean(recent$Interest))
def_dep_mean <- round(mean(recent$Depreciation))

# ================================================================
#  HELPERS
# ================================================================
rtriangle <- function(n, mn, mode, mx) {
  u  <- runif(n)
  fc <- (mode - mn) / (mx - mn)
  ifelse(u < fc,
         mn + sqrt(u * (mx - mn) * (mode - mn)),
         mx - sqrt((1 - u) * (mx - mn) * (mx - mode))
  )
}

plotly_dark <- function(p, xt = "", yt = "") {
  p %>%
    layout(
      paper_bgcolor = "#0d1117",
      plot_bgcolor  = "#0d1117",
      font  = list(family = "IBM Plex Mono, monospace", color = "#8b949e", size = 10),
      xaxis = list(title = list(text = xt, font = list(size = 10, color = "#6e7681")),
                   gridcolor = "#21262d", zerolinecolor = "#30363d",
                   tickfont  = list(size = 9, color = "#6e7681")),
      yaxis = list(title = list(text = yt, font = list(size = 10, color = "#6e7681")),
                   gridcolor = "#21262d", zerolinecolor = "#30363d",
                   tickfont  = list(size = 9, color = "#6e7681")),
      margin    = list(l = 50, r = 20, t = 20, b = 45),
      showlegend = FALSE
    ) %>%
    config(displayModeBar = FALSE)
}

# ================================================================
#  UI
# ================================================================
ui <- fluidPage(
  
  tags$head(tags$style(HTML('
    @import url("https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@300;400;500;600&family=IBM+Plex+Mono:wght@400;500&display=swap");
 
    html, body {
      margin: 0; padding: 0;
      background: #0d1117;
      color: #c9d1d9;
      font-family: "IBM Plex Sans", sans-serif;
      min-height: 100vh;
    }
    * { box-sizing: border-box; }
 
    /* ══ TOP BAR ══════════════════════════════════════════ */
    .topbar {
      background: #0d1f3c;
      border-bottom: 1px solid #1a3a6b;
      display: flex; align-items: center;
      justify-content: space-between;
      padding: 0 24px; height: 56px;
      position: sticky; top: 0; z-index: 100;
    }
    .topbar-left  { display: flex; align-items: center; gap: 14px; }
    .sail-logo {
      background: #c8a400; border-radius: 6px;
      padding: 4px 10px; font-weight: 700;
      font-size: 14px; color: #0d1f3c;
      letter-spacing: 1px; font-family: "IBM Plex Mono", monospace;
    }
    .company-title {
      font-size: 17px; font-weight: 600; color: #e6edf3;
      letter-spacing: -0.2px;
    }
    .mc-pill {
      background: #162032; border: 1px solid #1a3a6b;
      border-radius: 20px; padding: 3px 12px;
      font-size: 11px; color: #7ab0e0;
      font-family: "IBM Plex Mono", monospace;
    }
    .topbar-right {
      font-family: "IBM Plex Mono", monospace;
      font-size: 12px; color: #6e7681;
    }
    .topbar-right span { color: #e6edf3; font-weight: 500; }
 
    /* ══ BODY LAYOUT ══════════════════════════════════════ */
    .app-body {
      display: flex;
      height: calc(100vh - 56px);
      overflow: hidden;
    }
 
    /* ══ LEFT SIDEBAR ═════════════════════════════════════ */
    .sidebar {
      width: 250px; flex-shrink: 0;
      background: #0d1117;
      border-right: 1px solid #21262d;
      overflow-y: auto;
      padding: 14px 12px 24px;
    }
 
    /* Verified box */
    .verified-box {
      background: #0d1f3c;
      border: 1px solid #1a3a6b;
      border-radius: 8px;
      padding: 12px; margin-bottom: 14px;
    }
    .verified-hdr {
      font-size: 9px; font-weight: 700;
      letter-spacing: 1.6px; text-transform: uppercase;
      color: #22c55e; margin-bottom: 10px;
      display: flex; align-items: center; gap: 5px;
    }
    .verified-hdr::before { content: "✓ "; }
    .vrow {
      display: flex; justify-content: space-between;
      padding: 4px 0;
      border-bottom: 1px solid #162032;
      font-size: 11px;
    }
    .vrow:last-child { border-bottom: none; }
    .vlbl { color: #6e7681; }
    .vval { font-family: "IBM Plex Mono", monospace; color: #c9d1d9; }
    .vval.pos { color: #3fb950; }
    .vval.neg { color: #f85149; }
 
    /* Section headers */
    .sec-hdr {
      font-size: 9px; font-weight: 700;
      letter-spacing: 1.8px; text-transform: uppercase;
      color: #30363d; margin: 14px 0 8px;
      padding-bottom: 5px;
      border-bottom: 1px solid #21262d;
    }
 
    /* Param card */
    .param-card {
      background: #161b22; border: 1px solid #21262d;
      border-radius: 8px; padding: 10px 12px; margin-bottom: 10px;
    }
    .param-note {
      font-size: 9px; color: #484f58; margin-bottom: 8px;
      font-style: italic; line-height: 1.4;
    }
 
    /* Run button */
    #run_sim {
      width: 100%; background: #c8a400; color: #0d1117;
      border: none; border-radius: 8px; padding: 11px;
      font-size: 13px; font-weight: 700; cursor: pointer;
      font-family: "IBM Plex Sans", sans-serif;
      letter-spacing: 0.3px; transition: background 0.18s;
      margin-top: 6px;
    }
    #run_sim:hover { background: #e6be00; }
 
    /* Sliders */
    label { font-size: 11px !important; color: #6e7681 !important; font-weight: 500 !important; }
    .irs--shiny .irs-bar, .irs--shiny .irs-bar--single { background: #c8a400 !important; }
    .irs--shiny .irs-handle > i:first-child { background: #c8a400 !important; border-color: #c8a400 !important; }
    .irs--shiny .irs-from,.irs--shiny .irs-to,.irs--shiny .irs-single {
      background: #1a3a6b !important; color: #e6edf3 !important;
      font-size: 9px !important; font-family: "IBM Plex Mono",monospace !important;
    }
    .irs--shiny .irs-line  { background: #21262d !important; }
    .irs--shiny .irs-grid-text { color: #30363d !important; font-size: 9px !important; }
    .shiny-input-container { margin-bottom: 10px; }
 
    /* ══ MAIN AREA ════════════════════════════════════════ */
    .main-area {
      flex: 1; overflow-y: auto;
      padding: 14px 18px 32px;
      background: #0d1117;
    }
 
    /* ══ KPI ROW ══════════════════════════════════════════ */
    .kpi-row {
      display: grid;
      grid-template-columns: repeat(5, 1fr);
      gap: 10px; margin-bottom: 14px;
    }
    .kpi-card {
      background: #161b22; border: 1px solid #21262d;
      border-radius: 10px; padding: 14px 12px 12px;
    }
    .kpi-card.k-green  { border-top: 2px solid #3fb950; }
    .kpi-card.k-blue   { border-top: 2px solid #388bfd; }
    .kpi-card.k-amber  { border-top: 2px solid #c8a400; }
    .kpi-card.k-purple { border-top: 2px solid #bc8cff; }
    .kpi-card.k-teal   { border-top: 2px solid #39d353; }
    .kpi-lbl {
      font-size: 9px; font-weight: 700; letter-spacing: 1.3px;
      text-transform: uppercase; color: #484f58; margin-bottom: 5px;
    }
    .kpi-val {
      font-family: "IBM Plex Mono", monospace;
      font-size: 19px; font-weight: 500; color: #e6edf3; line-height: 1.2;
    }
    .kpi-sub { font-size: 10px; color: #484f58; margin-top: 3px; }
 
    /* ══ TABS ═════════════════════════════════════════════ */
    .nav-tabs {
      border-bottom: 1px solid #21262d !important;
      margin-bottom: 14px;
    }
    .nav-tabs > li > a {
      color: #6e7681 !important; font-size: 12px !important;
      font-weight: 500 !important; background: transparent !important;
      border: none !important; border-radius: 0 !important;
      padding: 8px 16px !important;
    }
    .nav-tabs > li.active > a {
      color: #e6edf3 !important; background: transparent !important;
      border: none !important;
      border-bottom: 2px solid #c8a400 !important;
    }
    .nav-tabs > li > a:hover { color: #c9d1d9 !important; background: transparent !important; }
 
    /* ══ PLOT CARDS ═══════════════════════════════════════ */
    .pcard {
      background: #161b22; border: 1px solid #21262d;
      border-radius: 10px; padding: 16px; margin-bottom: 12px;
    }
    .pcard-hdr {
      font-size: 9px; font-weight: 700; letter-spacing: 1.3px;
      text-transform: uppercase; color: #484f58;
      margin-bottom: 10px; display: flex; align-items: center; gap: 8px;
    }
    .badge {
      background: #21262d; border-radius: 4px;
      padding: 2px 7px; font-size: 9px; color: #6e7681;
    }
    .grid2 { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; }
    .grid3 { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 12px; }
 
    /* ══ SENSITIVITY TABLE ════════════════════════════════ */
    .stbl { width: 100%; border-collapse: collapse; font-size: 11px; }
    .stbl th {
      background: #162032; color: #6e7681;
      font-size: 9px; font-weight: 700; letter-spacing: 1px;
      text-transform: uppercase; padding: 7px 10px;
      border: 1px solid #21262d; text-align: center;
    }
    .stbl td {
      padding: 6px 10px; text-align: center;
      border: 1px solid #21262d;
      font-family: "IBM Plex Mono", monospace; font-size: 11px;
    }
    .sg  { background: #033a16; color: #3fb950; }
    .slg { background: #0a2a1a; color: #56d364; }
    .sn  { background: #161b22; color: #8b949e; }
    .slr { background: #2d1017; color: #ffa198; }
    .sr  { background: #490202; color: #f85149; }
 
    /* ══ DT TABLES ════════════════════════════════════════ */
    .dataTables_wrapper { font-size: 11px; color: #8b949e; }
    table.dataTable { background: #161b22 !important; color: #8b949e !important; }
    table.dataTable thead th {
      background: #0d1f3c !important; color: #6e7681 !important;
      font-size: 9px !important; font-weight: 700 !important;
      letter-spacing: 1px !important; text-transform: uppercase !important;
      border-bottom: 1px solid #21262d !important;
    }
    table.dataTable tbody tr   { background: #161b22 !important; }
    table.dataTable tbody tr:hover td { background: #1c2128 !important; color: #e6edf3 !important; }
    table.dataTable tbody td   { border-color: #21262d !important; font-family: "IBM Plex Mono", monospace; }
    .dataTables_info, .dataTables_filter, .dataTables_length { color: #484f58 !important; font-size: 10px !important; }
    .dataTables_filter input {
      background: #161b22 !important; border: 1px solid #21262d !important;
      color: #e6edf3 !important; border-radius: 4px; padding: 3px 8px;
    }
 
    /* ══ SCENARIO CARDS ═══════════════════════════════════ */
    .scen-val { font-family: "IBM Plex Mono", monospace; font-size: 28px; font-weight: 500; margin-bottom: 4px; }
    .scen-upside { font-size: 12px; margin-bottom: 8px; }
    .scen-rows div { font-size: 10px; color: #484f58; padding: 2px 0; line-height: 1.6; }
 
    @media(max-width:1100px){
      .app-body { flex-direction: column; height: auto; }
      .sidebar  { width: 100%; border-right: none; border-bottom: 1px solid #21262d; }
      .kpi-row  { grid-template-columns: repeat(3,1fr); }
      .grid2,.grid3 { grid-template-columns: 1fr; }
    }
  '))),
  
  # ── TOP BAR ──────────────────────────────────────────────
  div(class = "topbar",
      div(class = "topbar-left",
          div(class = "sail-logo", "SAIL"),
          div(class = "company-title", "STEEL AUTHORITY OF INDIA LIMITED"),
          div(class = "mc-pill", "Monte Carlo Simulation")
      ),
      div(class = "topbar-right",
          "NSE: SAIL  ·  CMP: ", tags$span(paste0("₹", CMP)),
          "  ·  BV: ", tags$span(paste0("₹", BV)),
          "  ·  FY2025 Verified"
      )
  ),
  
  # ── BODY ─────────────────────────────────────────────────
  div(class = "app-body",
      
      # ══ SIDEBAR ════════════════════════════════════════════
      div(class = "sidebar",
          
          # Verified panel
          div(class = "verified-box",
              div(class = "verified-hdr", "SAIL FY2025 — VERIFIED"),
              div(class="vrow", div(class="vlbl","Revenue"),      div(class="vval","₹1,02,479 Cr")),
              div(class="vrow", div(class="vlbl","Op. Profit"),   div(class="vval pos","₹10,690 Cr")),
              div(class="vrow", div(class="vlbl","OPM"),          div(class="vval","10.4%")),
              div(class="vrow", div(class="vlbl","Net Profit"),   div(class="vval pos","₹2,372 Cr")),
              div(class="vrow", div(class="vlbl","Interest"),     div(class="vval neg","₹2,793 Cr")),
              div(class="vrow", div(class="vlbl","Depreciation"), div(class="vval","₹5,651 Cr")),
              div(class="vrow", div(class="vlbl","Net Debt"),     div(class="vval neg","₹36,934 Cr")),
              div(class="vrow", div(class="vlbl","EPS"),          div(class="vval","₹5.74")),
              div(class="vrow", div(class="vlbl","TTM EPS"),      div(class="vval pos","₹6.75")),
              div(class="vrow", div(class="vlbl","ROCE"),         div(class="vval","6.76%"))
          ),
          
          actionButton("run_sim", "▶  Run Simulation"),
          
          # Simulation
          div(class = "sec-hdr", "SIMULATION"),
          div(class = "param-card",
              sliderInput("n_sim", "Iterations", 1000, 50000, 10000, 1000),
              sliderInput("seed_val", "Random Seed", 1, 999, 42, 1)
          ),
          
          # Revenue
          div(class = "sec-hdr", "REVENUE DRIVERS"),
          div(class = "param-card",
              div(class = "param-note",
                  paste0("FY25: ₹1,02,479 Cr  |  3Y Avg: ₹",
                         format(round(def_rev_mean), big.mark=","), " Cr")),
              sliderInput("rev_mode", "Most Likely Revenue",
                          60000, 150000, def_rev_mean, 1000, pre="₹", post=" Cr"),
              sliderInput("rev_min", "Pessimistic (Min)",
                          40000, 110000, def_rev_min, 1000, pre="₹", post=" Cr"),
              sliderInput("rev_max", "Optimistic (Max)",
                          90000, 200000, def_rev_max, 1000, pre="₹", post=" Cr")
          ),
          
          # Margin
          div(class = "sec-hdr", "MARGIN DRIVERS"),
          div(class = "param-card",
              div(class = "param-note", "FY25: 10.4%  |  FY24: 10.6%  |  FY22 peak: 20.6%"),
              sliderInput("opm_mean", "EBIT Margin Mode (%)", 2, 25, def_opm_mean, 0.5, post="%"),
              sliderInput("opm_sd",   "Margin Std Dev (±%)",  0.5, 8, def_opm_sd, 0.5, post="%")
          ),
          
          # Costs
          div(class = "sec-hdr", "COST ASSUMPTIONS"),
          div(class = "param-card",
              div(class = "param-note", "FY25 actuals from Screener.in"),
              sliderInput("interest",    "Interest (mean)",  500, 5000, def_int_mean, 100, pre="₹", post=" Cr"),
              sliderInput("interest_sd", "Interest Std Dev", 0, 500, 150, 25, pre="±₹", post=" Cr"),
              sliderInput("depreciation","Depreciation",     3000, 8000, def_dep_mean, 100, pre="₹", post=" Cr"),
              sliderInput("tax_rate",    "Effective Tax (%)", 15, 45, 25, 1, post="%")
          )
      ),
      
      # ══ MAIN AREA ══════════════════════════════════════════
      div(class = "main-area",
          
          # KPI tiles
          uiOutput("kpi_row"),
          
          # Tabs
          tabsetPanel(id = "tabs",
                      
                      # ── Distribution ─────────────────────────────────
                      tabPanel("Distribution",
                               div(style = "margin-top:12px;"),
                               div(class = "grid2",
                                   div(class = "pcard",
                                       div(class="pcard-hdr","SHARE PRICE DISTRIBUTION",span(class="badge","PDF")),
                                       plotlyOutput("dist_plot", height="290px")
                                   ),
                                   div(class = "pcard",
                                       div(class="pcard-hdr","CUMULATIVE DISTRIBUTION",span(class="badge","CDF")),
                                       plotlyOutput("cdf_plot", height="290px")
                                   )
                               ),
                               div(class = "pcard",
                                   div(class="pcard-hdr","PERCENTILE FAN CHART",span(class="badge","P5 – P95")),
                                   plotlyOutput("fan_plot", height="240px")
                               )
                      ),
                      
                      # ── Cash Flow Model ──────────────────────────────
                      tabPanel("Cash Flow Model",
                               div(style = "margin-top:12px;"),
                               div(class = "pcard",
                                   div(class="pcard-hdr","NET PROFIT DISTRIBUTION BY SCENARIO",span(class="badge","₹ CRORE")),
                                   plotlyOutput("cf_dist_plot", height="270px")
                               ),
                               div(class = "grid2",
                                   div(class = "pcard",
                                       div(class="pcard-hdr","REVENUE VS NET PROFIT",span(class="badge","SCATTER")),
                                       plotlyOutput("scatter_plot", height="250px")
                                   ),
                                   div(class = "pcard",
                                       div(class="pcard-hdr","HISTORICAL CFO vs CAPEX",span(class="badge","FY14–FY25")),
                                       plotlyOutput("hist_cf_plot", height="250px")
                                   )
                               )
                      ),
                      
                      # ── Sensitivity ──────────────────────────────────
                      tabPanel("Sensitivity",
                               div(style = "margin-top:12px;"),
                               div(class = "pcard",
                                   div(class="pcard-hdr","NET PROFIT SENSITIVITY — OPM vs REVENUE",span(class="badge","₹ CRORE")),
                                   uiOutput("sens1_ui")
                               ),
                               div(class = "pcard",
                                   div(class="pcard-hdr","NET PROFIT SENSITIVITY — INTEREST vs DEPRECIATION",span(class="badge","₹ CRORE")),
                                   uiOutput("sens2_ui")
                               )
                      ),
                      
                      # ── Scenarios ────────────────────────────────────
                      tabPanel("Scenarios",
                               div(style = "margin-top:12px;"),
                               div(class = "grid3",
                                   div(class = "pcard",
                                       div(class="pcard-hdr","BEAR CASE",span(class="badge","P10")),
                                       uiOutput("bear_ui")
                                   ),
                                   div(class = "pcard",
                                       div(class="pcard-hdr","BASE CASE",span(class="badge","MEDIAN")),
                                       uiOutput("base_ui")
                                   ),
                                   div(class = "pcard",
                                       div(class="pcard-hdr","BULL CASE",span(class="badge","P90")),
                                       uiOutput("bull_ui")
                                   )
                               ),
                               div(class = "pcard",
                                   div(class="pcard-hdr","SCENARIO COMPARISON TABLE"),
                                   DTOutput("scen_dt")
                               )
                      ),
                      
                      # ── Tornado ──────────────────────────────────────
                      tabPanel("Tornado",
                               div(style = "margin-top:12px;"),
                               div(class = "pcard",
                                   div(class="pcard-hdr","TORNADO CHART — IMPACT ON NET PROFIT",span(class="badge","±1 SD SHOCK")),
                                   plotlyOutput("tornado_plot", height="360px")
                               )
                      ),
                      
                      # ── Stats & Probability ──────────────────────────
                      tabPanel("Stats & Probability",
                               div(style = "margin-top:12px;"),
                               div(class = "grid2",
                                   div(class = "pcard",
                                       div(class="pcard-hdr","FULL PERCENTILE TABLE"),
                                       DTOutput("pct_dt")
                                   ),
                                   div(class = "pcard",
                                       div(class="pcard-hdr","PROBABILITY SUMMARY"),
                                       uiOutput("prob_ui")
                                   )
                               ),
                               div(class = "pcard",
                                   div(class="pcard-hdr","HISTORICAL FINANCIALS — SCREENER.IN"),
                                   DTOutput("hist_dt")
                               )
                      ),
                      
                      # ── Historical Charts ────────────────────────────
                      tabPanel("Historical Charts",
                               div(style = "margin-top:12px;"),
                               div(class = "grid2",
                                   div(class = "pcard",
                                       div(class="pcard-hdr","REVENUE & NET PROFIT FY14–FY25"),
                                       plotlyOutput("h_rev_plot", height="240px")
                                   ),
                                   div(class = "pcard",
                                       div(class="pcard-hdr","OPM % ANNUAL"),
                                       plotlyOutput("h_opm_plot", height="240px")
                                   ),
                                   div(class = "pcard",
                                       div(class="pcard-hdr","QUARTERLY — LAST 13 QUARTERS"),
                                       plotlyOutput("h_qtr_plot", height="240px")
                                   ),
                                   div(class = "pcard",
                                       div(class="pcard-hdr","DEBT & ROCE %"),
                                       plotlyOutput("h_debt_plot", height="240px")
                                   )
                               )
                      )
          )
      )
  )
)

# ================================================================
#  SERVER
# ================================================================
server <- function(input, output, session) {
  
  # ── Simulation ────────────────────────────────────────────
  sim_data <- eventReactive(input$run_sim, {
    req(input$rev_min < input$rev_mode, input$rev_mode < input$rev_max)
    set.seed(input$seed_val)
    n <- input$n_sim
    
    revenue    <- rtriangle(n, input$rev_min, input$rev_mode, input$rev_max)
    opm_pct    <- pmax(rnorm(n, input$opm_mean, input$opm_sd), -15)
    interest   <- pmax(rnorm(n, input$interest, input$interest_sd), 0)
    op_profit  <- revenue * (opm_pct / 100)
    pbt        <- op_profit - interest - input$depreciation
    net_profit <- pbt * (1 - input$tax_rate / 100)
    
    data.frame(revenue=revenue, opm_pct=opm_pct, op_profit=op_profit,
               interest=interest, pbt=pbt, net_profit=net_profit)
  }, ignoreNULL = TRUE)
  
  # ── KPI row ───────────────────────────────────────────────
  output$kpi_row <- renderUI({
    req(sim_data())
    df  <- sim_data()
    np  <- df$net_profit
    mnp <- mean(np); mdnp <- median(np)
    lp  <- mean(np < 0) * 100
    v5  <- quantile(np, 0.05); p95 <- quantile(np, 0.95)
    fmt <- function(x) paste0("₹", format(round(x), big.mark=","), " Cr")
    
    div(class="kpi-row",
        div(class="kpi-card k-green",
            div(class="kpi-lbl","MEAN NET PROFIT"),
            div(class="kpi-val", fmt(mnp)),
            div(class="kpi-sub", paste0("Med: ", fmt(mdnp)))
        ),
        div(class="kpi-card k-blue",
            div(class="kpi-lbl","MEAN REVENUE"),
            div(class="kpi-val", paste0("₹",format(round(mean(df$revenue)/1000,1),big.mark=","),"K Cr")),
            div(class="kpi-sub", paste0("OPM: ", round(mean(df$opm_pct),1),"%"))
        ),
        div(class="kpi-card k-amber",
            div(class="kpi-lbl","P10 / P90 RANGE"),
            div(class="kpi-val", paste0("₹",round(quantile(np,0.1)/100)/10,"K – ₹",round(quantile(np,0.9)/100)/10,"K")),
            div(class="kpi-sub","80% confidence band")
        ),
        div(class=paste0("kpi-card ", if(lp>25)"k-purple" else "k-teal"),
            div(class="kpi-lbl","PROB. OF LOSS"),
            div(class="kpi-val",
                style=if(lp>25)"color:#f85149" else "color:#3fb950",
                paste0(round(lp,1),"%")),
            div(class="kpi-sub","Simulated scenarios")
        ),
        div(class="kpi-card k-amber",
            div(class="kpi-lbl","VALID PATHS"),
            div(class="kpi-val", format(input$n_sim, big.mark=",")),
            div(class="kpi-sub", paste0("SD: ₹",format(round(sd(np)),big.mark=",")," Cr"))
        )
    )
  })
  
  # ── Distribution (PDF) ────────────────────────────────────
  output$dist_plot <- renderPlotly({
    req(sim_data())
    np   <- sim_data()$net_profit
    mn   <- mean(np); p10 <- quantile(np,.10); p90 <- quantile(np,.90)
    
    plot_ly(x=~np, type="histogram", nbinsx=80,
            marker=list(color="#388bfd", line=list(color="#0d1117",width=0.3)),
            opacity=0.85) %>%
      add_segments(x=mn,  xend=mn,  y=0, yend=Inf, line=list(color="#3fb950",width=1.8,dash="dash")) %>%
      add_segments(x=p10, xend=p10, y=0, yend=Inf, line=list(color="#f85149",width=1.2,dash="dot")) %>%
      add_segments(x=p90, xend=p90, y=0, yend=Inf, line=list(color="#c8a400",width=1.2,dash="dot")) %>%
      add_segments(x=2372, xend=2372, y=0, yend=Inf, line=list(color="#bc8cff",width=1.5,dash="dash")) %>%
      plotly_dark("Net Profit (₹ Crore)", "Frequency")
  })
  
  # ── CDF ───────────────────────────────────────────────────
  output$cdf_plot <- renderPlotly({
    req(sim_data())
    np  <- sort(sim_data()$net_profit)
    cdf <- seq_along(np) / length(np) * 100
    
    plot_ly(x=~np, y=~cdf, type="scatter", mode="lines",
            line=list(color="#bc8cff", width=2)) %>%
      add_segments(x=0,    xend=0,    y=0, yend=100, line=list(color="#f85149",width=1.2,dash="dash")) %>%
      add_segments(x=2372, xend=2372, y=0, yend=100, line=list(color="#c8a400",width=1.2,dash="dash")) %>%
      plotly_dark("Net Profit (₹ Crore)", "Cumulative Prob (%)")
  })
  
  # ── Fan chart ─────────────────────────────────────────────
  output$fan_plot <- renderPlotly({
    req(sim_data())
    np  <- sim_data()$net_profit
    pbs <- c(.05,.10,.25,.50,.75,.90,.95)
    lbl <- c("P5","P10","P25","P50","P75","P90","P95")
    val <- round(quantile(np, pbs))
    clr <- c("#f85149","#ffa657","#e3b341","#3fb950","#39d353","#58a6ff","#bc8cff")
    
    p <- plot_ly()
    for (i in seq_along(pbs)) {
      p <- p %>% add_bars(
        x=lbl[i], y=val[i],
        marker=list(color=clr[i], opacity=0.85),
        text=paste0("₹",format(val[i],big.mark=",")),
        textposition="outside",
        textfont=list(color=clr[i],size=11,family="IBM Plex Mono")
      )
    }
    plotly_dark(p, "Percentile", "Net Profit (₹ Crore)")
  })
  
  # ── CF dist plot ──────────────────────────────────────────
  output$cf_dist_plot <- renderPlotly({
    req(sim_data())
    np <- sim_data()$net_profit
    
    plot_ly() %>%
      add_histogram(x=~np, nbinsx=80,
                    marker=list(color="#388bfd",opacity=0.7,
                                line=list(color="#0d1117",width=0.2)),
                    name="All simulations") %>%
      add_segments(x=mean(np), xend=mean(np), y=0, yend=Inf,
                   line=list(color="#c8a400",width=2,dash="dash"), name="Mean") %>%
      add_segments(x=2372, xend=2372, y=0, yend=Inf,
                   line=list(color="#3fb950",width=1.5,dash="dash"), name="FY25 Actual") %>%
      add_segments(x=0, xend=0, y=0, yend=Inf,
                   line=list(color="#f85149",width=1.2,dash="dot"), name="Break-even") %>%
      plotly_dark("Net Profit (₹ Crore)", "Frequency") %>%
      layout(showlegend=TRUE,
             legend=list(font=list(color="#6e7681",size=9), bgcolor="transparent"))
  })
  
  # ── Scatter ───────────────────────────────────────────────
  output$scatter_plot <- renderPlotly({
    req(sim_data())
    df <- sim_data()
    s  <- df[sample(nrow(df), min(2000, nrow(df))), ]
    
    plot_ly(x=~s$revenue, y=~s$net_profit,
            type="scatter", mode="markers",
            marker=list(
              color=~s$net_profit,
              colorscale=list(c(0,"#f85149"),c(0.5,"#6e7681"),c(1,"#3fb950")),
              size=3, opacity=0.45
            )) %>%
      add_segments(x=min(s$revenue), xend=max(s$revenue), y=0, yend=0,
                   line=list(color="#f85149",width=1,dash="dash")) %>%
      plotly_dark("Revenue (₹ Crore)", "Net Profit (₹ Crore)")
  })
  
  # ── Historical CF ─────────────────────────────────────────
  output$hist_cf_plot <- renderPlotly({
    d <- tail(sail_history, 9)
    plot_ly() %>%
      add_bars(x=~d$FY, y=~d$CFO, name="CFO",
               marker=list(color="#3fb950", opacity=0.8)) %>%
      add_bars(x=~d$FY, y=~(-d$Capex), name="CapEx",
               marker=list(color="#f85149", opacity=0.8)) %>%
      add_lines(x=~d$FY, y=~(d$CFO - d$Capex), name="Free CF",
                line=list(color="#c8a400", width=2)) %>%
      plotly_dark("", "₹ Crore") %>%
      layout(barmode="group", showlegend=TRUE,
             legend=list(font=list(color="#6e7681",size=9), bgcolor="transparent"))
  })
  
  # ── Sensitivity 1: OPM vs Revenue ────────────────────────
  output$sens1_ui <- renderUI({
    opms <- seq(4, 18, by=2)
    revs <- seq(80000, 130000, by=10000)
    
    calc <- function(opm, rev) {
      op  <- rev * (opm / 100)
      pbt <- op - input$interest - input$depreciation
      round(pbt * (1 - input$tax_rate / 100))
    }
    
    cc <- function(v) {
      if (v > 5000) "sg" else if (v > 2000) "slg" else if (v > 0) "sn" else if (v > -2000) "slr" else "sr"
    }
    
    hdr <- tags$tr(
      tags$th("OPM \\ Revenue"),
      lapply(revs, function(r) tags$th(paste0("₹",r/1000,"K")))
    )
    rows <- lapply(opms, function(o) {
      tags$tr(
        tags$th(paste0(o,"%")),
        lapply(revs, function(r) {
          v <- calc(o, r)
          tags$td(class=cc(v), paste0("₹",format(v,big.mark=",")))
        })
      )
    })
    tags$table(class="stbl", tags$thead(hdr), tags$tbody(rows))
  })
  
  # ── Sensitivity 2: Interest vs Depreciation ──────────────
  output$sens2_ui <- renderUI({
    ints  <- seq(1500, 4000, by=500)
    deprs <- seq(4000, 7000, by=500)
    
    calc <- function(int, dep) {
      op  <- input$rev_mode * (input$opm_mean / 100)
      pbt <- op - int - dep
      round(pbt * (1 - input$tax_rate / 100))
    }
    
    cc <- function(v) {
      if (v > 5000) "sg" else if (v > 2000) "slg" else if (v > 0) "sn" else if (v > -2000) "slr" else "sr"
    }
    
    hdr <- tags$tr(
      tags$th("Interest \\ Depn"),
      lapply(deprs, function(d) tags$th(paste0("₹",d/1000,"K Cr")))
    )
    rows <- lapply(ints, function(i) {
      tags$tr(
        tags$th(paste0("₹",i/1000,"K")),
        lapply(deprs, function(d) {
          v <- calc(i, d)
          tags$td(class=cc(v), paste0("₹",format(v,big.mark=",")))
        })
      )
    })
    tags$table(class="stbl", tags$thead(hdr), tags$tbody(rows))
  })
  
  # ── Scenario cards ────────────────────────────────────────
  make_scen <- function(price, col, label) {
    req(sim_data())
    np  <- sim_data()$net_profit
    pct <- if(label=="Bear") 0.10 else if(label=="Bull") 0.90 else 0.50
    val <- round(quantile(np, pct))
    vs  <- round((val / 2372 - 1) * 100, 1)  # vs FY25 actual
    vc  <- if (vs >= 0) "#3fb950" else "#f85149"
    
    div(
      div(class="scen-val", style=paste0("color:",col,";"), paste0("₹",format(val,big.mark=",")," Cr")),
      div(class="scen-upside", style=paste0("color:",vc,";"),
          paste0(if(vs>=0)"▲ " else "▼ ", abs(vs), "% vs FY25 Actual")),
      div(class="scen-rows",
          div(paste0("OPM assumed: ",
                     round(if(label=="Bear") input$opm_mean - input$opm_sd*1.28
                           else if(label=="Bull") input$opm_mean + input$opm_sd*1.28
                           else input$opm_mean, 1), "%")),
          div(paste0("Revenue: ₹", format(round(
            if(label=="Bear") quantile(sim_data()$revenue, 0.10)
            else if(label=="Bull") quantile(sim_data()$revenue, 0.90)
            else quantile(sim_data()$revenue, 0.50)
          ), big.mark=","), " Cr")),
          div(paste0("Interest: ₹", format(input$interest, big.mark=","), " Cr"))
      )
    )
  }
  
  output$bear_ui <- renderUI({ make_scen(0, "#f85149", "Bear") })
  output$base_ui <- renderUI({ make_scen(0, "#e6edf3", "Base") })
  output$bull_ui <- renderUI({ make_scen(0, "#3fb950", "Bull") })
  
  output$scen_dt <- renderDT({
    req(sim_data())
    np    <- sim_data()$net_profit
    probs <- c(0.05,0.10,0.25,0.50,0.75,0.90,0.95)
    vals  <- round(quantile(np, probs))
    vs    <- round((vals / 2372 - 1) * 100, 1)
    
    df <- data.frame(
      Scenario        = c("Bear (P5)","Bear (P10)","Below Base (P25)","Base (Median)",
                          "Above Base (P75)","Bull (P90)","Bull (P95)"),
      "Net Profit"    = paste0("₹", format(vals, big.mark=",")),
      "vs FY25 Actual"= paste0(ifelse(vs>=0,"+",""), vs, "%"),
      "vs FY24 Actual"= paste0(ifelse((vals/3067-1)*100>=0,"+",""), round((vals/3067-1)*100,1), "%"),
      Source          = rep("Simulation", 7)
    )
    datatable(df, rownames=FALSE,
              options=list(pageLength=7, dom='t', ordering=FALSE),
              class="compact")
  })
  
  # ── Tornado ───────────────────────────────────────────────
  output$tornado_plot <- renderPlotly({
    req(sim_data())
    base_np <- median(sim_data()$net_profit)
    
    run_quick <- function(opm, rev, int, dep, tax) {
      n   <- 2000
      set.seed(1)
      r   <- rtriangle(n, input$rev_min, rev, input$rev_max)
      o   <- pmax(rnorm(n, opm, input$opm_sd), -15)
      i   <- pmax(rnorm(n, int, input$interest_sd), 0)
      op  <- r * (o / 100)
      pbt <- op - i - dep
      median(pbt * (1 - tax / 100))
    }
    
    params <- list(
      list("Revenue (Mode)",    "rev_mode",   5000),
      list("OPM %",             "opm_mean",   input$opm_sd),
      list("Interest Cost",     "interest",   input$interest_sd),
      list("Depreciation",      "depreciation", 300),
      list("Tax Rate",          "tax_rate",   3)
    )
    
    results <- lapply(params, function(p) {
      hi_args <- list(input$opm_mean, input$rev_mode, input$interest, input$depreciation, input$tax_rate)
      lo_args <- hi_args
      idx <- match(p[[2]], c("opm_mean","rev_mode","interest","depreciation","tax_rate"))
      if (p[[2]] %in% c("interest","depreciation","tax_rate")) {
        hi_args[[idx]] <- input[[p[[2]]]] + p[[3]]
        lo_args[[idx]] <- input[[p[[2]]]] - p[[3]]
      } else {
        hi_args[[idx]] <- input[[p[[2]]]] + p[[3]]
        lo_args[[idx]] <- input[[p[[2]]]] - p[[3]]
      }
      hi <- do.call(run_quick, hi_args)
      lo <- do.call(run_quick, lo_args)
      list(name=p[[1]], hi=round(hi), lo=round(lo), range=abs(hi-lo))
    })
    
    res <- do.call(rbind, lapply(results, as.data.frame))
    res <- res[order(res$range), ]
    
    plot_ly() %>%
      add_bars(y=~res$name, x=~(res$hi - base_np), orientation="h",
               marker=list(color="#3fb950", opacity=0.85), name="Upside") %>%
      add_bars(y=~res$name, x=~(res$lo - base_np), orientation="h",
               marker=list(color="#f85149", opacity=0.85), name="Downside") %>%
      add_segments(x=0, xend=0, y=0.5, yend=nrow(res)+0.5,
                   line=list(color="#6e7681", width=1)) %>%
      plotly_dark("Impact on Net Profit (₹ Crore)", "") %>%
      layout(barmode="overlay", showlegend=TRUE,
             legend=list(font=list(color="#6e7681",size=9), bgcolor="transparent"))
  })
  
  # ── Percentile table ──────────────────────────────────────
  output$pct_dt <- renderDT({
    req(sim_data())
    np    <- sim_data()$net_profit
    probs <- c(0.01,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.99)
    vals  <- round(quantile(np, probs))
    
    datatable(data.frame(
      Percentile     = paste0(probs*100,"th"),
      "Net Profit"   = paste0("₹",format(vals,big.mark=",")),
      "vs FY25"      = paste0(ifelse((vals/2372-1)*100>=0,"+",""),round((vals/2372-1)*100,1),"%"),
      Interpretation = c(
        "Extreme bear — 1-in-100",
        "VaR 5% worst realistic",
        "Poor year",
        "Below base",
        "Median (base case)",
        "Above base",
        "Strong year",
        "Very bullish",
        "Exceptional 1-in-100"
      )
    ), rownames=FALSE, options=list(pageLength=9, dom='t', ordering=FALSE), class="compact")
  })
  
  # ── Probability summary ───────────────────────────────────
  output$prob_ui <- renderUI({
    req(sim_data())
    np <- sim_data()$net_profit
    
    row <- function(lbl, val, col="#8b949e") {
      div(style="display:flex;justify-content:space-between;padding:7px 0;border-bottom:1px solid #21262d;",
          div(style="font-size:11px;color:#6e7681;", lbl),
          div(style=paste0("font-family:'IBM Plex Mono',monospace;font-size:12px;font-weight:600;color:",col,";"), val)
      )
    }
    
    div(
      row("Mean Net Profit",      paste0("₹",format(round(mean(np)),big.mark=",")," Cr"), "#e6edf3"),
      row("Median Net Profit",    paste0("₹",format(round(median(np)),big.mark=",")," Cr"), "#e6edf3"),
      row("Std Deviation",        paste0("₹",format(round(sd(np)),big.mark=",")," Cr"), "#8b949e"),
      row("Prob > FY25 (₹2,372 Cr)", paste0(round(mean(np>2372)*100,1),"%"),
          if(mean(np>2372)>.5)"#3fb950" else "#f85149"),
      row("Prob > FY24 (₹3,067 Cr)", paste0(round(mean(np>3067)*100,1),"%"), "#bc8cff"),
      row("Prob > ₹5,000 Cr",     paste0(round(mean(np>5000)*100,1),"%"), "#58a6ff"),
      row("Prob of Loss",         paste0(round(mean(np<0)*100,1),"%"),
          if(mean(np<0)<.15)"#3fb950" else "#f85149"),
      row("Prob > FY22 peak (₹12,243 Cr)", paste0(round(mean(np>12243)*100,1),"%"), "#c8a400"),
      row("Skewness",             round(mean((np-mean(np))^3)/sd(np)^3,2), "#8b949e"),
      row("Kurtosis",             round(mean((np-mean(np))^4)/sd(np)^4,2), "#8b949e")
    )
  })
  
  # ── Historical Charts ─────────────────────────────────────
  output$h_rev_plot <- renderPlotly({
    d <- sail_history
    plot_ly() %>%
      add_bars(x=~d$FY, y=~d$Revenue/1000, name="Revenue",
               marker=list(color="#1a3a6b", opacity=0.9)) %>%
      add_lines(x=~d$FY, y=~d$Net_Profit/1000, name="Net Profit",
                line=list(color="#c8a400", width=2)) %>%
      add_markers(x=~d$FY, y=~d$Net_Profit/1000,
                  marker=list(color="#c8a400",size=6,
                              line=list(color="#0d1117",width=1.5))) %>%
      plotly_dark("", "₹ '000 Crore") %>%
      layout(showlegend=TRUE,
             legend=list(font=list(color="#6e7681",size=9),bgcolor="transparent"))
  })
  
  output$h_opm_plot <- renderPlotly({
    d <- sail_history
    plot_ly(x=~d$FY, y=~d$OPM_pct, type="bar",
            marker=list(
              color=ifelse(d$OPM_pct>0,"#1a3a6b","#4a0f0f"),
              line=list(color="#0d1117",width=0.5)
            )) %>%
      add_segments(x=d$FY[1], xend=tail(d$FY,1), y=0, yend=0,
                   line=list(color="#6e7681",width=0.8)) %>%
      plotly_dark("", "OPM (%)")
  })
  
  output$h_qtr_plot <- renderPlotly({
    d <- sail_quarterly
    d$Quarter <- factor(d$Quarter, levels=d$Quarter)
    plot_ly() %>%
      add_bars(x=~d$Quarter, y=~d$Revenue/1000, name="Revenue",
               marker=list(color="#1a3a6b", opacity=0.85)) %>%
      add_lines(x=~d$Quarter, y=~d$Net_Profit/1000, name="Net Profit",
                line=list(color="#c8a400", width=2)) %>%
      plotly_dark("", "₹ '000 Crore") %>%
      layout(showlegend=TRUE,
             legend=list(font=list(color="#6e7681",size=9),bgcolor="transparent"),
             xaxis=list(tickangle=-45))
  })
  
  output$h_debt_plot <- renderPlotly({
    d <- sail_history
    plot_ly() %>%
      add_bars(x=~d$FY, y=~d$Borrowings/1000, name="Debt",
               marker=list(color="#c8a400", opacity=0.75)) %>%
      add_lines(x=~d$FY, y=~d$ROCE_pct, name="ROCE %",
                line=list(color="#388bfd", width=2), yaxis="y2") %>%
      plotly_dark("", "Borrowings (₹ '000 Cr)") %>%
      layout(
        yaxis2 = list(title="ROCE (%)", overlaying="y", side="right",
                      gridcolor="#21262d", tickfont=list(size=9,color="#6e7681")),
        showlegend=TRUE,
        legend=list(font=list(color="#6e7681",size=9),bgcolor="transparent")
      )
  })
  
  # ── Historical data table ─────────────────────────────────
  output$hist_dt <- renderDT({
    d <- sail_history %>%
      transmute(
        FY,
        "Revenue"     = format(Revenue, big.mark=","),
        "Op.Profit"   = format(Op_Profit, big.mark=","),
        "OPM %"       = paste0(OPM_pct,"%"),
        "Net Profit"  = format(Net_Profit, big.mark=","),
        "Interest"    = format(Interest, big.mark=","),
        "Depn"        = format(Depreciation, big.mark=","),
        "CFO"         = format(CFO, big.mark=","),
        "CapEx"       = format(Capex, big.mark=","),
        "Debt"        = format(Borrowings, big.mark=","),
        "ROCE %"      = paste0(ROCE_pct,"%"),
        "EPS (₹)"     = EPS
      )
    datatable(d, rownames=FALSE,
              options=list(pageLength=12, dom='t', ordering=FALSE, scrollX=TRUE),
              class="compact")
  })
}

# ================================================================
#  LAUNCH
# ================================================================
shinyApp(ui=ui, server=server)
