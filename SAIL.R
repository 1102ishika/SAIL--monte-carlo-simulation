library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(DT)

# ----------------------------------------------------------------
#  REAL HISTORICAL DATA FROM SCREENER.IN (Consolidated, ₹ Crores)
# ----------------------------------------------------------------
sail_history <- data.frame(
  FY          = c("FY14","FY15","FY16","FY17","FY18","FY19",
                  "FY20","FY21","FY22","FY23","FY24","FY25"),
  Revenue     = c(46804,46032,38793,44210,57496,66973,
                  61664,69114,103477,104448,105378,102479),
  Expenses    = c(42646,41217,41628,44094,52788,57167,
                  51400,56337,82114,96410,94229,91789),
  Op_Profit   = c(4157,4815,-2834,115,4708,9807,
                  10264,12776,21363,8038,11149,10690),
  OPM_pct     = c(9,10,-7,0,8,15,17,18,21,8,11,10),
  Interest    = c(1047,1555,2300,2528,2823,3155,
                  3487,2817,1698,2037,2474,2793),
  Depreciation= c(1836,1907,2404,2682,3066,3385,
                  3756,4103,4275,4964,5278,5651),
  Net_Profit  = c(2652,1939,-4176,-2756,-281,2349,
                  2121,4148,12243,2177,3067,2372),
  ROCE_pct    = c(5,5,-6,-2,3,9,8,11,24,6,8,7),
  Borrowings  = c(26020,32146,35141,41396,45409,45170,
                  54127,37677,17284,30773,36323,36934),
  EPS         = c(6.42,4.93,-10.11,-6.67,-0.68,5.69,
                  5.13,10.04,29.64,5.27,7.42,5.74)
)

# Quarterly data (last 13 quarters from Screener)
sail_quarterly <- data.frame(
  Quarter     = c("Dec22","Mar23","Jun23","Sep23","Dec23",
                  "Mar24","Jun24","Sep24","Dec24","Mar25",
                  "Jun25","Sep25","Dec25"),
  Revenue     = c(25042,29131,24359,29712,23349,27959,
                  23998,24675,24490,29316,25922,26704,27371),
  Op_Profit   = c(2079,2924,1649,3875,2142,3483,
                  2220,2913,2030,3484,2769,2528,2294),
  OPM_pct     = c(8,10,7,13,9,12,9,12,8,12,11,9,8),
  Net_Profit  = c(542,1159,212,1306,423,1126,82,897,142,1251,745,419,374)
)

# ----------------------------------------------------------------
#  HELPER: Triangular distribution sampler
# ----------------------------------------------------------------
rtriangle <- function(n, mn, mode, mx) {
  u  <- runif(n)
  fc <- (mode - mn) / (mx - mn)
  ifelse(u < fc,
         mn + sqrt(u * (mx - mn) * (mode - mn)),
         mx - sqrt((1 - u) * (mx - mn) * (mx - mode))
  )
}

# ----------------------------------------------------------------
#  DERIVED DEFAULTS from Screener data (last 3 years FY23-FY25)
# ----------------------------------------------------------------
recent <- tail(sail_history, 3)
def_rev_mean  <- round(mean(recent$Revenue))        # ~104,101
def_rev_min   <- round(min(recent$Revenue) * 0.85)  # stress scenario
def_rev_max   <- round(max(recent$Revenue) * 1.20)  # optimistic
def_opm_mean  <- round(mean(recent$OPM_pct), 1)     # ~9.7%
def_opm_sd    <- round(sd(recent$OPM_pct), 1)       # ~1.5%
def_int_mean  <- round(mean(recent$Interest))       # ~2,434
def_dep_mean  <- round(mean(recent$Depreciation))   # ~5,298
def_tax_rate  <- 25                                  # effective ~25%

# ================================================================
#  UI
# ================================================================
ui <- fluidPage(
  
  tags$head(tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@300;400;500;600&family=IBM+Plex+Mono:wght@400;500&display=swap');
 
    *, *::before, *::after { box-sizing: border-box; margin:0; padding:0; }
 
    body {
      font-family: 'IBM Plex Sans', sans-serif;
      background: #f0f4f8;
      color: #1a202c;
    }
 
    /* ── HEADER ── */
    .header {
      background: #003366;
      padding: 0 32px;
      display: flex;
      align-items: center;
      justify-content: space-between;
      height: 64px;
      box-shadow: 0 2px 8px rgba(0,51,102,0.3);
    }
    .header-left { display: flex; align-items: center; gap: 14px; }
    .header-logo {
      width: 40px; height: 40px;
      background: #c8a400;
      border-radius: 6px;
      display: flex; align-items: center; justify-content: center;
      font-weight: 700; font-size: 14px; color: #003366; letter-spacing: 1px;
    }
    .header-title { color: #fff; font-size: 18px; font-weight: 600; line-height: 1.2; }
    .header-sub   { color: #7ab0e0; font-size: 11px; letter-spacing: 0.5px; }
    .header-badge {
      background: rgba(255,255,255,0.12);
      border: 1px solid rgba(255,255,255,0.2);
      border-radius: 20px;
      padding: 4px 14px;
      font-size: 11px;
      color: #b3d4f5;
      font-family: 'IBM Plex Mono', monospace;
    }
 
    /* ── LAYOUT ── */
    .page-body {
      display: flex;
      gap: 20px;
      padding: 20px 24px 32px;
      max-width: 1500px;
      margin: 0 auto;
    }
    .left-col  { width: 320px; flex-shrink: 0; }
    .right-col { flex: 1; min-width: 0; }
 
    /* ── DATA SOURCE BANNER ── */
    .data-banner {
      background: #e8f4ff;
      border: 1px solid #b3d4f5;
      border-radius: 8px;
      padding: 10px 14px;
      margin-bottom: 16px;
      font-size: 11px;
      color: #003366;
      display: flex;
      align-items: center;
      gap: 8px;
    }
    .data-banner strong { color: #003366; }
 
    /* ── CARDS ── */
    .card {
      background: #fff;
      border: 1px solid #e2e8f0;
      border-radius: 10px;
      padding: 18px;
      margin-bottom: 16px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.06);
    }
    .card-title {
      font-size: 10px;
      font-weight: 600;
      text-transform: uppercase;
      letter-spacing: 1.3px;
      color: #003366;
      margin-bottom: 14px;
      padding-bottom: 8px;
      border-bottom: 2px solid #003366;
    }
    .param-note {
      font-size: 10px;
      color: #718096;
      margin-top: -10px;
      margin-bottom: 10px;
      font-style: italic;
    }
 
    /* ── SLIDERS ── */
    label { font-size:12px !important; color:#4a5568 !important; font-weight:500 !important; }
    .irs--shiny .irs-bar, .irs--shiny .irs-bar--single { background:#003366 !important; }
    .irs--shiny .irs-handle > i:first-child { background:#003366 !important; border-color:#003366 !important; }
    .irs--shiny .irs-from,.irs--shiny .irs-to,.irs--shiny .irs-single {
      background:#003366 !important; font-size:10px !important;
      font-family:'IBM Plex Mono',monospace !important;
    }
    .irs--shiny .irs-line { background:#e2e8f0 !important; }
 
    /* ── RUN BUTTON ── */
    #run_sim {
      width:100%; background:#003366; color:#fff;
      border:none; border-radius:8px; padding:13px;
      font-size:14px; font-weight:600; cursor:pointer;
      font-family:'IBM Plex Sans',sans-serif;
      letter-spacing:0.3px; transition:background 0.2s;
    }
    #run_sim:hover { background:#004d99; }
 
    /* ── KPI GRID ── */
    .kpi-grid { display:grid; grid-template-columns:repeat(3,1fr); gap:12px; margin-bottom:16px; }
    .kpi {
      background:#fff;
      border:1px solid #e2e8f0;
      border-radius:10px;
      padding:14px 12px 12px;
      text-align:center;
      box-shadow:0 1px 3px rgba(0,0,0,0.05);
    }
    .kpi.blue   { border-top:3px solid #003366; }
    .kpi.gold   { border-top:3px solid #c8a400; }
    .kpi.red    { border-top:3px solid #c53030; }
    .kpi.green  { border-top:3px solid #276749; }
    .kpi.slate  { border-top:3px solid #4a5568; }
    .kpi.teal   { border-top:3px solid #2c7a7b; }
    .kpi-lbl { font-size:9px; font-weight:600; text-transform:uppercase; letter-spacing:1px; color:#718096; margin-bottom:5px; }
    .kpi-val { font-family:'IBM Plex Mono',monospace; font-size:18px; font-weight:500; color:#1a202c; white-space:nowrap; }
    .kpi-val.pos { color:#276749; }
    .kpi-val.neg { color:#c53030; }
 
    /* ── TABS ── */
    .nav-tabs { border-bottom:2px solid #e2e8f0; margin-bottom:16px; }
    .nav-tabs > li > a { color:#4a5568; font-size:13px; font-weight:500; border:none !important; padding:9px 16px; }
    .nav-tabs > li.active > a { color:#003366; border-bottom:2px solid #003366 !important; background:transparent !important; }
 
    /* ── PLOTS ── */
    .plot-card {
      background:#fff; border:1px solid #e2e8f0;
      border-radius:10px; padding:20px;
      margin-bottom:16px;
      box-shadow:0 1px 3px rgba(0,0,0,0.05);
    }
    .plot-label { font-size:12px; font-weight:600; color:#4a5568; margin-bottom:12px; }
 
    /* ── TABLE ── */
    .dataTables_wrapper { font-size:12px; }
    table.dataTable thead th { background:#003366; color:#fff; font-size:11px; font-weight:600; }
    table.dataTable tbody tr:hover td { background:#eef4ff !important; }
 
    /* ── HISTORICAL ── */
    .hist-grid { display:grid; grid-template-columns:1fr 1fr; gap:16px; }
 
    @media(max-width:950px){
      .page-body{flex-direction:column; padding:14px;}
      .left-col{width:100%;}
      .kpi-grid{grid-template-columns:repeat(2,1fr);}
      .hist-grid{grid-template-columns:1fr;}
    }
  "))),
  
  # ── Header ──────────────────────────────────────────────────
  div(class="header",
      div(class="header-left",
          div(class="header-logo","SAIL"),
          div(
            div(class="header-title","Steel Authority of India Limited"),
            div(class="header-sub","MONTE CARLO PROFIT SIMULATION  ·  NSE: SAIL")
          )
      ),
      div(class="header-badge","Data: Screener.in  ·  FY2014–FY2025")
  ),
  
  div(class="page-body",
      
      # ════════════════ LEFT SIDEBAR ════════════════
      div(class="left-col",
          
          div(class="data-banner",
              "📊",
              tags$span(HTML("<strong>Live Parameters</strong> pre-filled from Screener.in consolidated data.
                        Adjust to model scenarios."))
          ),
          
          # Revenue block
          div(class="card",
              div(class="card-title","Revenue Assumptions (₹ Crore)"),
              div(class="param-note",
                  paste0("FY25 Actual: ₹1,02,479 Cr  |  3Y Avg: ₹",
                         format(round(def_rev_mean), big.mark=","), " Cr")),
              sliderInput("rev_mode","Most likely revenue",
                          min=60000, max=150000, value=def_rev_mean, step=1000,
                          pre="₹", post=" Cr"),
              sliderInput("rev_min","Pessimistic (minimum)",
                          min=40000, max=110000, value=def_rev_min, step=1000,
                          pre="₹", post=" Cr"),
              sliderInput("rev_max","Optimistic (maximum)",
                          min=90000, max=200000, value=def_rev_max, step=1000,
                          pre="₹", post=" Cr")
          ),
          
          # OPM block
          div(class="card",
              div(class="card-title","Operating Profit Margin (%)"),
              div(class="param-note","FY25: 10.4%  |  FY24: 10.6%  |  FY22 peak: 20.6%"),
              sliderInput("opm_mean","Mean OPM (%)",
                          min=2, max=25, value=def_opm_mean, step=0.5,
                          post="%"),
              sliderInput("opm_sd","OPM Std Deviation (±%)",
                          min=0.5, max=8, value=def_opm_sd, step=0.5,
                          post="%")
          ),
          
          # Below-line costs
          div(class="card",
              div(class="card-title","Below-line Costs (₹ Crore)"),
              div(class="param-note","FY25 Actuals from Screener.in"),
              sliderInput("interest","Interest (mean)",
                          min=500, max=5000, value=def_int_mean, step=100,
                          pre="₹", post=" Cr"),
              sliderInput("interest_sd","Interest variability (±)",
                          min=0, max=500, value=150, step=25,
                          pre="±₹", post=" Cr"),
              sliderInput("depreciation","Depreciation (fixed)",
                          min=3000, max=8000, value=def_dep_mean, step=100,
                          pre="₹", post=" Cr"),
              sliderInput("tax_rate","Effective Tax Rate (%)",
                          min=15, max=45, value=def_tax_rate, step=1,
                          post="%")
          ),
          
          # Sim settings
          div(class="card",
              div(class="card-title","Simulation Settings"),
              sliderInput("n_sim","Iterations",
                          min=1000, max=100000, value=20000, step=1000),
              sliderInput("seed_val","Random Seed",
                          min=1, max=999, value=42, step=1)
          ),
          
          actionButton("run_sim","▶  Run Simulation")
      ),
      
      # ════════════════ RIGHT PANEL ════════════════
      div(class="right-col",
          
          # KPI row
          uiOutput("kpi_row"),
          
          # Tabs
          tabsetPanel(id="main_tabs",
                      
                      # ── Tab 1: Simulation Results ──────────────
                      tabPanel("Simulation Results",
                               
                               div(style="display:grid;grid-template-columns:1fr 1fr;gap:16px;margin-top:12px;",
                                   div(class="plot-card",
                                       div(class="plot-label","Net Profit Distribution — Histogram"),
                                       plotOutput("hist_plot", height="260px")
                                   ),
                                   div(class="plot-card",
                                       div(class="plot-label","Cumulative Probability (CDF)"),
                                       plotOutput("cdf_plot", height="260px")
                                   )
                               ),
                               
                               div(style="display:grid;grid-template-columns:1fr 1fr;gap:16px;",
                                   div(class="plot-card",
                                       div(class="plot-label","Revenue vs Net Profit (scatter sample)"),
                                       plotOutput("scatter_plot", height="240px")
                                   ),
                                   div(class="plot-card",
                                       div(class="plot-label","OPM Distribution across simulations"),
                                       plotOutput("opm_hist", height="240px")
                                   )
                               )
                      ),
                      
                      # ── Tab 2: Percentile Table ─────────────────
                      tabPanel("Risk & Percentiles",
                               div(style="margin-top:12px;"),
                               div(class="plot-card",
                                   div(class="plot-label","Full Percentile Summary — Net Profit (₹ Crore)"),
                                   DTOutput("pct_table")
                               ),
                               div(class="plot-card",
                                   div(class="plot-label","Scenario Analysis"),
                                   DTOutput("scenario_table")
                               )
                      ),
                      
                      # ── Tab 3: Historical Data ──────────────────
                      tabPanel("Historical Financials (Screener.in)",
                               div(style="margin-top:12px;"),
                               div(class="hist-grid",
                                   div(class="plot-card",
                                       div(class="plot-label","Revenue & Net Profit — FY2014 to FY2025"),
                                       plotOutput("hist_rev_plot", height="240px")
                                   ),
                                   div(class="plot-card",
                                       div(class="plot-label","Operating Profit Margin % — Annual"),
                                       plotOutput("hist_opm_plot", height="240px")
                                   ),
                                   div(class="plot-card",
                                       div(class="plot-label","Quarterly Revenue & Net Profit (last 13 quarters)"),
                                       plotOutput("qtrly_plot", height="240px")
                                   ),
                                   div(class="plot-card",
                                       div(class="plot-label","Debt (Borrowings) & ROCE %"),
                                       plotOutput("debt_roce_plot", height="240px")
                                   )
                               ),
                               div(class="plot-card",
                                   div(class="plot-label","Historical Financials Table — Screener.in Data"),
                                   DTOutput("hist_table")
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
  
  # ── Run simulation ─────────────────────────────────────────
  sim_data <- eventReactive(input$run_sim, {
    
    set.seed(input$seed_val)
    n <- input$n_sim
    
    validate(
      need(input$rev_min < input$rev_mode,
           "Min revenue must be < most likely revenue."),
      need(input$rev_mode < input$rev_max,
           "Most likely revenue must be < max revenue.")
    )
    
    revenue  <- rtriangle(n, input$rev_min, input$rev_mode, input$rev_max)
    opm_pct  <- rnorm(n, input$opm_mean, input$opm_sd)
    opm_pct  <- pmax(opm_pct, -15)   # floor at -15%
    interest <- pmax(rnorm(n, input$interest, input$interest_sd), 0)
    
    op_profit <- revenue * (opm_pct / 100)
    pbt       <- op_profit - interest - input$depreciation
    net_profit <- pbt * (1 - input$tax_rate / 100)
    
    data.frame(
      revenue    = revenue,
      opm_pct    = opm_pct,
      op_profit  = op_profit,
      interest   = interest,
      pbt        = pbt,
      net_profit = net_profit
    )
  }, ignoreNULL = FALSE)
  
  # ── KPI row ────────────────────────────────────────────────
  output$kpi_row <- renderUI({
    df <- sim_data(); req(df)
    
    np <- df$net_profit
    loss_prob <- mean(np < 0) * 100
    mean_np   <- mean(np)
    var5      <- quantile(np, 0.05)
    p95       <- quantile(np, 0.95)
    med_np    <- median(np)
    mean_rev  <- mean(df$revenue)
    
    fmt <- function(x) paste0("₹", format(round(x), big.mark=","), " Cr")
    col <- function(x) if (x >= 0) "pos" else "neg"
    
    div(class="kpi-grid",
        div(class="kpi blue",
            div(class="kpi-lbl","Mean Net Profit"),
            div(class=paste("kpi-val",col(mean_np)), fmt(mean_np))
        ),
        div(class="kpi gold",
            div(class="kpi-lbl","Mean Revenue"),
            div(class="kpi-val", fmt(mean_rev))
        ),
        div(class=paste("kpi", if(loss_prob>25)"red" else "green"),
            div(class="kpi-lbl","Prob. of Loss"),
            div(class="kpi-val",
                style=if(loss_prob>25)"color:#c53030" else "color:#276749",
                paste0(round(loss_prob,1),"%"))
        ),
        div(class="kpi teal",
            div(class="kpi-lbl","Median Net Profit"),
            div(class=paste("kpi-val",col(med_np)), fmt(med_np))
        ),
        div(class="kpi red",
            div(class="kpi-lbl","VaR 5% (worst 1-in-20)"),
            div(class=paste("kpi-val",col(var5)), fmt(var5))
        ),
        div(class="kpi slate",
            div(class="kpi-lbl","95th Percentile"),
            div(class=paste("kpi-val",col(p95)), fmt(p95))
        )
    )
  })
  
  # ── Plot theme ─────────────────────────────────────────────
  sail_theme <- function(base=11) {
    theme_minimal(base_size=base, base_family="sans") +
      theme(
        plot.background  = element_rect(fill="#ffffff", color=NA),
        panel.background = element_rect(fill="#ffffff", color=NA),
        panel.grid.major = element_line(color="#f0f4f8", linewidth=0.4),
        panel.grid.minor = element_blank(),
        axis.text        = element_text(color="#4a5568", size=8),
        axis.title       = element_text(color="#4a5568", size=9),
        plot.title       = element_blank(),
        legend.text      = element_text(size=8),
        legend.title     = element_text(size=8, face="bold")
      )
  }
  
  # ── Histogram ──────────────────────────────────────────────
  output$hist_plot <- renderPlot({
    df <- sim_data(); req(df)
    np     <- df$net_profit
    mean_v <- mean(np)
    
    ggplot(data.frame(np=np), aes(x=np)) +
      geom_histogram(bins=80, fill="#003366", color="white",
                     alpha=0.85, linewidth=0.15) +
      geom_vline(xintercept=mean_v, color="#c8a400",
                 linewidth=1.2, linetype="dashed") +
      geom_vline(xintercept=0, color="#c53030",
                 linewidth=0.9, linetype="dotted") +
      annotate("text", x=mean_v, y=Inf,
               label=paste0(" Mean\n ₹",format(round(mean_v),big.mark=","),"Cr"),
               color="#c8a400", hjust=0, vjust=1.5, size=3) +
      scale_x_continuous(labels=label_comma(suffix=" Cr", scale=1)) +
      labs(x="Net Profit (₹ Crore)", y="Count") +
      sail_theme()
  }, bg="#ffffff")
  
  # ── CDF ────────────────────────────────────────────────────
  output$cdf_plot <- renderPlot({
    df <- sim_data(); req(df)
    
    fy25_actual <- 2372  # from Screener FY25
    
    ggplot(data.frame(np=df$net_profit), aes(x=np)) +
      stat_ecdf(color="#003366", linewidth=1.3, pad=FALSE) +
      geom_vline(xintercept=0, color="#c53030",
                 linetype="dashed", linewidth=0.8) +
      geom_vline(xintercept=fy25_actual, color="#c8a400",
                 linetype="dashed", linewidth=0.8) +
      annotate("text", x=fy25_actual, y=0.12,
               label=" FY25\n Actual", color="#c8a400", hjust=0, size=2.8) +
      annotate("text", x=0, y=0.06,
               label=" Break-even", color="#c53030", hjust=0, size=2.8) +
      scale_x_continuous(labels=label_comma(suffix=" Cr")) +
      scale_y_continuous(labels=label_percent()) +
      labs(x="Net Profit (₹ Crore)", y="Cumulative probability") +
      sail_theme()
  }, bg="#ffffff")
  
  # ── Scatter ────────────────────────────────────────────────
  output$scatter_plot <- renderPlot({
    df <- sim_data(); req(df)
    s  <- df[sample(nrow(df), min(3000, nrow(df))), ]
    
    ggplot(s, aes(x=revenue, y=net_profit, color=net_profit)) +
      geom_point(alpha=0.3, size=0.7) +
      geom_hline(yintercept=0, color="#c53030",
                 linetype="dashed", linewidth=0.8) +
      scale_color_gradient2(low="#c53030", mid="#718096",
                            high="#276749", midpoint=0, guide="none") +
      scale_x_continuous(labels=label_comma(suffix="Cr", scale=1e-3, prefix="₹")) +
      scale_y_continuous(labels=label_comma(suffix="Cr")) +
      labs(x="Revenue (₹ '000 Crore)", y="Net Profit (₹ Crore)") +
      sail_theme()
  }, bg="#ffffff")
  
  # ── OPM Histogram ──────────────────────────────────────────
  output$opm_hist <- renderPlot({
    df <- sim_data(); req(df)
    
    ggplot(data.frame(opm=df$opm_pct), aes(x=opm)) +
      geom_histogram(bins=60, fill="#c8a400", color="white",
                     alpha=0.9, linewidth=0.15) +
      geom_vline(xintercept=mean(df$opm_pct), color="#003366",
                 linewidth=1.2, linetype="dashed") +
      geom_vline(xintercept=0, color="#c53030",
                 linewidth=0.8, linetype="dotted") +
      scale_x_continuous(labels=label_number(suffix="%")) +
      labs(x="OPM (%)", y="Count") +
      sail_theme()
  }, bg="#ffffff")
  
  # ── Percentile table ───────────────────────────────────────
  output$pct_table <- renderDT({
    df <- sim_data(); req(df)
    
    probs <- c(0.01,0.05,0.10,0.20,0.25,0.50,0.75,0.80,0.90,0.95,0.99)
    np    <- df$net_profit
    
    pct_df <- data.frame(
      "Percentile"      = paste0(probs*100,"th"),
      "Net Profit (₹Cr)"= format(round(quantile(np,probs)), big.mark=","),
      "Revenue (₹Cr)"   = format(round(quantile(df$revenue,probs)), big.mark=","),
      "OPM %"           = paste0(round(quantile(df$opm_pct,probs),1),"%"),
      "Interpretation"  = c(
        "Extreme loss — 1-in-100 bad year",
        "Value at Risk — 5% chance of worse",
        "Poor year (bottom 10%)",
        "Below-average year",
        "Lower quartile",
        "Median — 50/50 outcome",
        "Upper quartile",
        "Above-average year",
        "Strong year (top 10%)",
        "Very strong year",
        "Exceptional — 1-in-100 good year"
      )
    )
    
    datatable(pct_df, rownames=FALSE, options=list(
      pageLength=11, dom='t', ordering=FALSE
    ), class="stripe hover")
  })
  
  # ── Scenario table ─────────────────────────────────────────
  output$scenario_table <- renderDT({
    df <- sim_data(); req(df)
    
    scen <- data.frame(
      "Scenario" = c("Bear (5th pct)","Weak (25th pct)","Base (Median)",
                     "Strong (75th pct)","Bull (95th pct)","FY25 Actual",
                     "FY24 Actual","FY22 Best-ever"),
      "Revenue (₹Cr)" = c(
        format(round(quantile(df$revenue,0.05)),big.mark=","),
        format(round(quantile(df$revenue,0.25)),big.mark=","),
        format(round(quantile(df$revenue,0.50)),big.mark=","),
        format(round(quantile(df$revenue,0.75)),big.mark=","),
        format(round(quantile(df$revenue,0.95)),big.mark=","),
        "1,02,479","1,05,378","1,03,477"
      ),
      "Net Profit (₹Cr)" = c(
        format(round(quantile(df$net_profit,0.05)),big.mark=","),
        format(round(quantile(df$net_profit,0.25)),big.mark=","),
        format(round(quantile(df$net_profit,0.50)),big.mark=","),
        format(round(quantile(df$net_profit,0.75)),big.mark=","),
        format(round(quantile(df$net_profit,0.95)),big.mark=","),
        "2,372","3,067","12,243"
      ),
      "Source" = c(rep("Simulation",5),"Screener","Screener","Screener")
    )
    
    datatable(scen, rownames=FALSE, options=list(
      pageLength=8, dom='t', ordering=FALSE
    ), class="stripe hover")
  })
  
  # ── Historical: Revenue + Net Profit ──────────────────────
  output$hist_rev_plot <- renderPlot({
    d <- sail_history
    
    ggplot(d, aes(x=FY)) +
      geom_col(aes(y=Revenue/1000), fill="#003366", alpha=0.8, width=0.55) +
      geom_line(aes(y=(Net_Profit/1000)+0, group=1), color="#c8a400",
                linewidth=1.2) +
      geom_point(aes(y=Net_Profit/1000), color="#c8a400",
                 size=2.5, shape=21, fill="white", stroke=1.5) +
      geom_hline(yintercept=0, color="#c53030", linetype="dashed", linewidth=0.6) +
      scale_y_continuous(
        name="Revenue (₹ '000 Cr)",
        sec.axis=sec_axis(~., name="Net Profit (₹ '000 Cr)")
      ) +
      scale_x_discrete() +
      labs(x=NULL) +
      sail_theme() +
      theme(axis.text.x=element_text(angle=40, hjust=1, size=8),
            axis.title.y=element_text(color="#003366"),
            axis.title.y.right=element_text(color="#c8a400"))
  }, bg="#ffffff")
  
  # ── Historical: OPM ────────────────────────────────────────
  output$hist_opm_plot <- renderPlot({
    d <- sail_history
    
    ggplot(d, aes(x=FY, y=OPM_pct)) +
      geom_col(aes(fill=OPM_pct>0), width=0.55, show.legend=FALSE) +
      scale_fill_manual(values=c("TRUE"="#003366","FALSE"="#c53030")) +
      geom_hline(yintercept=0, color="#4a5568", linewidth=0.5) +
      scale_x_discrete() +
      labs(x=NULL, y="OPM (%)") +
      sail_theme() +
      theme(axis.text.x=element_text(angle=40, hjust=1, size=8))
  }, bg="#ffffff")
  
  # ── Quarterly plot ─────────────────────────────────────────
  output$qtrly_plot <- renderPlot({
    d <- sail_quarterly
    d$Quarter <- factor(d$Quarter, levels=d$Quarter)
    
    ggplot(d, aes(x=Quarter)) +
      geom_col(aes(y=Revenue/1000), fill="#003366", alpha=0.75, width=0.6) +
      geom_line(aes(y=Net_Profit/1000*4, group=1), color="#c8a400",
                linewidth=1.1) +
      geom_point(aes(y=Net_Profit/1000*4), color="#c8a400",
                 size=2.2, shape=21, fill="white", stroke=1.4) +
      scale_y_continuous(
        name="Revenue (₹ '000 Cr)",
        sec.axis=sec_axis(~./4, name="Net Profit (₹ '000 Cr)")
      ) +
      labs(x=NULL) +
      sail_theme() +
      theme(axis.text.x=element_text(angle=45, hjust=1, size=7.5),
            axis.title.y=element_text(color="#003366"),
            axis.title.y.right=element_text(color="#c8a400"))
  }, bg="#ffffff")
  
  # ── Debt + ROCE ────────────────────────────────────────────
  output$debt_roce_plot <- renderPlot({
    d <- sail_history
    
    ggplot(d, aes(x=FY)) +
      geom_col(aes(y=Borrowings/1000), fill="#c8a400", alpha=0.8, width=0.55) +
      geom_line(aes(y=ROCE_pct*2000, group=1), color="#003366",
                linewidth=1.2) +
      geom_point(aes(y=ROCE_pct*2000), color="#003366",
                 size=2.5, shape=21, fill="white", stroke=1.5) +
      scale_y_continuous(
        name="Borrowings (₹ '000 Cr)",
        sec.axis=sec_axis(~./2000, name="ROCE (%)")
      ) +
      labs(x=NULL) +
      sail_theme() +
      theme(axis.text.x=element_text(angle=40, hjust=1, size=8),
            axis.title.y=element_text(color="#c8a400"),
            axis.title.y.right=element_text(color="#003366"))
  }, bg="#ffffff")
  
  # ── Historical table ───────────────────────────────────────
  output$hist_table <- renderDT({
    d <- sail_history %>%
      transmute(
        "FY"              = FY,
        "Revenue (₹Cr)"  = format(Revenue, big.mark=","),
        "Expenses (₹Cr)" = format(Expenses, big.mark=","),
        "Op.Profit (₹Cr)"= format(Op_Profit, big.mark=","),
        "OPM %"          = paste0(OPM_pct,"%"),
        "Interest (₹Cr)" = format(Interest, big.mark=","),
        "Depn (₹Cr)"     = format(Depreciation, big.mark=","),
        "Net Profit"     = format(Net_Profit, big.mark=","),
        "ROCE %"         = paste0(ROCE_pct,"%"),
        "Debt (₹Cr)"     = format(Borrowings, big.mark=","),
        "EPS (₹)"        = EPS
      )
    
    datatable(d, rownames=FALSE,
              options=list(pageLength=12, dom='t', ordering=FALSE, scrollX=TRUE),
              class="stripe hover compact"
    )
  })
}

# ================================================================
#  LAUNCH
# ================================================================
shinyApp(ui=ui, server=server)

