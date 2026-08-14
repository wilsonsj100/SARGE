# app.R

library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)

source("R/plot_sarge.R")
source("R/check_timestamps.R")


# ============================================================
# SPECIFICATIONS
# ============================================================

TIMEZONE <- "EST"


# ============================================================
# VARIABLE CHOICES
# ============================================================

exo_choices <- c(
  "DO_mgL",
  "FDOM_RFU",
  "Chlorophyll_ugL",
  "Conductivity",
  "DO_saturation",
  "Salinity_PPT",
  "TDS_mgL",
  "pH",
  "Temp_C",
  "Depth_m"
)

hydrology_choices <- c(
  "Water_depth_m",
  "Flowrate_ms",
  "Index_velocity_ms",
  "Mean_velocity_ms"
)

ghg_choices <- c(
  "H2O_ppm",
  "CO2d_ppm",
  "CH4d_ppb",
  "Cavity_pressure",
  "Cavity_temperature"
)

radon_choices <- c(
  "Relative_humidity_pct",
  "Radon_Bqm3",
  "Radon_error_Bqm3"
)


# ============================================================
# COLORS
# ============================================================

exo_colors <- c(
  "Depth_m" = "darkblue",
  "DO_mgL" = "lightblue",
  "pH" = "pink",
  "Salinity_PPT" = "grey50",
  "Temp_C" = "red",
  "FDOM_RFU" = "brown",
  "Chlorophyll_ugL" = "darkgreen",
  "Conductivity" = "grey20",
  "DO_saturation" = "lightblue",
  "TDS_mgL" = "orange"
)

hydrology_colors <- c(
  "Water_depth_m" = "darkblue",
  "Flowrate_ms" = "lightblue1",
  "Index_velocity_ms" = "lightblue3",
  "Mean_velocity_ms" = "lightblue4"
)

ghg_colors <- c(
  "H2O_ppm" = "lightblue",
  "CO2d_ppm" = "goldenrod",
  "CH4d_ppb" = "red4",
  "Cavity_pressure" = "purple4",
  "Cavity_temperature" = "red"
)

rad_colors <- c(
  "Relative_humidity_pct" = "lightblue",
  "Radon_Bqm3" = "purple",
  "Radon_error_Bqm3" = "darkblue"
)


# ============================================================
# LOAD DATA
# ============================================================

# ------------------------------------------------------------
# EXO DATA
# ------------------------------------------------------------

exo_names <- c(
  "TIMESTAMP", "RECORD", "Date", "Time", "Chlorophyll_RFU",
  "Chlorophyll_ugL", "Conductivity", "FDOM_QSU", "FDOM_RFU",
  "NLF_conductivity", "ODO_sat", "ODO_local", "ODO_MgL",
  "Pressure_psia", "Salinity_PPT", "Specific_Conductivity_uScm",
  "BGA_PE_RFU", "BGA_PE_ugL", "TDS_mg_L", "Turbidity_FNU",
  "Wiper_Position_mv", "pH", "pH_mv", "Temp_C", "Depth_m",
  "Battery_v", "Cable_v", "Wiper_Current_ma", "sn", "snn"
)

exo <- read_delim(
  "https://www.dropbox.com/scl/fi/0l884zjlaphi2nu9uupiw/WILSON_ExoTable.dat?rlkey=3ydvfbetag118ql5a03f3o9l8&st=80wnki57&dl=1",
  delim = ",",
  skip = 4,
  show_col_types = FALSE,
  col_names = exo_names
) %>%
  mutate(
    TIMESTAMP = paste(format(TIMESTAMP, "%Y-%m-%d %H:%M:%S")),
    TIMESTAMP = ymd_hms(TIMESTAMP, tz = TIMEZONE)
  ) %>%
  select(-all_of(c("sn", "snn"))) %>%
  rename(
    DO_mgL = ODO_MgL,
    DO_saturation = ODO_sat,
    TDS_mgL = TDS_mg_L
  ) %>%
  select(all_of(c(exo_choices, "TIMESTAMP")))


# ------------------------------------------------------------
# HYDROLOGY DATA
# ------------------------------------------------------------

hydrology_names <- c(
  "TIMESTAMP", "RECORD", "SONTEK_ID", "Sample_number", "yyyy",
  "MM", "dd", "hh", "Minute", "ss", "Flowrate", "Stage",
  "Mean_velocity", "Total_volume", "Water_depth",
  "Index_velocity", "Cross_area", "Water_temperature",
  "System_status", "Velocity_XZxc", "Velocity_XZzc",
  "Velocity_XZxL", "Velocity_XZxR", "Batt_Vol_Sontek",
  "Pitch_angle", "Roll_angle", "Perc_submergance", "IceScore"
)

hydrology <- read_delim(
  "https://www.dropbox.com/scl/fi/dqas4e92ngcsigqy7u107/WILSON_SontekTable.dat?rlkey=iw6azl1fqyyl521jpcgfjjcrk&st=d4xp83p3&dl=1",
  delim = ",",
  skip = 4,
  show_col_types = FALSE,
  col_names = hydrology_names
) %>%
  mutate(
    TIMESTAMP = paste(format(TIMESTAMP, "%Y-%m-%d %H:%M:%S")),
    TIMESTAMP = ymd_hms(TIMESTAMP, tz = TIMEZONE)
  ) %>%
  rename(
    Water_depth_m = Water_depth,
    Index_velocity_ms = Index_velocity,
    Mean_velocity_ms = Mean_velocity,
    Flowrate_ms = Flowrate
  ) %>%
  select(all_of(c(hydrology_choices, "TIMESTAMP")))


# ------------------------------------------------------------
# GHG DATA
# ------------------------------------------------------------

ghg_names <- c(
  "TIMESTAMP", "RECORD", "TimeLong7810", "LI7810_time", "Seconds",
  "Nanoseconds", "NDX", "Diag", "H2O", "CO2", "CH4", "cavity_p",
  "cavity_t", "laser_phase_p", "laser_t", "residual", "ringdown",
  "thermal_enc_t", "phase_err", "laser_t_shift", "vin", "cavity_p_sp",
  "board_t", "case_t", "exfit_tau_rd1", "peak_ht_1", "exfit_tau_rd2",
  "peak_ht_2", "tec_sp", "phase_ctrl_out", "meas_phase",
  "laser_phase_sp", "num_peaks_win", "post_ringdwn_1_ign_len",
  "ringdwn_1_loc", "peak_x0", "xderiv_weight", "xderiv2_weight",
  "ch4_spec_weight", "co2_spec_weight", "h2o_spec_weight",
  "start_freq", "cavity_htr_w", "laser_ref_pwr", "offset_weight",
  "slope_weight", "evenodd_weight", "evenodd_lin_weight",
  "num_skipped_modes", "pump_sp", "pump_tach_ph"
)

ghg <- read_delim(
  "https://www.dropbox.com/scl/fi/gtmjx6216pfqnmb5cd1yc/WILSON_FLUX_7810.dat?rlkey=zfeh41fxjeyd3jn9sqf76zfza&st=6ljj68yu&dl=1",
  delim = ",",
  skip = 4,
  show_col_types = FALSE,
  col_names = ghg_names
) %>%
  mutate(
    TIMESTAMP = paste(format(TIMESTAMP, "%Y-%m-%d %H:%M:%S")),
    TIMESTAMP = ymd_hms(TIMESTAMP, tz = TIMEZONE)
  ) %>%
  rename(
    CH4d_ppb = CH4,
    CO2d_ppm = CO2,
    H2O_ppm = H2O,
    Cavity_pressure = cavity_p,
    Cavity_temperature = cavity_t
  ) %>%
  select(all_of(c(ghg_choices, "TIMESTAMP"))) %>%
  mutate(
    TIMESTAMP = round_date(TIMESTAMP, "minute")
  ) %>%
  group_by(TIMESTAMP) %>%
  summarize(
    across(everything(), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )


# ------------------------------------------------------------
# RADON DATA
# ------------------------------------------------------------

radon_names <- c(
  "TIMESTAMP", "RECORD", "Record_RAD7", "Year_RAD7", "Month_RAD7",
  "Day_Rad7", "Hour_Rad7", "Minute_Rad7", "Total_Counts_Rad7",
  "Live_Time_Rad7", "PER_TOT_A_Rad7", "PER_TOT_B_Rad7",
  "PER_TOT_C_Rad7", "PER_TOT_D_Rad7", "High_Voltage_Level_Rad7",
  "High_Voltage_Duty_Rad7", "Temp_sample_Rad7", "RH_sample_Rad7",
  "Leakage_Current_Rad7", "Battery_Volt_Rad7", "Pump_current_Rad7",
  "Flags_Byte_Rad7", "Radon_concentration_Rad7",
  "Radon_concentration_uncertainty_Rad7", "Units_Byt_Rad7"
)

radon <- read_delim(
  "https://www.dropbox.com/scl/fi/ttjde8cpqf7xgn8dgcdv2/WILSON_Rad7Table.dat?rlkey=hcg2ycle3cmkerc6ic7pa13g1&st=vx91f41x&dl=1",
  delim = ",",
  skip = 4,
  show_col_types = FALSE,
  col_names = radon_names
) %>%
  mutate(
    TIMESTAMP = paste(format(TIMESTAMP, "%Y-%m-%d %H:%M:%S")),
    TIMESTAMP = ymd_hms(TIMESTAMP, tz = TIMEZONE)
  ) %>%
  rename(
    Relative_humidity_pct = RH_sample_Rad7,
    Radon_Bqm3 = Radon_concentration_Rad7,
    Radon_error_Bqm3 = Radon_concentration_uncertainty_Rad7
  ) %>%
  select(all_of(c(radon_choices, "TIMESTAMP")))


# ============================================================
# DATE RANGE
# ============================================================

min_date <- min(
  c(
    exo$TIMESTAMP,
    hydrology$TIMESTAMP,
    ghg$TIMESTAMP,
    radon$TIMESTAMP
  ),
  na.rm = TRUE
)

max_date <- max(
  c(
    exo$TIMESTAMP,
    hydrology$TIMESTAMP,
    ghg$TIMESTAMP,
    radon$TIMESTAMP
  ),
  na.rm = TRUE
)


# ============================================================
# UI
# ============================================================

ui <- fixedPage(
  
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "custom.css"
    )
  ),
    
    # Loading overlay JavaScript
    tags$script(HTML("
      Shiny.addCustomMessageHandler(
        'hideLoadingOverlay',
        function(message) {
          const overlay = document.getElementById('loading-overlay');

          if (overlay) {
            overlay.classList.add('hidden');

            setTimeout(
              () => overlay.remove(),
              600
            );
          }
        }
      );
    ")),
  
  
  # ----------------------------------------------------------
  # TITLE BANNER
  # ----------------------------------------------------------
  
  div(
    class = "quarto-title-banner",
    div(
      class = "banner-content",
      h1(class = "title",
         "SARGE Real-Time Data Dashboard"))
    ),
  
  
  # ----------------------------------------------------------
  # LOADING OVERLAY
  # ----------------------------------------------------------
  
  div(
    id = "loading-overlay",
    div(class = "spinner"),
    p("Loading the latest SARGE data…")
  ),
  
  
  # ----------------------------------------------------------
  # DATE INPUTS
  # ----------------------------------------------------------
  
  div(
    class = "date-controls",
    
    fluidRow(
      column(
        width = 3,
        dateInput(
          "start_date",
          "Start date for plots",
          value = max_date - days(3),
          min = as.Date(min_date),
          max = Sys.Date()
        )
      ),
      
      column(
        width = 3,
        dateInput(
          "end_date",
          "End date for plots",
          value = as.Date(max_date),
          min = as.Date(min_date),
          max = Sys.Date()
        )
      )
    )
  ),
  
  
  # ----------------------------------------------------------
  # MAIN TABS
  # ----------------------------------------------------------
  
  tabsetPanel(
    
    id = "main_tabs",
    type = "tabs",
    
    # ========================================================
    # OVERVIEW
    # ========================================================
    
    tabPanel(
      title = "Overview",
      fluidRow(
        column(
          width = 8,
          div(
            class = "overview-text",
            br(),
            htmlOutput("today"),
            h3("Latest data"),
            htmlOutput("data_latest")
          )
        ),
        
        column(
          width = 4,
          div(
            class = "overview-images",
            img(
              src = "images/SARGE_BKGD_SHINY.jpg",
              alt = "SARGE in the field"
            ),
            
            img(
              src = "images/GCW_HighTide.png",
              alt = "marsh at high tide"
            )
          )
        )
      )
    ),
    
    
    # ========================================================
    # PHYSIOCHEMISTRY
    # ========================================================
    
    tabPanel(
      title = "Physiochemistry",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            "exo_vars",
            h4("Variables to plot"),
            choices = exo_choices,
            selected = c("Depth_m", "Salinity_PPT")
          ),
          width = 3
        ),
        
        mainPanel(
          plotlyOutput("plot_exo", height = "400px"),
          br(),
          h3("Table of all physiochemistry data"),
          div(
            DTOutput("table_exo"),
            style = "font-size:80%;"
          ),
          width = 9
        )
      )
    ),
    
    
    # ========================================================
    # HYDROLOGY
    # ========================================================
    
    tabPanel(
      title = "Hydrology",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            "hydrology_vars",
            h4("Variables to plot"),
            choices = hydrology_choices,
            selected = c("Water_depth_m", "Index_velocity_ms")),
          width = 3
        ),
        
        mainPanel(
          plotlyOutput("plot_hydrology", height = "400px"),
          br(),
          h3("Table of all hydrology data"),
          div(
            DTOutput("table_hydrology"),
            style = "font-size:80%;"
          ),
          width = 9
        )
      )
    ),
    
    
    # ========================================================
    # GHG
    # ========================================================
    
    tabPanel(
      title = "GHG",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            "ghg_vars",
            h4("Variables to plot"),
            choices = ghg_choices,
            selected = c("CH4d_ppb", "CO2d_ppm")),
          width = 3
        ),
        
        mainPanel(
          plotlyOutput("plot_ghg", height = "400px"),
          br(),
          h3("Table of all GHG data"),
          div(
            DTOutput("table_ghg"),
            style = "font-size:80%;"
          ),
          width = 9
        )
      )
    ),
    
    
    # ========================================================
    # RADON
    # ========================================================
    
    tabPanel(
      title = "Radon",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            "rad_vars",
            h4("Variables to plot"),
            choices = radon_choices,
            selected = c(
              "Radon_Bqm3",
              "Radon_error_Bqm3"
            )
          ),
          width = 3
        ),
        
        mainPanel(
          plotlyOutput(
            "plot_radon",
            height = "400px"
          ),
          br(),
          h3("Table of all radon data"),
          div(
            DTOutput("table_radon"),
            style = "font-size:80%;"
          ),
          width = 9
        )
      )
    )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  Sys.setenv(TZ = TIMEZONE)
  
  # ----------------------------------------------------------
  # DATE INPUTS
  # ----------------------------------------------------------
  
  updateDateInput(
    session,
    inputId = "start_date",
    min = as.Date(min_date),
    max = Sys.Date(),
    value = as.Date(max_date) - days(3)
  )
  
  updateDateInput(
    session,
    inputId = "end_date",
    min = as.Date(min_date),
    max = Sys.Date(),
    value = as.Date(max_date)
  )

  
  # ==========================================================
  # PLOTS
  # ==========================================================
  
  output$plot_exo <- renderPlotly({
    
    p1 <- plot_sarge(
      exo,
      input$start_date,
      input$end_date,
      input$exo_vars,
      exo_colors
    )
    
    ggplotly(
      p1, height = 400, width = 600,
      tooltip = c("TIMESTAMP", "value")
    )
  })
  
  
  output$plot_hydrology <- renderPlotly({
    
    p1 <- plot_sarge(
      hydrology,
      input$start_date,
      input$end_date,
      input$hydrology_vars,
      hydrology_colors
    )
    
    ggplotly(
      p1, height = 400, width = 600,
      tooltip = c("TIMESTAMP", "value")
    )
  })
  
  
  output$plot_ghg <- renderPlotly({
    
    p1 <- plot_sarge(
      ghg,
      input$start_date,
      input$end_date,
      input$ghg_vars,
      ghg_colors
    )
    
    ggplotly(
      p1, height = 400, width = 600,
      tooltip = c("TIMESTAMP", "value")
    )
  })
  
  
  output$plot_radon <- renderPlotly({
    
    p1 <- plot_sarge(
      radon,
      input$start_date,
      input$end_date,
      input$rad_vars,
      rad_colors
    )
    
    ggplotly(
      p1, height = 400, width = 600,
      tooltip = c("TIMESTAMP", "value")
    )
  })
  
  
  # ==========================================================
  # DATA TABLES
  # ==========================================================
  
  output$table_exo <- DT::renderDataTable({
    exo %>%
      arrange(desc(TIMESTAMP)) %>%
      mutate(TIMESTAMP = format(TIMESTAMP, "%Y-%m-%d %H:%M"))
  }, rownames = FALSE, class = "display nowrap")
  
  
  output$table_hydrology <- DT::renderDataTable({
    hydrology %>%
      arrange(desc(TIMESTAMP)) %>%
      mutate(TIMESTAMP = format(TIMESTAMP, "%Y-%m-%d %H:%M"))
  }, rownames = FALSE, class = "display nowrap")
  
  
  output$table_ghg <- DT::renderDataTable({
    ghg %>%
      arrange(desc(TIMESTAMP)) %>%
      mutate(TIMESTAMP = format(TIMESTAMP, "%Y-%m-%d %H:%M"))
  }, rownames = FALSE, class = "display nowrap")
  
  
  output$table_radon <- DT::renderDataTable({
    radon %>%
      arrange(desc(TIMESTAMP)) %>%
      mutate(TIMESTAMP = format(TIMESTAMP, "%Y-%m-%d %H:%M"))
  }, rownames = FALSE, class = "display nowrap")
  
  
  # ==========================================================
  # CURRENT TIME
  # ==========================================================
  
  now_txt <- reactiveTimer(1000)
  
  output$today <- renderText({
    paste(
      "Current time: ",
      format(
        now_txt(),
        "%Y-%m-%d %H:%M:%S"
      ), TIMEZONE)
  })
  
  
  # ==========================================================
  # LATEST DATA
  # ==========================================================
  
  output$data_latest <- renderText({
    current_time <- now_txt()
    
    exo_ts <- check_timestamps(exo, TIMEZONE, current_time)
    hydrology_ts <- check_timestamps(hydrology, TIMEZONE, current_time)
    ghg_ts <- check_timestamps(ghg, TIMEZONE, current_time)
    radon_ts <- check_timestamps(radon, TIMEZONE, current_time)
    
    
    # --------------------------------------------------------
    # HTML
    # --------------------------------------------------------
    
    paste0(
      "<ul>",
      "<li", exo_ts[[2]], ">",
      "<b>EXO2 water quality sonde:</b> ", exo_ts[[1]],"</li>",
      
      "<li", hydrology_ts[[2]], ">",
      "<b>SontekIQ ADCP:</b> ",hydrology_ts[[1]],"</li>",
      
      "<li", ghg_ts[[2]], ">",
      "<b>GHG analyzer:</b> ",ghg_ts[[1]],"</li>",
      
      "<li", radon_ts[[2]], ">",
      "<b>Radon detector:</b> ",radon_ts[[1]],"</li>",
      "</ul>"
    )
  })
  
  
  # ==========================================================
  # HIDE LOADING SCREEN
  # ==========================================================
  
  observe({
    
    req(
      nrow(exo) > 0,
      nrow(hydrology) > 0,
      nrow(ghg) > 0,
      nrow(radon) > 0
    )
    
    session$sendCustomMessage(
      "hideLoadingOverlay",
      TRUE
    )
  })
}


# ============================================================
# RUN APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)