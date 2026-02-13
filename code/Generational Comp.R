# How have homeownership rates changed across generations by age?
# Ben Glasner | 12/09/2024

# Preserve SKIP_SHINY flag if set by run_all.R, then clean environment
.skip_shiny <- exists("SKIP_SHINY") && isTRUE(SKIP_SHINY)
rm(list = ls())
SKIP_SHINY <- .skip_shiny
rm(.skip_shiny)

options(scipen = 999)
set.seed(42)

###########################
###   Load Packages     ###
###########################
library(dplyr)
library(tidyr)
library(ipumsr)
library(fredr)
library(stringr)
library(tigris)
library(tidycensus)
library(purrr)
library(plotly)
library(scales)
library(ggplot2)
library(ggrepel)

library(shiny)
library(tidyverse)
library(forcats)

###########################
###   Set Paths         ###
###########################
# Locate project root (same strategy as 00a_ipums_api_extract.R)
# Resolution order:
#   1. Environment variable HOMEOWNERSHIP_BY_GEN_PROJECT_ROOT
#   2. This script's own location (code/ is one level below project root)
#   3. Walk upward from getwd() looking for project markers (README.md + data/)

has_project_markers <- function(path) {
  if (!dir.exists(path)) return(FALSE)
  readme_ok <- file.exists(file.path(path, "README.md"))
  data_ok   <- dir.exists(file.path(path, "data")) || dir.exists(file.path(path, "Data"))
  readme_ok && data_ok
}

# Strategy 1: Environment variable
path_project <- Sys.getenv("HOMEOWNERSHIP_BY_GEN_PROJECT_ROOT", unset = "")

# Strategy 2: Derive from this script's own file path
if (!nzchar(path_project)) {
  this_file <- tryCatch(
    normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
    error = function(e) NA_character_
  )
  if (!is.na(this_file) && nzchar(this_file)) {
    candidate <- dirname(dirname(this_file))
    if (has_project_markers(candidate)) {
      path_project <- candidate
    }
  }
}

# Strategy 3: Walk upward from getwd()
if (!nzchar(path_project)) {
  current <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in 1:15) {
    if (has_project_markers(current)) {
      path_project <- current
      break
    }
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
}

if (!nzchar(path_project) || is.na(path_project) || !dir.exists(path_project)) {
  stop(
    "Could not locate project root.\n\n",
    "Fix by either:\n",
    "  (1) Set env var: Sys.setenv(HOMEOWNERSHIP_BY_GEN_PROJECT_ROOT = '/path/to/project')\n",
    "  (2) Run from within the project directory (must contain README.md and data/)\n",
    call. = FALSE
  )
}

path_project <- normalizePath(path_project, winslash = "/", mustWork = TRUE)
path_data    <- file.path(path_project, "data")
path_output  <- file.path(path_project, "output")

###########################
###   Load IPUMS Data   ###
###########################
# Auto-detect the most recent CPS DDI file in data/
find_latest_cps_ddi <- function(data_dir) {
  xmls <- list.files(data_dir, pattern = "^cps_.*\\.xml$", full.names = TRUE, ignore.case = TRUE)
  if (length(xmls) == 0) stop("No cps_*.xml found in: ", data_dir, call. = FALSE)
  xmls[which.max(file.info(xmls)$mtime)]
}

ddi  <- read_ipums_ddi(find_latest_cps_ddi(path_data))
data <- read_ipums_micro(ddi)

###########################
###   Load PCE Deflator ###
###########################
# Pull annual PCE price index from FRED (series PCEPI, 2017=100)
# FRED API key: set via FRED_API_KEY env var or Sys.setenv(FRED_API_KEY = "your_key")
# Get a free key at: https://fred.stlouisfed.org/docs/api/api_key.html
fred_key <- Sys.getenv("FRED_API_KEY", unset = "")
if (!nzchar(fred_key)) {
  stop(
    "\n===========================================\n",
    "FRED API key not found!\n\n",
    "Please either:\n",
    "  1. Add to .Renviron file: FRED_API_KEY=your_key\n",
    "  2. Set in R session: Sys.setenv(FRED_API_KEY = 'your_key')\n\n",
    "Get your free key from: https://fred.stlouisfed.org/docs/api/api_key.html\n",
    "===========================================\n",
    call. = FALSE
  )
}
fredr_set_key(fred_key)

PCE <- fredr(
  series_id        = "PCEPI",
  observation_start = as.Date("1960-01-01"),
  frequency        = "a"
) %>%
  transmute(
    YEAR = as.integer(format(date, "%Y")),
    PCE  = value
  )


data <- data %>%
  filter(YEAR >= 1976, AGE >= 19) %>%
  left_join(PCE, by = "YEAR") %>%
  mutate(
    birthyear = YEAR - AGE,
    
    generation = case_when(
      birthyear <= 1927 ~ "Greatest",
      birthyear %in% 1928:1945 ~ "Silent",
      birthyear %in% 1946:1964 ~ "Boomers",
      birthyear %in% 1965:1980 ~ "Gen-X",
      birthyear %in% 1981:1996 ~ "Millennials",
      birthyear %in% 1997:2012 ~ "Gen-Z",
      birthyear >= 2013 ~ "Gen-Alpha",
      TRUE ~ NA_character_
    ),
    
    home_owner = case_when(
      RELATE %in% c(101, 201, 202, 203) & OWNERSHP == 10        ~ 1,
      RELATE %in% c(101, 201, 202, 203) & OWNERSHP %in% c(21,22) ~ 0,
      RELATE %in% 301:1260                                       ~ 0,
      TRUE                                                       ~ NA_real_
    ),
    
    home_owner_worst = case_when(
      OWNERSHP == 10        ~ 1,
      OWNERSHP %in% c(21,22) ~ 0,
      TRUE                  ~ NA_real_
    ),
    
    home_owner_bad = case_when(
      RELATE %in% c(101, 201, 202, 203, 501, 701) & OWNERSHP == 10        ~ 1,
      RELATE %in% c(101, 201, 202, 203, 501, 701) & OWNERSHP %in% c(21,22) ~ 0,
      RELATE %in% 901:1260                                       ~ 0,
      TRUE                                                       ~ NA_real_
    ),
    
    INCWAGE = if_else(INCWAGE %in% c(99999998, 99999999), NA_real_, INCWAGE * 100 / PCE)
  )

#########################################
###   Summarize Homeownership Rates   ###
#########################################
age_averages <- data %>%
  group_by(generation, AGE) %>%
  summarise(
    home_owner_avg = round(100*weighted.mean(home_owner, w = ASECWTH, na.rm = TRUE),1),
    
    .groups = "drop"
  )

age_averages_married <- data %>%
  mutate(
    Married = case_when(
      MARST == 1 ~ "Married",
      MARST == 6 ~ "Never Married",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Married)) %>%
  group_by(generation, AGE, Married) %>%
  summarise(
    home_owner_avg = round(100 * weighted.mean(home_owner, w = ASECWTH, na.rm = TRUE), 1),
    .groups = "drop"
  )

age_averages_sex <- data %>%
  mutate(
    Sex = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Sex)) %>%
  group_by(generation, AGE, Sex) %>%
  summarise(
    home_owner_avg = round(100 * weighted.mean(home_owner, w = ASECWTH, na.rm = TRUE), 1),
    .groups = "drop"
  )

age_averages_race <- data %>%
  mutate(
    Race = case_when(
      RACE == 100 ~ "White",
      RACE == 200 ~ "Black",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(Race)) %>%
  group_by(generation, AGE, Race) %>%
  summarise(
    home_owner_avg = round(100 * weighted.mean(home_owner, w = ASECWTH, na.rm = TRUE), 1),
    .groups = "drop"
  )


age_averages_metro <- data %>%
  mutate(
    City = case_when(
      METRO == 1 ~ "Not in metropolitan area",
      METRO == 2 ~ "In central/principal city",
      METRO == 3 ~ "Not in central/principal city",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(City)) %>%
  group_by(generation, AGE, City) %>%
  summarise(
    home_owner_avg = round(100 * weighted.mean(home_owner, w = ASECWTH, na.rm = TRUE), 1),
    .groups = "drop"
  )

age_averages_married_metro <- data %>%
  filter(METRO %in% c(1,2,3)) %>%
  mutate(
    Married = case_when(
      MARST == 1 ~ "Married",
      MARST == 6 ~ "Never Married",
      TRUE ~ NA_character_
    ),
    City = case_when(
      METRO == 1 ~ "Not in metropolitan area",
      METRO == 2 ~ "In central/principal city",
      METRO == 3 ~ "Not in central/principal city",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Married), !is.na(City)) %>%
  group_by(generation, AGE, Married, City) %>%
  summarise(
    home_owner_avg = round(100 * weighted.mean(home_owner, w = ASECWTH, na.rm = TRUE), 1),
    .groups = "drop"
  )


age_averages_wide <- age_averages %>%
  pivot_wider(
    names_from = generation,
    values_from = home_owner_avg
  )

writexl::write_xlsx(age_averages_wide, path = file.path(path_output, "Ownership_rate_by_generation.xlsx"))


###############################################
###   Static ggplot2 figure (EIG style)     ###
###############################################

# Source the EIG theme
source(file.path(path_project, "code", "theme_eig.R"))

# Define generation display order (exclude Gen-Alpha — only has ages under 20)
gen_order <- c("Greatest", "Silent", "Boomers", "Gen-X", "Millennials", "Gen-Z")

# Filter data for the plot: ages 20–65, exclude Gen-Alpha
plot_data <- age_averages %>%
  filter(generation %in% gen_order, AGE >= 20, AGE <= 65) %>%
  mutate(generation = factor(generation, levels = gen_order))

# --- Build ribbon data: Gen-X vs Millennials gap ---
ribbon_genx_mill <- plot_data %>%
  filter(generation %in% c("Gen-X", "Millennials")) %>%
  select(generation, AGE, home_owner_avg) %>%
  pivot_wider(names_from = generation, values_from = home_owner_avg) %>%
  filter(!is.na(`Gen-X`) & !is.na(Millennials))

# --- Build ribbon data: Millennials vs Gen-Z gap ---
ribbon_mill_genz <- plot_data %>%
  filter(generation %in% c("Millennials", "Gen-Z")) %>%
  select(generation, AGE, home_owner_avg) %>%
  pivot_wider(names_from = generation, values_from = home_owner_avg) %>%
  filter(!is.na(Millennials) & !is.na(`Gen-Z`))

# --- Endpoint labels: rightmost point of each generation line ---
label_data <- plot_data %>%
  group_by(generation) %>%
  filter(AGE == max(AGE)) %>%
  ungroup()

# --- Build the plot ---
p <- ggplot(plot_data, aes(x = AGE, y = home_owner_avg, color = generation)) +
  # Shaded ribbon: Gen-X to Millennials
  geom_ribbon(
    data = ribbon_genx_mill,
    aes(x = AGE, ymin = Millennials, ymax = `Gen-X`),
    inherit.aes = FALSE,
    fill = eig_ribbon_fill, alpha = 0.4
  ) +
  # Shaded ribbon: Millennials to Gen-Z
  geom_ribbon(
    data = ribbon_mill_genz,
    aes(x = AGE, ymin = `Gen-Z`, ymax = Millennials),
    inherit.aes = FALSE,
    fill = eig_ribbon_fill, alpha = 0.4
  ) +
  # Generation lines
  geom_line(linewidth = 0.9) +
  # Direct endpoint labels (no legend)
  geom_text_repel(
    data = label_data,
    aes(label = generation),
    direction = "y",
    hjust = 0,
    nudge_x = 0.5,
    segment.color = NA,
    color = "#555555",
    size = 3.5,
    family = eig_font_family
  ) +
  # Color scale
  scale_color_manual(values = eig_gen_colors) +
  # Axis scales
  scale_x_continuous(breaks = seq(20, 65, 5), limits = c(20, 67)) +
  scale_y_continuous(breaks = seq(0, 80, 20), limits = c(0, 85)) +
  # Labels
  labs(
    title = "Homeownership rate across the generations",
    subtitle = "1976 to 2024",
    caption = "Chart: Ben Glasner | Economic Innovation Group\nSource: CPS ASEC via IPUMS",
    x = NULL,
    y = NULL
  ) +
  # Apply EIG theme
  theme_eig()

# Save the figure
ggsave(
  filename = file.path(path_output, "homeownership_by_generation.png"),
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)

###############################################
###   Married vs Never Married figure       ###
###############################################

plot_data_marr <- age_averages_married %>%
  filter(generation %in% gen_order, AGE >= 20, AGE <= 65) %>%
  mutate(generation = factor(generation, levels = gen_order))

# --- Ribbons per marital-status panel ---
ribbon_genx_mill_marr <- plot_data_marr %>%
  filter(generation %in% c("Gen-X", "Millennials")) %>%
  select(Married, generation, AGE, home_owner_avg) %>%
  pivot_wider(names_from = generation, values_from = home_owner_avg) %>%
  filter(!is.na(`Gen-X`) & !is.na(Millennials))

ribbon_mill_genz_marr <- plot_data_marr %>%
  filter(generation %in% c("Millennials", "Gen-Z")) %>%
  select(Married, generation, AGE, home_owner_avg) %>%
  pivot_wider(names_from = generation, values_from = home_owner_avg) %>%
  filter(!is.na(Millennials) & !is.na(`Gen-Z`))

# --- Endpoint labels per panel ---
label_data_marr <- plot_data_marr %>%
  group_by(Married, generation) %>%
  filter(AGE == max(AGE)) %>%
  ungroup()

# --- Build the faceted plot ---
p_marr <- ggplot(plot_data_marr, aes(x = AGE, y = home_owner_avg, color = generation)) +
  geom_ribbon(
    data = ribbon_genx_mill_marr,
    aes(x = AGE, ymin = Millennials, ymax = `Gen-X`),
    inherit.aes = FALSE,
    fill = eig_ribbon_fill, alpha = 0.4
  ) +
  geom_ribbon(
    data = ribbon_mill_genz_marr,
    aes(x = AGE, ymin = `Gen-Z`, ymax = Millennials),
    inherit.aes = FALSE,
    fill = eig_ribbon_fill, alpha = 0.4
  ) +
  geom_line(linewidth = 0.9) +
  geom_text_repel(
    data = label_data_marr,
    aes(label = generation),
    direction = "y",
    hjust = 0,
    nudge_x = 0.5,
    segment.color = NA,
    color = "#555555",
    size = 3,
    family = eig_font_family
  ) +
  scale_color_manual(values = eig_gen_colors) +
  scale_x_continuous(breaks = seq(20, 65, 5), limits = c(20, 70)) +
  scale_y_continuous(breaks = seq(0, 80, 20), limits = c(0, 85)) +
  facet_wrap(~ Married) +
  labs(
    title = "Homeownership rate across the generations",
    subtitle = "By marital status, 1976 to 2024",
    caption = "Chart: Ben Glasner | Economic Innovation Group\nSource: CPS ASEC via IPUMS",
    x = NULL,
    y = NULL
  ) +
  theme_eig() +
  theme(
    strip.text = element_text(
      family = eig_font_family, face = "bold",
      size = 12, colour = "#2A6B4B", hjust = 0
    ),
    strip.background = element_blank()
  )

ggsave(
  filename = file.path(path_output, "homeownership_by_generation_married.png"),
  plot = p_marr,
  width = 12,
  height = 6,
  dpi = 300
)

age_averages_married_wide <- age_averages_married %>%
  mutate(generation_married = paste(generation,"-",Married)) %>%
  select(generation_married, AGE, home_owner_avg) %>% 
  pivot_wider(
    names_from = generation_married,
    values_from = home_owner_avg
  )

writexl::write_xlsx(age_averages_married_wide, path = file.path(path_output, "Ownership_rate_by_generation_married.xlsx"))


#################################

# ---- Generation metadata ----
gen_lookup <- tibble(
  generation = c("Greatest", "Silent", "Boomers", "Gen-X", "Millennials", "Gen-Z", "Gen-Alpha"),
  birth_start = c(1901, 1928, 1946, 1965, 1981, 1997, 2013),
  birth_end   = c(1927, 1945, 1964, 1980, 1996, 2012, 2025),
  color       = c("#1a654d", "#356859", "#da9969", "#e1ad28", "#f3d9b1", "#eee5d9", "#fdf6ec")
)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Homeownership by Age and Generation"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("outcome_var", "Select Outcome Variable:",
                  choices = c("Homeownership" = "home_owner", "Wage Income" = "INCWAGE"),
                  selected = "home_owner"),
      
      selectizeInput(
        "group_vars",
        "Group By (up to 2):",
        choices = c("Married", "City", "Sex", "Race"),
        multiple = TRUE,
        options = list(maxItems = 2, placeholder = 'None')
      ),
      
      uiOutput("generation_filter"),
      uiOutput("group_filters"),
      uiOutput("facet_selector"),
      
      sliderInput("selected_year", "Select Year to Highlight Generations:",
                  min = 1976, max = 2024, value = 2024, step = 1, sep = "")
    ),
    
    mainPanel(
      plotlyOutput("homeownershipPlot", height = "700px")
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # Clean and enrich base data
  data_prepped <- reactive({
    data %>%
      mutate(
        Married = case_when(MARST == 1 ~ "Married", MARST == 6 ~ "Never Married", TRUE ~ NA_character_),
        City = case_when(
          METRO == 1 ~ "Not in metropolitan area",
          METRO == 2 ~ "In central/principal city",
          METRO == 3 ~ "Not in central/principal city",
          TRUE ~ NA_character_
        ),
        Sex = case_when(SEX == 1 ~ "Male", SEX == 2 ~ "Female", TRUE ~ NA_character_),
        Race = case_when(
          RACE == 100 ~ "White",
          RACE == 200 ~ "Black",
          TRUE ~ "Other"
        )
      ) %>%
      filter(AGE >= 19)
  })
  
  # Generation filter (unconditional)
  output$generation_filter <- renderUI({
    gens <- unique(data$generation)
    gens <- gens[!is.na(gens)]
    
    tagList(
      checkboxGroupInput("selected_generations", "Select Generations to Plot:",
                         choices = gens, selected = gens),
      actionButton("select_all_gen", "Select All Generations"),
      actionButton("clear_gen", "Clear All Generations")
    )
  })
  
  observeEvent(input$select_all_gen, {
    updateCheckboxGroupInput(session, "selected_generations", selected = unique(data$generation))
  })
  observeEvent(input$clear_gen, {
    updateCheckboxGroupInput(session, "selected_generations", selected = character(0))
  })
  
  # Conditional filters for group vars
  output$group_filters <- renderUI({
    req(input$group_vars)
    vars <- input$group_vars
    base <- data_prepped()
    
    filter_ui <- map(vars, function(var) {
      choices <- base %>%
        filter(!is.na(.data[[var]])) %>%
        distinct(.data[[var]]) %>%
        pull() %>%
        sort()
      
      input_id <- paste0("filter_", var)
      
      tagList(
        checkboxGroupInput(inputId = input_id,
                           label = paste("Filter", var),
                           choices = choices,
                           selected = choices),
        fluidRow(
          column(6, actionButton(paste0("select_all_", var), "Select All")),
          column(6, actionButton(paste0("clear_", var), "Clear"))
        )
      )
    })
    
    tagList(filter_ui)
  })
  
  observe({
    vars <- input$group_vars
    base <- data_prepped()
    for (var in vars) {
      choices <- base %>%
        filter(!is.na(.data[[var]])) %>%
        distinct(.data[[var]]) %>%
        pull() %>%
        sort()
      
      observeEvent(input[[paste0("select_all_", var)]], {
        updateCheckboxGroupInput(session, paste0("filter_", var), selected = choices)
      })
      
      observeEvent(input[[paste0("clear_", var)]], {
        updateCheckboxGroupInput(session, paste0("filter_", var), selected = character(0))
      })
    }
  })
  
  # Facet UI only shows selected group vars
  output$facet_selector <- renderUI({
    vars <- input$group_vars
    if (length(vars) == 0) return(NULL)
    
    checkboxGroupInput(
      "facet_vars", 
      "Facet Plot By (up to 2):", 
      choices = vars,
      selected = NULL
    )
  })
  
  # Final grouped + filtered data
  grouped_data <- reactive({
    vars <- input$group_vars
    outcome <- input$outcome_var
    base <- data_prepped()
    group_vars <- c("generation", "AGE", vars)
    
    df <- base %>% filter(!is.na(.data[[outcome]]))
    
    # Filter by selected generations
    if (!is.null(input$selected_generations)) {
      df <- df %>% filter(generation %in% input$selected_generations)
    }
    
    # Drop NA in grouping vars & apply filters
    if (length(vars) > 0) {
      df <- df %>% filter(across(all_of(vars), ~ !is.na(.)))
      for (v in vars) {
        selected_vals <- input[[paste0("filter_", v)]]
        if (!is.null(selected_vals)) {
          df <- df %>% filter(.data[[v]] %in% selected_vals)
        }
      }
    }
    
    df %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        avg_value = if (outcome == "home_owner") {
          round(100 * weighted.mean(home_owner, w = ASECWTH, na.rm = TRUE), 1)
        } else {
          round(weighted.mean(INCWAGE, w = ASECWTH, na.rm = TRUE), 0)
        },
        .groups = "drop"
      ) %>%
      mutate(
        group = if (length(vars) == 0) generation else do.call(paste, c(across(all_of(c("generation", vars))), sep = ", ")),
        generation_extracted = str_extract(group, paste(gen_lookup$generation, collapse = "|")),
        group_ordered = fct_reorder(group, as.integer(factor(generation_extracted, levels = gen_lookup$generation))),
        AGE = as.numeric(AGE)
      ) %>%
      filter(AGE <= 65)
  })
  
  # ---- PLOT ----
  output$homeownershipPlot <- renderPlotly({
    df <- grouped_data()
    year <- input$selected_year
    facet_vars <- input$facet_vars
    if (length(facet_vars) > 2) facet_vars <- facet_vars[1:2]
    
    ytitle <- if (input$outcome_var == "home_owner") "Homeownership Rate (%)" else "Average Wage Income ($)"
    
    bands <- gen_lookup %>%
      mutate(min_age = pmax(0, year - birth_end), max_age = pmin(100, year - birth_start)) %>%
      filter(min_age <= 65, max_age >= 0)
    
    make_plot <- function(data_slice, title_label) {
      plt <- plot_ly(
        data = data_slice,
        x = ~AGE,
        y = ~avg_value,
        color = ~group_ordered,
        type = 'scatter',
        mode = 'lines',
        hoverinfo = 'text',
        text = ~paste0("Age: ", AGE, "<br>Group: ", group, "<br>Value: ", avg_value)
      )
      for (i in 1:nrow(bands)) {
        plt <- plt %>% add_segments(
          x = bands$min_age[i], xend = bands$max_age[i],
          y = max(data_slice$avg_value, na.rm = TRUE) * 0.1,
          yend = max(data_slice$avg_value, na.rm = TRUE) * 0.1,
          line = list(color = bands$color[i], width = 4),
          showlegend = FALSE,
          hoverinfo = "text",
          text = paste0(bands$generation[i], ": Age ", bands$min_age[i], "–", bands$max_age[i])
        )
      }
      plt %>% layout(
        title = title_label,
        xaxis = list(title = "Age", range = c(19, 65)),
        yaxis = list(title = ytitle, range = c(0, NA)),
        hovermode = "closest",
        legend = list(title = list(text = "Group"))
      )
    }
    
    if (length(facet_vars) == 0) {
      return(make_plot(df, paste0("Outcome by Age and Group (", year, ")")))
    }
    
    df_nested <- df %>%
      group_nest(across(all_of(facet_vars))) %>%
      mutate(
        plot = map2(data, row_number(), ~ make_plot(.x, paste(paste(facet_vars, .y, sep = ": "), collapse = " | ")))
      )
    
    subplot(df_nested$plot,
            nrows = ceiling(length(df_nested$plot) / 2),
            margin = 0.06,
            shareX = TRUE,
            shareY = TRUE,
            titleX = TRUE,
            titleY = TRUE)
  })
}

# ---- RUN APP ----
# When sourced from run_all.R, skip the Shiny launch (set SKIP_SHINY = TRUE before sourcing)
if (!isTRUE(SKIP_SHINY)) {
  shinyApp(ui = ui, server = server)
}
