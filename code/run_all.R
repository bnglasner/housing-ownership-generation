###############################################
# run_all.R
# Master script: full pipeline from IPUMS download to exported figures
#
# Usage:
#   source("code/run_all.R")          # analysis + figures only (default)
#   REDOWNLOAD_DATA <- TRUE           # set before sourcing to re-pull from IPUMS
#   LAUNCH_SHINY    <- TRUE           # set before sourcing to open the Shiny dashboard
#
# Pipeline:
#   Step 1 (optional) — 00a_ipums_api_extract.R : download CPS ASEC data from IPUMS
#   Step 2            — Generational Comp.R      : clean, summarize, export tables + figures
#   Step 3 (optional) — Launch Shiny dashboard
#
# Requirements:
#   - IPUMS_API_KEY env var (only if REDOWNLOAD_DATA = TRUE)
#   - CPS data already in data/ (if skipping download)
#   - BEA_deflator.xlsx in data/
###############################################

# =========================================
# 0) Pipeline flags
# =========================================
# These can be set before sourcing this script to control behavior.
# If not already set, defaults apply.

if (!exists("REDOWNLOAD_DATA")) REDOWNLOAD_DATA <- FALSE
if (!exists("LAUNCH_SHINY"))    LAUNCH_SHINY    <- FALSE

# =========================================
# 1) Locate project root
# =========================================
# Same strategy used by all scripts in this project.

has_project_markers <- function(path) {
  if (!dir.exists(path)) return(FALSE)
  readme_ok <- file.exists(file.path(path, "README.md"))
  data_ok   <- dir.exists(file.path(path, "data")) || dir.exists(file.path(path, "Data"))
  readme_ok && data_ok
}

path_project <- Sys.getenv("HOMEOWNERSHIP_BY_GEN_PROJECT_ROOT", unset = "")

if (!nzchar(path_project)) {
  this_file <- tryCatch(
    normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
    error = function(e) NA_character_
  )
  if (!is.na(this_file) && nzchar(this_file)) {
    candidate <- dirname(dirname(this_file))
    if (has_project_markers(candidate)) path_project <- candidate
  }
}

if (!nzchar(path_project)) {
  current <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in 1:15) {
    if (has_project_markers(current)) { path_project <- current; break }
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
path_code    <- file.path(path_project, "code")

message("\n###############################################")
message("# run_all.R — Generational Homeownership Pipeline")
message("###############################################")
message("Project root:    ", path_project)
message("Redownload data: ", REDOWNLOAD_DATA)
message("Launch Shiny:    ", LAUNCH_SHINY)
message("###############################################\n")

# =========================================
# 2) Step 1: IPUMS data download (optional)
# =========================================
if (isTRUE(REDOWNLOAD_DATA)) {
  message("=== Step 1/3: Downloading CPS ASEC data from IPUMS ===\n")
  source(file.path(path_code, "00a_ipums_api_extract.R"), local = new.env())
  message("\n=== Step 1 complete ===\n")
} else {
  message("=== Step 1/3: Skipping data download (REDOWNLOAD_DATA = FALSE) ===")
  message("  To re-pull from IPUMS, set REDOWNLOAD_DATA <- TRUE before sourcing run_all.R\n")
}

# =========================================
# 3) Step 2: Analysis + figures
# =========================================
message("=== Step 2/3: Running analysis (Generational Comp.R) ===\n")

# Save flags before sourcing — Generational Comp.R calls rm(list = ls())
.launch_shiny <- LAUNCH_SHINY

# Tell Generational Comp.R not to launch the Shiny app at the end
SKIP_SHINY <- TRUE
source(file.path(path_code, "Generational Comp.R"))

# Restore flags
LAUNCH_SHINY <- .launch_shiny
rm(.launch_shiny)

message("\n=== Step 2 complete ===")
message("  Outputs written to: ", file.path(path_project, "output"))
message("    - Ownership_rate_by_generation.xlsx")
message("    - homeownership_by_generation.png\n")

# =========================================
# 4) Step 3: Shiny dashboard (optional)
# =========================================
if (isTRUE(LAUNCH_SHINY)) {
  message("=== Step 3/3: Launching Shiny dashboard ===\n")
  shinyApp(ui = ui, server = server)
} else {
  message("=== Step 3/3: Skipping Shiny dashboard (LAUNCH_SHINY = FALSE) ===")
  message("  To launch interactively, set LAUNCH_SHINY <- TRUE before sourcing run_all.R")
  message("  Or run Generational Comp.R directly.\n")
}

message("###############################################")
message("# Pipeline complete")
message("###############################################\n")
