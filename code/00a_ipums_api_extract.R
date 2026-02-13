###############################################
# 00_ipums_api_extract_homeownership_generation.R
# How have homeownership rates changed across generations by age?
#
# Purpose:
#   Define and download IPUMS CPS ASEC extract via API
#   This script creates an extract with ONLY the variables
#   needed for the generational homeownership analysis.
#
# Key design goals:
#   - Avoid hard-coded paths (robust project-root discovery)
#   - Metadata-driven ASEC sample list (robust to naming changes)
#   - Minimal variable list that matches Generational Comp.R
#   - Save extract metadata for reproducibility
#   - Make downstream scripts auto-load the latest cps_*.xml
#
# Requirements:
#   - ipumsr
#   - Environment variable IPUMS_API_KEY
###############################################

rm(list = ls())
options(scipen = 999)

suppressPackageStartupMessages({
  library(ipumsr)
  library(dplyr)
  library(stringr)
})

# =========================
# 0) Locate project root
# =========================
# Resolution order:
#   1. Environment variable HOMEOWNERSHIP_BY_GEN_PROJECT_ROOT (preferred)
#   2. This script's own location (code/ is one level below project root)
#   3. Walk upward from getwd() looking for project-specific markers
#
# Markers for this project:
#   - README.md exists
#   - data/ exists (or Data/)

project_root_from_env <- function() {
  path_project <- Sys.getenv("HOMEOWNERSHIP_BY_GEN_PROJECT_ROOT", unset = "")
  if (nzchar(path_project)) return(path_project)
  ""
}

has_project_markers <- function(path) {
  if (!dir.exists(path)) return(FALSE)
  readme_ok <- file.exists(file.path(path, "README.md"))
  data_ok   <- dir.exists(file.path(path, "data")) || dir.exists(file.path(path, "Data"))
  readme_ok && data_ok
}

# Strategy 1: Environment variable
path_project <- project_root_from_env()

# Strategy 2: Derive from this script's own file path
if (!nzchar(path_project)) {
  this_file <- tryCatch(
    normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
    error = function(e) NA_character_
  )
  if (!is.na(this_file) && nzchar(this_file)) {
    candidate <- dirname(dirname(this_file)) # Code/xx.R -> Code/ -> project root
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

# Validate project root
if (!nzchar(path_project) || is.na(path_project)) {
  stop(
    "Could not locate project root.\n\n",
    "Fix by either:\n",
    "  (1) Set env var: Sys.setenv(HOMEOWNERSHIP_BY_GEN_PROJECT_ROOT = '/path/to/project')\n",
    "  (2) Run from within the project directory (must contain README.md and data/)\n",
    call. = FALSE
  )
}

if (!dir.exists(path_project)) {
  stop("Project root does not exist on disk: ", path_project, call. = FALSE)
}

path_project <- normalizePath(path_project, winslash = "/", mustWork = TRUE)
setwd(path_project)
message("+ Project root: ", path_project)

# Standardize to lowercase folders for outputs in this project
path_data   <- file.path(path_project, "data")
path_output <- file.path(path_project, "output")
dir.create(path_data, showWarnings = FALSE, recursive = TRUE)
dir.create(path_output, showWarnings = FALSE, recursive = TRUE)

message("+ Data folder:   ", path_data)
message("+ Output folder: ", path_output)

# =========================
# 1) Set IPUMS API Key
# =========================
api_key <- Sys.getenv("IPUMS_API_KEY", unset = "")
if (!nzchar(api_key)) {
  stop(
    "\n===========================================\n",
    "IPUMS API key not found!\n\n",
    "Please either:\n",
    "  1. Add to .Renviron file: IPUMS_API_KEY=your_key\n",
    "  2. Set in R session: Sys.setenv(IPUMS_API_KEY = 'your_key')\n\n",
    "Get your API key from: https://account.ipums.org/api_keys\n",
    "===========================================\n",
    call. = FALSE
  )
}
set_ipums_api_key(api_key)
message("✓ IPUMS API key set successfully")

# =========================
# 2) Define sample range
# =========================
# Analysis period: CPS ASEC / March supplement
start_year <- 1976
end_year   <- 2025

message("\n===========================================")
message("IPUMS CPS ASEC Extract Definition")
message("===========================================")
message("Time period: ", start_year, " to ", end_year)
message("Sample type: CPS ASEC (Annual Social and Economic Supplement)")
message("===========================================\n")

# =========================
# 3) Define variables (MINIMAL SET)
# =========================
# Only variables used for the generational homeownership analysis
variable_list <- c(
  # Identifiers / structure
  "YEAR",
  
  # Weights
  "ASECWTH",
  
  # Demographics
  "AGE",
  "SEX",
  "RACE",
  "MARST",
  
  # Household / housing
  "RELATE",
  "OWNERSHP",
  
  # Geography
  "METRO",
  
  # Income
  "INCWAGE"
)

message("Variables requested: ", length(variable_list))
message("  ", paste(variable_list, collapse = ", "))

# =========================
# 4) Create ASEC sample list (metadata-driven + fallback)
# =========================
si <- get_sample_info("cps")

# Defensive column handling (ipumsr versions can differ)
nm_col   <- if ("name" %in% names(si)) "name" else names(si)[1]
desc_col <- if ("description" %in% names(si)) "description" else NA_character_

si <- si %>%
  rename(sample = all_of(nm_col)) %>%
  mutate(
    year = suppressWarnings(as.integer(str_match(sample, "^cps(\\d{4})_")[, 2]))
  ) %>%
  filter(!is.na(year), year >= start_year, year <= end_year)

if (!is.na(desc_col) && desc_col %in% names(si)) {
  sample_list <- si %>%
    filter(str_detect(.data[[desc_col]], regex("\\bASEC\\b", ignore_case = TRUE))) %>%
    arrange(year, sample) %>%
    pull(sample)
} else {
  # Fallback: many CPS ASEC samples follow cpsYYYY_03s
  sample_list <- si %>%
    filter(str_detect(sample, "_03s$")) %>%
    arrange(year, sample) %>%
    pull(sample)
}

sample_list <- unique(sample_list)

if (length(sample_list) == 0) {
  stop("No ASEC samples found in IPUMS metadata for the specified year range.", call. = FALSE)
}

message("Total ASEC samples requested: ", length(sample_list))
message("  First sample: ", sample_list[[1]])
message("  Last sample:  ", sample_list[[length(sample_list)]])

# =========================
# 5) Define + submit extract
# =========================
message("\nDefining extract...")

extract_def <- ipumsr::define_extract_cps(
  description = paste0(
    "Generational Homeownership Analysis: CPS ASEC ",
    start_year, " to ", end_year,
    " | vars: ", paste(variable_list, collapse = ", ")
  ),
  samples        = sample_list,
  variables      = variable_list,
  data_format    = "fixed_width",
  data_structure = "rectangular"
)


message("Submitting extract to IPUMS...")
submitted_extract <- submit_extract(extract_def)

# Extract number (ipumsr returns different structures across versions; handle both)
extract_number <- NA_character_
if (!is.null(submitted_extract$number)) {
  extract_number <- as.character(submitted_extract$number)
} else if (!is.null(submitted_extract$id)) {
  extract_number <- as.character(submitted_extract$id)
}

message("\n===========================================")
message("EXTRACT SUBMITTED")
message("===========================================")
if (!is.na(extract_number)) message("Extract number/id: ", extract_number)
if (!is.null(submitted_extract$status)) message("Status: ", submitted_extract$status)
message("===========================================\n")

# =========================
# 6) Wait + download + unzip
# =========================
message("Waiting for IPUMS to process extract (this may take a while)...")
extract_ready <- wait_for_extract(submitted_extract)

message("Downloading extract files to: ", path_data)
dl_path <- download_extract(extract_ready, download_dir = path_data)

if (!file.exists(dl_path)) {
  stop("Download did not return a valid file path. Got: ", dl_path, call. = FALSE)
}

# download_extract() may return a .zip or a .dat.gz depending on ipumsr version.
# Only unzip if it's actually a zip file.
if (grepl("\\.zip$", dl_path, ignore.case = TRUE)) {
  message("Unzipping: ", basename(dl_path))
  unzip(dl_path, exdir = path_data)
} else {
  message("Downloaded: ", basename(dl_path), " (no unzip needed)")
}

# =========================
# 7) Helper: find the most recent cps_*.xml in ./data
# =========================
find_latest_cps_ddi <- function(data_dir = path_data) {
  xmls <- list.files(data_dir, pattern = "^cps_.*\\.xml$", full.names = TRUE, ignore.case = TRUE)
  if (length(xmls) == 0) stop("No cps_*.xml found in: ", data_dir, call. = FALSE)
  xmls[which.max(file.info(xmls)$mtime)]
}

latest_xml <- find_latest_cps_ddi(path_data)
message("✓ Latest CPS DDI XML in data/: ", basename(latest_xml))

# List downloaded CPS files (useful quick audit)
cps_files <- list.files(path_data, pattern = "^cps_.*\\.(xml|dat|gz|txt)$", full.names = FALSE, ignore.case = TRUE)
if (length(cps_files) > 0) {
  message("\nFiles in data/ (CPS-related):")
  for (f in cps_files) message("  ", f)
} else {
  message("\n(No cps_* files detected in data/. Check zip contents if needed.)")
}

# =========================
# 8) Save extract info (reproducibility)
# =========================
extract_info_file <- file.path(path_output, "ipums_extract_info.rds")

saveRDS(
  list(
    extract_number = extract_number,
    submitted_at   = Sys.time(),
    start_year     = start_year,
    end_year       = end_year,
    n_samples      = length(sample_list),
    n_variables    = length(variable_list),
    variables      = variable_list,
    samples        = sample_list,
    latest_ddi_xml = latest_xml,
    project_root   = path_project,
    data_dir       = path_data
  ),
  extract_info_file
)

message("\n===========================================")
message("DONE")
message("===========================================")
message("Extract info saved to: ", extract_info_file)
message("\nIn Generational Comp.R, you can now load the latest pull with:")
message("  ddi <- read_ipums_ddi(find_latest_cps_ddi('data'))")
message("  cps <- read_ipums_micro(ddi)")
message("===========================================\n")
