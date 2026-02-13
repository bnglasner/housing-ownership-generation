# =============================================================================
# theme_eig.R
# Reusable ggplot2 theme matching the Datawrapper / EIG chart style
# Source this file, then use theme_eig() in your plots.
# =============================================================================

library(ggplot2)

# ---------------------------------------------------------------------------
# Font setup: Roboto from Google Fonts, with Arial fallback
# ---------------------------------------------------------------------------
eig_font_family <- "Arial"  # default fallback

if (requireNamespace("sysfonts", quietly = TRUE) &&
    requireNamespace("showtext", quietly = TRUE)) {
  library(sysfonts)
  library(showtext)
  tryCatch({
    sysfonts::font_add_google("Roboto", "Roboto")
    showtext::showtext_auto()
    eig_font_family <- "Roboto"
  }, error = function(e) {
    message("theme_eig: Could not load Roboto from Google Fonts; falling back to Arial.")
    eig_font_family <<- "Arial"
  })
} else {
  message("theme_eig: showtext/sysfonts not installed; using Arial as font family.")
}

# ---------------------------------------------------------------------------
# Generation line colors — Datawrapper palette
# ---------------------------------------------------------------------------
# NOTE: The original palette in Generational Comp.R (gen_lookup) uses:
#   Greatest = "#1a654d"
#   Silent   = "#356859"
#   Boomers  = "#da9969"
#   Gen-X    = "#e1ad28"
#   Millennials = "#f3d9b1"
#   Gen-Z    = "#eee5d9"
#   Gen-Alpha   = "#fdf6ec"
#
# The Datawrapper / EIG figure uses a different palette (greens for older
# generations, gold/amber for newer ones). eig_gen_colors below reflects
# that Datawrapper palette.
# ---------------------------------------------------------------------------
eig_gen_colors <- c(
  "Greatest"    = "#1a654d",
  "Silent"      = "#356859",
  "Boomers"     = "#3D8C6B",
  "Gen-X"       = "#D4A843",
  "Millennials" = "#D4A843",
  "Gen-Z"       = "#D4A843",
  "Gen-Alpha"   = "#D4A843"
)

# ---------------------------------------------------------------------------
# Ribbon fill color (use with alpha ~0.4 in geom_ribbon / geom_area)
# ---------------------------------------------------------------------------
eig_ribbon_fill <- "#F2D9A0"

# ---------------------------------------------------------------------------
# theme_eig()
# A complete ggplot2 theme for EIG-style charts.
#
# Recommended geom defaults (not enforced by the theme):
#   geom_line(linewidth = 0.8)   # or up to 1.0
# ---------------------------------------------------------------------------
theme_eig <- function() {
  theme_minimal(base_family = eig_font_family, base_size = 12) %+replace%
    theme(
      # ----- Backgrounds & borders -----
      plot.background  = element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = element_rect(fill = "#FFFFFF", colour = NA),
      panel.border     = element_blank(),

      # ----- Grid lines -----
      panel.grid.major.y = element_line(colour = "#E0E0E0", linetype = "solid",
                                        linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),

      # ----- Axes -----
      axis.line.x  = element_line(colour = "#CCCCCC", linewidth = 0.5),
      axis.line.y  = element_blank(),
      axis.ticks.x = element_line(colour = "#CCCCCC", linewidth = 0.3),
      axis.ticks.y = element_blank(),

      # ----- Axis text -----
      axis.text = element_text(family = eig_font_family, size = 10,
                               colour = "#6B6B6B"),

      # ----- Axis titles (removed) -----
      axis.title = element_blank(),

      # ----- Title, subtitle, caption -----
      plot.title = element_text(family = eig_font_family, face = "bold",
                                size = 16, colour = "#2A6B4B", hjust = 0),
      plot.subtitle = element_text(family = eig_font_family, face = "plain",
                                   size = 12, colour = "#6B6B6B", hjust = 0),
      plot.caption = element_text(family = eig_font_family, face = "plain",
                                  size = 8, colour = "#888888", hjust = 0),

      plot.title.position   = "plot",
      plot.caption.position = "plot",

      # ----- Legend -----
      legend.position = "none",

      # ----- Margins -----
      # Extra right margin leaves room for direct endpoint labels
      plot.margin = margin(t = 15, r = 80, b = 10, l = 10)
    )
}
