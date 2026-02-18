library(shiny)
library(tidyverse)
library(lubridate)
library(showtext)

# Load Jost from Google Fonts
font_add_google("Jost", "Jost")
font_add_google("Jost", "Jost Medium", regular.wt = 500)
showtext_auto()
showtext_opts(dpi = 96)

# --- Configuration ---
PARTY_COLORS <- c(
  "PiS" = "blue",
  "KO" = "orange",
  "Polska 2050" = "goldenrod",
  "PSL" = "darkgreen",
  "Konfederacja" = "midnightblue",
  "KKP" = "brown",
  "Lewica" = "red",
  "Razem" = "purple",
  "Other" = "gray50"
)

PARTY_ORDER <- c(
  "PiS",
  "KO",
  "Polska 2050",
  "PSL",
  "Lewica",
  "Razem",
  "Konfederacja",
  "KKP"
)

theme_plots <- function(base_size = 16, base_family = "Jost") {
  theme_bw(base_size, base_family) +
    theme(
      panel.background = element_rect(fill = "#ffffff", colour = NA),
      panel.border = element_rect(
        color = "grey50",
        fill = NA,
        linewidth = 0.15
      ),
      panel.spacing = unit(1, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25, colour = "grey90"),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_text(size = 18),
      axis.title.x = element_text(size = 18, margin = margin(t = 10)),
      axis.title.y = element_text(
        size = 18,
        hjust = 1,
        margin = margin(r = 10)
      ),
      legend.position = "none",
      strip.background = element_rect(fill = "white", colour = NA),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )
}

my_date_format <- function() {
  function(x) {
    m <- format(x, "%b")
    y <- format(x, "\n%Y")
    ifelse(duplicated(y), m, paste(m, y))
  }
}

# --- Load pre-computed data ---
trend_lines <- readRDS("trend_lines.rds")
date_summaries <- readRDS("date_summaries.rds") %>%
  mutate(party = factor(party, levels = PARTY_ORDER)) %>%
  filter(!is.na(party))
point_dta <- readRDS("point_dta.rds") %>%
  mutate(midDate_num = as.numeric(midDate))

available_dates <- sort(unique(date_summaries$date))

# --- UI ---
ui <- fluidPage(
  tags$head(tags$style(HTML(
    "
    @import url('https://fonts.googleapis.com/css2?family=Jost:wght@400;500&display=swap');
    body { font-family: 'Jost', sans-serif; }
    .fixed-container {
      max-width: 900px;
      margin: 0 auto;
    }
    .popup-container {
      min-height: 320px;
      margin-top: 20px;
      display: flex;
      justify-content: center;
    }
    .popup-placeholder {
      text-align: center;
      color: #999;
      padding-top: 40px;
    }
    .popup-grid {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 0 32px;
    }
    .popup-entry {
      display: flex;
      align-items: baseline;
      padding: 6px 0;
      border-bottom: 1px solid #eee;
    }
    .popup-party {
      min-width: 130px;
    }
    .popup-est {
      min-width: 50px;
      text-align: right;
    }
    .popup-ci {
      margin-left: 12px;
      color: #666;
      white-space: nowrap;
    }
    .color-dot {
      display: inline-block; width: 10px; height: 10px;
      border-radius: 50%; margin-right: 6px; vertical-align: middle;
    }
    .point-tooltip {
      position: absolute;
      background: white;
      border: 1px solid #ccc;
      border-radius: 6px;
      padding: 8px 12px;
      font-size: 0.9em;
      pointer-events: none;
      box-shadow: 0 2px 6px rgba(0,0,0,0.15);
      z-index: 100;
      white-space: nowrap;
    }
  "
  ))),
  div(
    class = "fixed-container",
    titlePanel("Vote intention polling in Poland"),
    div(
      style = "position: relative;",
      plotOutput(
        "trend_plot",
        click = "plot_click",
        hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
        width = "900px",
        height = "600px"
      ),
      uiOutput("point_tooltip")
    ),
    div(class = "popup-container", uiOutput("click_info"))
  )
)

# --- Helper ---
build_popup_html <- function(snapped_date) {
  day_data <- date_summaries %>%
    filter(date == snapped_date) %>%
    arrange(desc(median_pct))

  if (nrow(day_data) == 0) {
    return(NULL)
  }

  entries <- day_data %>%
    rowwise() %>%
    mutate(
      color_hex = PARTY_COLORS[as.character(party)],
      entry_html = paste0(
        "<div class='popup-entry'>",
        "<span class='popup-party'><span class='color-dot' style='background:",
        color_hex,
        ";'></span>",
        party,
        "</span>",
        "<span class='popup-est'>",
        median_pct,
        "%</span>",
        "<span class='popup-ci'>(",
        lower_pct,
        "% \u2013 ",
        upper_pct,
        "%)</span>",
        "</div>"
      )
    ) %>%
    pull(entry_html)

  paste0(
    "<div style='display:inline-block; padding:16px; border:1px solid #ddd; ",
    "border-radius:8px;'>",
    "<h4 style='margin-top:0;'>",
    format(snapped_date, "%e %B %Y"),
    "</h4>",
    "<div class='popup-grid'>",
    paste(entries, collapse = ""),
    "</div>",
    "<p style='margin-bottom:0; margin-top:12px; color:#999; font-size:0.9em;'>",
    "Click anywhere on the plot to view vote shares for other dates. ",
    "Hover over any of the points to see particular polling house estimates.</p>",
    "</div>"
  )
}

# --- Server ---
server <- function(input, output, session) {
  selected_date <- reactiveVal(max(available_dates))

  observeEvent(input$plot_click, {
    clicked_date <- as.Date(input$plot_click$x, origin = "1970-01-01")
    idx <- which.min(abs(available_dates - clicked_date))
    selected_date(available_dates[idx])
  })

  output$trend_plot <- renderPlot(
    {
      showtext_opts(dpi = 96)
      ggplot() +
        geom_point(
          data = point_dta,
          aes(x = midDate, y = est, colour = party),
          size = 2,
          alpha = 0.3,
          show.legend = FALSE
        ) +
        geom_line(
          data = trend_lines,
          aes(x = date, y = median_epred, colour = party),
          linewidth = 1
        ) +
        scale_color_manual(values = PARTY_COLORS) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_x_date(date_breaks = "1 month", labels = my_date_format()) +
        coord_cartesian(ylim = c(0, NA)) +
        labs(y = "", x = "", color = "") +
        theme_plots()
    },
    res = 96,
    execOnResize = TRUE
  )

  output$point_tooltip <- renderUI({
    hover <- input$plot_hover
    if (is.null(hover)) {
      return(NULL)
    }

    point <- nearPoints(
      point_dta,
      hover,
      xvar = "midDate_num",
      yvar = "est",
      threshold = 10,
      maxpoints = 1
    )

    if (nrow(point) == 0) {
      return(NULL)
    }

    color <- PARTY_COLORS[as.character(point$party[1])]
    left_px <- hover$coords_css$x
    top_px <- hover$coords_css$y

    style <- paste0(
      "left:",
      left_px + 12,
      "px; top:",
      top_px + 12,
      "px;"
    )

    div(
      class = "point-tooltip",
      style = style,
      HTML(paste0(
        "<span class='color-dot' style='background:",
        color,
        ";'></span>",
        "<b>",
        point$party[1],
        "</b> ",
        round(point$est[1] * 100, 1),
        "%<br>",
        as.character(point$org[1]),
        " &middot; ",
        format(point$midDate[1], "%e %B %Y")
      ))
    )
  })

  output$click_info <- renderUI({
    HTML(build_popup_html(selected_date()))
  })
}

shinyApp(ui, server)
