library(shiny)
library(tidyverse)

ui <- fluidPage(

  titlePanel("Regional GFCF Data Exploration Tool"),

  sidebarLayout(
    sidebarPanel(width = 2,
                 uiOutput("asset"),
                 uiOutput("geog_name"),
                 uiOutput("industry"),
                 uiOutput("date"),
                 p("A quick and dirty app by @ChristianSpence")
    ),

    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {

  gfcf <- readRDS("data/rgfcf.rds")

  output$asset <- renderUI({
    selectInput("asset",
                "Asset Type",
                choices = unique(gfcf$variable_name),
                multiple = TRUE,
                selected = "All assets"
    )
  })

  output$geog_name <- renderUI({
    selectInput("geog_name",
                "Geography",
                choices = unique(gfcf$geography_name),
                multiple = TRUE,
                selected = "Greater Manchester"
    )
  })

  output$industry <- renderUI({
    selectInput("industry",
                "Industry",
                choices = unique(gfcf$industry_name),
                selected = "Total GFCF",
                multiple = TRUE
    )
  })

  output$date <- renderUI({
    min = min(gfcf$date)
    max = max(gfcf$date)
    sliderInput("date",
                "Date range",
                min = min,
                max = max,
                value = c(min, max),
                step = 1,
                sep = ""
    )
  })

  output$plot <- renderPlot({
    req(input$asset, input$geog_name,
        input$industry, input$date)
    ggplot2::ggplot(gfcf |>
                      dplyr::filter(date >= input$date[1],
                                    date <= input$date[2],
                                    industry_name %in% input$industry,
                                    geography_name %in% input$geog_name,
                                    variable_name %in% input$asset),
                    ggplot2::aes(x = date, y = value, colour = variable_name, linetype = industry_name)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::facet_wrap(~ geography_name) +
      ggplot2::labs(title = "Regional Gross Fixed Capital Formation",
                    subtitle = "Caution: experimental statistics",
                    x = "",
                    y = "£m, NSA, Current Prices",
                    caption = "App by @ChristianSpence"
      ) +
      ggplot2::theme(
        panel.background   = ggplot2::element_blank(),
        panel.grid         = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(linetype = "dotted"),
        legend.position    = "top",
        axis.line.y.right  = NULL,
        axis.line          = ggplot2::element_line(),
        strip.background   = ggplot2::element_blank()
      )
  }, height = 600)
}

shinyApp(ui = ui, server = server)

