library(shiny)
library(tidyverse)
library(lubridate)

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

  gfcf <- read_csv("gfcf.csv")

  output$asset <- renderUI({
    selectInput("asset",
                "Asset Type",
                choices = unique(gfcf$Asset),
                multiple = TRUE,
                selected = "All assets"
    )
  })

  output$geog_name <- renderUI({
    selectInput("geog_name",
                "Geography Name",
                choices = unique(gfcf$geog_name),
                multiple = TRUE,
                selected = "Greater Manchester"
    )
  })

  output$industry <- renderUI({
    selectInput("industry",
                "Industry",
                choices = unique(gfcf$`SIC07 industry name`),
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
                value = c(min, max)
    )
  })


  output$plot <- renderPlot({
    # req(input$sub_region_1, input$sub_region_2, input$type, input$date)
    ggplot2::ggplot(gfcf %>%
                      dplyr::filter(date >= input$date[1],
                                    date <= input$date[2],
                                    `SIC07 industry name` %in% input$industry,
                                    geog_name %in% input$geog_name,
                                    Asset %in% input$asset),

                    ggplot2::aes(x = date, y = value, colour = Asset, linetype = `SIC07 industry name`)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::facet_wrap(~ geog_name) +
      ggplot2::labs(title = "GFCF",
                    subtitle = "Experimental statistics",
                    x = "",
                    y = ""
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

