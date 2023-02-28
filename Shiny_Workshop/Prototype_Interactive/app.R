pacman::p_load(shiny, sf, tidyverse)

sgpools <- read_csv("data/aspatial/SGPools_svy21.csv")

sgpools_sf <- st_as_sf(sgpools,
                       coords = c("XCOORD",
                                  "YCOORD"),
                       crs = 3414)

ui <- fluidPage(
  titlePanel("Interactive Proportional Symbol Map"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      tmapOutput("mapPlot")
    )
  )
)

server <- function(input, output) {
  output$mapPlot <- renderTmap({
    tm_shape(sgpools_sf) +
      tm_bubbles(col = "OUTLET TYPE",
                 size = "Gp1Gp2 Winnings",
                 border.col = "black",
                 border.lwd = 0.5) +
      tm_view(set.zoom.limits = c(11, 16))
  })
}

shinyApp(ui = ui, server = server)