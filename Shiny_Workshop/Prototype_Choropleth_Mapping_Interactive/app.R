pacman::p_load(shiny, sf, tidyverse, tmap, DT)

mpsz <- st_read(dsn = "data/geospatial",
                layer = "MP14_SUBZONE_WEB_PL")

popdata <- read_csv("data/aspatial/respopagesextod2011to2020.csv")

print(popdata)

pop <- popdata %>%
  filter(Time == 2020) %>%
  group_by(PA, SZ, AG) %>%
  summarise(POP = sum(Pop)) %>%
  ungroup()%>%
  pivot_wider(names_from=AG, 
              values_from=POP) %>%
  mutate(YOUNG = `0_to_4` + `5_to_9` + `10_to_14` + `15_to_19` + `20_to_24`) %>%
  mutate(`ECONOMY ACTIVE` = rowSums(.[9:13])+
           rowSums(.[15:17]))%>%
  mutate(`AGED`=rowSums(.[18:22])) %>%
  mutate(`TOTAL`=rowSums(.[5:22])) %>%  
  mutate(`DEPENDENCY` = (`YOUNG` + `AGED`)
         /`ECONOMY ACTIVE`) %>%
  mutate_at(.vars = vars(PA, SZ),
            .funs = funs(toupper)) %>%
  select(`PA`, `SZ`, `YOUNG`, 
         `ECONOMY ACTIVE`, `AGED`, 
         `TOTAL`, `DEPENDENCY`) %>%
  filter(`ECONOMY ACTIVE` > 0)

mpsz_demog <- left_join(mpsz,
                        pop,
                        by = c("SUBZONE_N" = "SZ"))


ui <- fluidPage(
  titlePanel("Choropleth Mapping System"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "variable",
        label = "Mapping Variable:",
        choices = c("Dependency" = "DEPENDENCY",
                    "Young" = "YOUNG",
                    "Economy Active" = "ECONOMY ACTIVE",
                    "Aged" = "AGED"),
        selected = "DEPENDENCY"
      ),
      
      selectInput(
        inputId = "classification",
        label = "Classification:",
        choices = c("fixed" = "fixed",
                    "sd" = "sd",
                    "equal" = "equal",
                    "pretty" = "pretty",
                    "quantile" = "quantile",
                    "kmeans" = "kmeans",
                    "hclust" = "hclust",
                    "bclust" = "bclust",
                    "fisher" = "fisher",
                    "jenks" = "jenks"),
        selected = "pretty"
      ),
      
      sliderInput(
        inputId = "classes",
        label = "Number of Classes:",
        min = 6,
        max = 12,
        value = 8
      ),
      
      selectInput(
        inputId = "colour",
        label = "Colour Scheme:",
        choices = c("Blues" = "Blues",
                    "Reds" = "Reds",
                    "Greens" = "Greens",
                    "Yellow-Orange-Red" = "YlOrRd",
                    "Yellow-Orange-Brown" = "YlOrBr",
                    "Yellow-Green" = "YlGn",
                    "Orange-Red" = "OrRd"),
        selected = "YlOrRd"
      ),
      
      checkboxInput(
        inputId = "showData",
        label = "Show Data Table",
        value = TRUE
      )
    ),
    mainPanel(
      tmapOutput("mapPlot",
                 width = "100%",
                 height = 400),
    )
  )
)

server <- function(input, output) {
  
  output$mapPlot <- renderTmap({
    tmap_options(check.and.fix = TRUE) +
      tm_shape(mpsz_demog) +
      tm_fill(input$variable,
              n = input$classes,
              style = input$classification,
              palette = input$colour) +
      tm_borders(lwd = 0.1, alpha = 1) +
      tm_view(set.zoom.limits = c(11, 14))
  })
  output$aTable <- DT::renderDataTable({
    if(input$showData){
      DT::datatable(data = dataset() %>%
                      select(1:4),
                    options = list(pageLength = 10),
                    rownames = FALSE)
    }
  })
}

shinyApp(ui = ui, server = server)