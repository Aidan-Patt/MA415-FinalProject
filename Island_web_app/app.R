# MA 415 FINAL PROJECT WEB APP

# Find out more about building applications with Shiny here:
#    https://shiny.posit.co/


library(shiny)
library(bslib)
library(ggplot2)
library(leaflet)

# source("island_dc.R")


# Define UI for application that draws a histogram
ui <- fluidPage(

  
  navset_pill_list(
    
    #### INTRO ####
    nav_panel("Intro", 
              "North Island of New Zealand Introduction",
              tags$figure(
                class = "centerFigure",
                tags$img(
                  src = "NorthIsland.jpg",
                  width = 600,
                  alt = "Auckland City"
                ),
                tags$figcaption("Image of Auckland City New Zealand by sxbaird, 2017")
              )
              
              
              ),
    
    
    
    
    
    #### MAP #####
    nav_panel("Map",
              "North Island Map",
              
              leafletOutput("map")
              
              ),
    
    
    
    ### Education ###3 
    nav_panel("Education", 
              "Education Statistics"
              
              
              
              
              ),
    
    
    
    nav_panel("Ethnicity", "Ethnicity Data"),
    nav_panel("Expats", "Information on Expats in North Island"),
    nav_panel("Housing", "Examining Housing Data"),
    nav_panel("Income", "How much are people making?"),
    nav_panel("Jobs", "What is the work life"),
    nav_panel("Maori", "The Indigenous People of North Island"),
    nav_panel("Religion", 'What do people believe in?'),
    nav_panel("Smoking", "How prevalent is smoking?"),
    nav_panel("Transportation", "Getting from point A to B"),
    
    
    nav_menu( 
      "Other links", 
      nav_panel("D", "Panel D content"), 
      "----", 
      "Description:", 
      nav_item( 
        a("Shiny", href = "https://shiny.posit.co", target = "_blank") 
      ), 
    ), 
  ), 
  id = "tab" 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #### MAP ####
  cities = data.frame(lng = c(174.763336, 175.269363, 174.7730),
                      lat = c(-36.848461, -37.781528, -41.2924),
                      names =  c("Auckland",  "Hamilton", "Wellington"))

  output$map <-renderLeaflet({
      leaflet() |>
      addTiles() |>
      setView(lng = 175.6024, lat = -38.15,   zoom = 6) %>%
      addMarkers(data = cities)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
