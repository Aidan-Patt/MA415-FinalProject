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
    
    
    ### Education ####
    nav_panel("Education", 
              "Education Statistics",
              selectInput("place", "Places", choices = edu$`Area_description`),
              # take out island and oceanic bay
              
              selectInput("measure",
                          "Measures",
                          choices = c("Percent", "Count")),
              #actionButton("g", "Generate"),
              
              plotOutput("edu_plot")
              ),
    
    #### #####
    
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
  
  ##### Education #####
  edu_select = reactive({edu |> filter(`Area_description` == input$place)})
  
    output$edu_plot = renderPlot({
      if(input$measure == "Percent"){
        edu_select() |> ggplot() +
          aes( x = `Population Percent that Obtained Qualificaton Level`,
               y = `Highest Qualification`,
               colour = Year) +
          geom_point() +
          scale_color_gradient() +
          labs(
            x = "Percentage of Population",
            title = "Education Obtained"
          ) +
          ggthemes::theme_gdocs()
        
      } else{
        output$edu_plot = renderPlot({
          edu_select() |> ggplot() +
            aes( x = `Population Count`,
                 y = `Highest Qualification`,
                 colour = Year) +
            geom_point() +
            scale_color_gradient() +
            labs(
              x = "Population Count",
              title = "Education Obtained"
            ) +
            ggthemes::theme_gdocs()
        })
      }
    })

  

}

# Run the application 
shinyApp(ui = ui, server = server)
