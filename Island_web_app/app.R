# MA 415 FINAL PROJECT WEB APP

# Find out more about building applications with Shiny here:
#    https://shiny.posit.co/


library(shiny)
library(bslib)
library(ggplot2)
library(leaflet)

source("island_dc.R")


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
              
              plotOutput("edu_plot")
              ),
    
    #### Ethnicity #####
    
    nav_panel("Ethnicity",
              "Ethnicity Data",
              selectInput("eth_place", "Places",
                          choices = ethnicity$`Area_description`),
              
              plotOutput("eth_plot")
              ),
    
    #### Expat #####
    nav_panel("Expats", 
              "Information on Expats in North Island",
              selectInput("exp_place", "Places",
                          choices = expat$`Area_description`),
              
              selectInput("exp_year", "Census Year",
                          choices = expat$`Year`),
              
              plotOutput("exp_plot")
              
              ),
    
    
    #### Housing #####
    nav_panel("Housing",
              "Examining Housing Data",
              selectInput("house_place", "Places", 
                          choices = house$`Area_description`),
              
              plotOutput("house_plot")
              
              
              ),
    
    
    ### Income ##
    nav_panel("Income", 
              "How much are people making?",
              
              selectInput("inc_place", "Places",
                          choices = income$`Area_description`),
              
              plotOutput("inc_plot")
              ),
    #### Job ####
    nav_panel("Jobs", 
              "What is the work life",
              
              selectInput("job_place", "Places",
                          choices = job$`Area_description`),
              
              plotOutput("job_plot")
              ),
    
    ##### Maori #####
    nav_panel("Maori", 
              "The Indigenous People of North Island",
              selectInput("maori_place", "Places",
                          choices = maori$`Area_description`),
              
              plotOutput("maori_plot")
              ),
  
    #### Religion ####
    nav_panel("Religion",
              'What do people believe in?',
              
              selectInput("rel_place", "Places",
                          choices = religion$`Area_description`),
              
              plotOutput("rel_plot")
              ),
    
    
    ##### Smoking ####
    nav_panel("Smoking", 
              "How prevalent is smoking?",
              
              selectInput("smoking_place", "Places",
                          choices = smoking$`Area_description`),
              
              plotOutput("smoking_plot")
              
              ),
    
    ##### Transportation #####
    nav_panel("Transportation",
              "Getting from point A to B",
              
              selectInput("tran_place", "Places",
                          choices = transport$Area_description),
              
              plotOutput("tran_plot")

              ),
    
    
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
  
  ##### EDUCATION #####
  edu_select = reactive({edu |> 
        filter(`Area_description` == input$place) }) 
  
  output$edu_plot = renderPlot({
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
    })
  
  ###### ETHNICITY ########
  eth_select = reactive({ ethnicity |>
      filter(`Area_description` == input$eth_place)})
  
  output$eth_plot = renderPlot({
    eth_select() |> ggplot() +
      aes(
        x = Year,
        y = `Percent of Population`,
        colour = `Ethnic Group`
      ) +
      geom_line() +
      scale_color_manual(
        values = c(Asian = "#E41A1C",
                   European = "#040DC7",
                   Maori = "#B765C8",
                   `Middle Eastern/Latin American/African` = "#05BE39",
                   `Other ethnicity` = "#C9992C",
                   `Pacific peoples` = "#05011E")
      ) +
      labs(
        x = "Year",
        y = "Percentage",
        title = "How Ethnic Percentages Have Changed Over Time"
      ) +
      ggthemes::theme_stata()
      
  })
  
  
  ###### EXPAT #####
  exp_select = reactive({ expat |>
      filter(`Area_description` == input$exp_place &
               `Year` == input$exp_year)
  })
  
  output$exp_plot = renderPlot({
    exp_select() |>
      ggplot() +
      aes(
        x = Percentage,
        y = reorder(`Years since arrival`,`Years_since_arrival_in_New_Zealand_code`),
        fill = `Years since arrival`
      ) +
      geom_bar(stat = "summary", fun = "sum") +
      scale_fill_manual(
        values = c(`Less than 1 year` = "#D6EABA",
                   `1 year` = "#BEE9B1",
                   `2 years` = "#92D190",
                   `3 years` = "#6FCEA8",
                   `4 years` = "#62CDDE",
                   `5-9 years` = "#34A5D8",
                   `10-19 years` = "#126EAC",
                   `20 years or more` = "#053D7E") ) +
      labs(x = "Amount in Percentages",
           title = "Expat Information") +
      ggthemes::theme_wsj() +
      theme(
        axis.title.x = element_text(size = 17L)
      )
  })
  
  ###### HOUSING #####
  house_select = reactive({ house |>
      filter(`Area_description` == input$house_place) })
  
  output$house_plot = renderPlot({
    house_select() |>
      ggplot() +
      aes(
        x = Year,
        y = `Number of Households`,
        colour = `Tenure Type`
      ) +
      geom_smooth(se = TRUE) +
      scale_color_manual(
        values = c(`Dwelling held in a family trust` = "#F33E31",
                   `Dwelling not owned and not held in a family trust` = "#18A20B",
                   `Dwelling owned or partly owned` = "#0096D6",
                   `Not elsewhere included` = "#B7197B")
      ) +
      labs(
        x = "Time in Years",
        y = "Number of Households",
        title = "Account of Household Tenure Desccription Over Time"
      ) +
      ggthemes::theme_stata() +
      theme(legend.title = element_text(face = "bold"))
  })
  
  ##### INCOME ######
  inc_select = reactive({ income |>
      filter(`Area_description` == input$inc_place) })
  
  output$inc_plot = renderPlot({
    options(scipen=10000)
    inc_select() |>
    ggplot() +
      aes(x = Census, y = reorder(`Income`, `Total_personal_income_code`)) +
      geom_bar(stat = "summary", fun = "sum", fill = "#11A511") +
      labs(
        x = "Number of People",
        y = "Income Brackets",
        title = "Income Distribution"
      ) +
      ggthemes::theme_economist() +
      theme(
        plot.title = element_text(size = 32L,
                                  face = "bold",
                                  hjust = 0.5),
        axis.title.y = element_text(size = 25L),
        axis.title.x = element_text(size = 25L)
      )
  })
  
  ##### JOB #####
  job_select = reactive({job|>
      filter(`Area_description` == input$job_place) })
  
  output$job_plot = renderPlot({
    job_select() |>
      ggplot() +
      aes(x = Year, y = Census, colour = `Job Status Description`) +
      geom_smooth(se = TRUE) +
      scale_color_brewer(palette = "Set1", direction = 1) +
      labs(
        x = "Years",
        y = "Number of People",
        title = "Job Status Description of Residents in the North Island of New Zealand"
      ) +
      ggthemes::theme_solarized() +
      theme(
        legend.position = "left",
        legend.justification = "left",
        plot.title = element_text(size = 15L,
                                  hjust = 0.5)
      )
  })
  
  ##### MAORI #####
  maori_select = reactive({ maori |>
      filter(`Area_description` == input$maori_place) })
  
  output$maori_plot = renderPlot({
    maori_select() |>
      ggplot() +
      aes(
        x = Year,
        y = Census,
        colour = Maori_ethnic_group_indicator_summary_description
      ) +
      geom_smooth(se = TRUE) +
      scale_color_brewer(palette = "Set2", direction = 1) +
      labs(
        x = "Year",
        y = "Population Count (usually only residents 15 years or older)",
        title = "Comparing the Maori Population vs the Non Maori Population"
      ) +
      theme_gray() +
      theme(
        plot.title = element_text(size = 14L,
                                  hjust = 0.5),
        axis.title.y = element_text(size = 13L),
        axis.title.x = element_text(size = 13L)
      )
  })
  
  ##### RELIGION #####
  rel_select = reactive({ religion |>
      filter(`Area_description` == input$rel_place) })
  
  output$rel_plot = renderPlot({
    rel_select() |>
      ggplot() +
      aes(x = Year, y = Percent, colour = `Religion`) +
      geom_smooth(se = TRUE) +
      scale_color_manual(
        values = c(`Buddhism` = "#1B9E77",
                   `Christian` = "#FF6400",
                   `Hinduism` = "#C005DC",
                   `Islam` = "#A22802",
                   `Judaism` = "#FFE718",
                   `Maori religions, beliefs, and philosophies` = "#00C8B0",
                   `No religion` = "#020202")
      ) +
      labs(
        x = "Years",
        y = "Percentage Who Believe",
        title = "Religious Beliefs"
      ) +
      ggthemes::theme_fivethirtyeight() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 15L),
        axis.title.x = element_text(size = 14L)
      )
  })
  
  ##### SMOKING #####
  smoking_select = reactive({ smoking |>
      filter(`Area_description` == input$smoking_place) })
  
  output$smoking_plot = renderPlot({
    smoking_select() |>
      ggplot() +
      aes(
        x = Year,
        y = Percent,
        colour = `Cigarette Smoking Behavior`
      ) +
      geom_smooth(se = TRUE) +
      scale_color_hue(direction = 1) +
      labs(
        x = "Year",
        y = "Percent with Specified Smoking Behavior",
        title = "Smoking Behavior Over Time"
      ) +
      theme_dark() +
      theme(
        plot.title = element_text(size = 16L,
                                  face = "bold",
                                  hjust = 0.5)
      )
  })
  
  ##### TRANSPORTATION #####
  tran_select = reactive({ transport |>
      filter(`Area_description` == input$tran_place) })
  
  output$tran_plot = renderPlot({
    tran_select() |>
      ggplot() +
      aes(x = Census, y = `Means of Transport`) +
      geom_bar(stat = "summary", fun = "sum", fill = "#B45050") +
      labs(
        x = "Number of Students",
        y = "Transportation Method",
        title = "Method of Transportation for Students"
      ) +
      ggthemes::theme_igray() +
      theme(
        plot.title = element_text(size = 15L,
                                  face = "bold",
                                  hjust = 0.5),
        axis.title.y = element_text(size = 12L,
                                    face = "bold"),
        axis.title.x = element_text(size = 12L,
                                    face = "bold")
      )
  })


}

# Run the application 
shinyApp(ui = ui, server = server)
