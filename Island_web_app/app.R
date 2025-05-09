# MA 415 FINAL PROJECT WEB APP

# Find out more about building applications with Shiny here:
#    https://shiny.posit.co/


library(shiny)
library(shinydashboard)
library(bslib)
library(ggplot2)
library(leaflet)
#library(bsicons)

# source("island_dc.R")

# ui <- fluidPage( 
# Define UI for application that draws a histogram
ui <- page_fluid(

  
  navset_pill_list(
    
    #### INTRO ####
    nav_panel("Intro", 
              h2("Introduction to the North Island of New Zealand",
                 style = "text-align: center; font-style: bold; font-size: 35px"),
              tags$figure(
                class = "centerFigure",
                tags$img(
                  src = "NorthIsland.jpg",
                  width = 600,
                  alt = "Auckland City"
                ),
                tags$figcaption("Image of Auckland City New Zealand by sxbaird, 2017")
              ),
              hr(style = "border-top: 3px solid black; width: 80%;margin-top:15px;margin-bottom:20px"),
              
              p("New Zealand has two main islands, the North and the South Islands respectively, however there are many other islands that are a part of New Zealand. The North Island or Te Ika-a-Maui is the smaller of the main two, but is still the 14th largest island in the world. It is the most populated island in New Zealand, being home to about 77% of all people who live in New Zealand.",
                style = "margin-bottom:50px")

              ),
    
    
    
    
    
    #### MAP #####
    nav_panel("Map",
              h2("Map of North Island",
                 style = "text-align: center; font-style: bold; font-size: 40px"),
              leafletOutput("map"),
              hr(style = "border-top: 3px solid black; width: 80%;margin-top:15px;margin-bottom:30px"),
              p("North Island has many cities, and places to live. Three of the largest cities are Auckland, Hamilton(within the Waikato region), and Wellington."),
              p("Auckland city: From 2006-2008 Auckland was ranked as the 5th in the world for cities with the best quality of life. Within the city that are over 50 volcanoes, that contribute to its beautiful landscapes. Interestingly, Auckland used to be the capital of New Zealand for 25 years, but in 1865 it lost that role because the journey from South Island took too long."),
              p("Wellington: Is the capital of New Zealand, gaining the title after Auckland lost it; making it the southernmost capital in the world. A few decades later the parliament in Wellington, passed 'Electoral Act of 1893' which made it the first country in the world to give women the right to vote. While it's not number five, Wellington was voted as the 13th best city to live in with high quality of life. A funny fact is that it is beleived that the capital building, 'The Beehive', was intially designed on the back of a napkin as a joke, and the designer never thought it would be built."),
              
              tags$figure(
                class = "centerFigure",
                tags$img(
                  src = "Beehive.jpg",
                  width = 600,
                  alt = "Beehive Parliament Building"
                ),
                tags$figcaption("Image of Beehive Parliament Building in Wellington, New Zealand by gettyimages, 2021")
              ),
              hr(style = "border-top: 0.5px solid black; width: 1%;margin-top:20px;margin-bottom:20px"),
              
              
              p("Hamilton: Is the seventh largest city in New Zealand. Its Maori name is Kirikiriroa, which means long stretch of gravel, and New Zealands longest river, the Waikato flows 16 km through the cities.",
                style = "bottom-margin: 20px"),
              hr(style = "text-align:center; border-top: 1px solid black; width: 95%;margin-top:30px;margin-bottom:30px")
              
              
              ),
    
    
    ### Education ####
    nav_panel("Education", 
              h2("Education Information",
                    style = "text-align: center; font-style: bold; font-size: 40px"),
              
              p("New Zealand as a whole is ranked 7th in the world for its education system. School is mandatory from ages 6 through 16, and public schools are well funded. The majority of classes are taught in English however there are public schools where te reo Māori is used to teach and the curriculum has more of a foundation on Māori culture. There is outdoor education, usually in Yeaers 5,6,7,8,9, and 10 the students will spend a few nights camping outdoors with their peers building resourcefulness and other useful skills.",
                style = "margin-top: 20px; margin-bottom: 30px"),
              p("Below are ways of selecting various regions in the North Island to examine the education levels over time. The table allows you to look at the values for one year at a time, while the plot shows how things have changed over time.",
                style = "margin-bottom:30px"),
              
              fluidRow(column(8,
                              selectInput("place", "Places", choices = edu$`Area_description`),
                              # take out island and oceanic bay
                              ),
                       column(4,
                              selectInput("edu_year", "Year", choices = edu$`Year`)
                              )
                       
              ),
              tableOutput("edu_table"),
              
              plotOutput("edu_plot"),
              hr(style = "text-align:center; border-top: 1px solid black; width: 95%;margin-top:30px;margin-bottom:30px")
    ),
    
    #### Culture (ethnicity, expat, & maori) #####
    
    nav_panel("Culture, Diversity, & Immigration",
              
              tabBox(
                id = "tabset1", height = "550px", width = NULL,
                
                tabPanel("Information",
                         h2("Culture",
                            style = "text-align: left; font-style: bold; font-size: 40px"),
                         p("The North Island of New Zealand is home to people from many different cultural backgrounds. The Māori people are the indigenous Polynesian people of the country, however they are only the second largest ethnic group in the country as you will be able to see with the graphs and plots in the other tabs. Additionally, New Zealand is considered the 14th safest country for expatriates.",
                           style = "margin-top: 30px; margin-bottom: 30px"),
                         
                         tags$figure(
                           class = "centerFigure",
                           tags$img(
                             src = "Maori.jpg",
                             width = 600,
                             alt = "Maori Culture and Hospitality"
                           ),
                           tags$figcaption("Te Puia Maori Rotorua by Fraser Clements")
                         ),
                         hr(style = "border-top: 1px solid black; width: 20%; margin-top:10px;margin-bottom:30px")

                         ),
           #### Ethnicity Data #####     
                tabPanel("Ethnicity Data",
                         
                         h2("Examining the Various Ethnic Backgrounds",
                            style = "text-align: left; font-style: bold; font-size: 40px"),
                        
                         p("Below are ways of selecting various regions in the North Island to examine the prevalence of ethnic groups over time. The table allows you to look at the specific values for a specific year, while the plot shows how things have changed over time.",
                           style = "margin-top: 30px; margin-bottom:30px"),
                         
                         fluidRow(column(8,
                                         selectInput("eth_place", "Places",
                                                     choices = ethnicity$`Area_description`)
                                          ),
                         column(4,
                                selectInput("eth_year", "Year",
                                            choices = ethnicity$`Year`)
                                )
                          ),
                         tableOutput("eth_table"),
                         
                         plotOutput("eth_plot"),
                         hr(style = "text-align:center; border-top: 3px solid black; width: 95%;margin-top:30px;margin-bottom:40px")
                         
                ),
                
          ##### Expat Data ########
                tabPanel("Expatriate Data",
                         h2("How Many Expatriates Are in North Island?",
                            style = "text-align: left; font-style: bold; font-size: 40px"),
                         
                         p("Below are ways of selecting various regions in the North Island to examine expatriate information to examine the number of people born overseas and how long they have been in New Zealand . The table allows you to look at the specific values for a specific year, while the plot shows how things have changed over time.",
                           style = "margin-top: 30px; margin-bottom:30px"),
                         fluidRow(column(8,
                                         selectInput("exp_place", "Places",
                                                     choices = expat$`Area_description`)
                         ),
                         column(4,
                                selectInput("exp_year", "Census Year",
                                            choices = expat$`Year`),
                         )
                         ),
                         tableOutput("exp_table"),
                         
                         plotOutput("exp_plot"),
                         hr(style = "text-align:center; border-top: 3px solid black; width: 95%;margin-top:30px;margin-bottom:40px")

                    ),
                
                #### Maori Data #####
                
                tabPanel("Maori Data",
                         h2("The Indigenous People of North Island",
                            style = "text-align: left; font-style: bold; font-size: 40px"),
                         
                         p("Below are ways of selecting various regions in the North Island to compare the Maori population with the Non-Maori population. The table allows you to look at the specific values for a specific year, while the plot shows how things have changed over time.",
                           style = "margin-top: 30px; margin-bottom:30px"),
                         
                         
                         fluidRow(column(8,
                                         selectInput("maori_place", "Places",
                                                     choices = maori$`Area_description`)
                         ),
                         column(4,
                                selectInput("maori_year", "Year",
                                            choices = maori$`Year`)
                         )
                         ),
                         
                         tableOutput("maori_table"),
                         
                         plotOutput("maori_plot"),
                         hr(style = "text-align:center; border-top: 3px solid black; width: 95%;margin-top:30px;margin-bottom:30px")

                         ),
          
          ##### Religion #####
                tabPanel("Religion",
                         h2('What are the predominant religions and beliefs?',
                            style = "text-align: left; font-style: bold; font-size: 40px"),
                         
                         fluidRow(column(8,
                                         selectInput("rel_place", "Places",
                                                     choices = religion$`Area_description`)
                         ),
                         column(4,
                                selectInput("rel_year", "Year",
                                            choices = religion$`Year`)
                         )
                         ),
                         tableOutput("rel_table"),
                         plotOutput("rel_plot"),
                         hr(style = "text-align:center; border-top: 3px solid black; width: 95%;margin-top:30px;margin-bottom:30px")
                  
                      )
                
                  )
              ),
    
 
    
    
    #### Housing #####
    nav_panel("Living Information",
              
              h2("What is employment and housing like in North Island?",
                 style = "text-align: left; font-style: bold; font-size: 40px; top-margin: 30px; bottom-margin:40px"),
              tags$figure(
                class = "centerFigure",
                tags$img(
                  src = "Auckland2.jpg",
                  width = 600,
                  alt = "Auckland at night"
                ),
                tags$figcaption("Auckland city at night by New Zealand Tourism Group"),
                style = "text-align:center; bottom-margin: 40px"
              ),
              p("Below are three ways of examining life in the North Island. You are able to look at housing descriptions, the income distribution, and the types employment people have.",
                style = "top-margin: 20px; bottom-margin: 30px"),
              
              
              accordion(
                
                #### Housing #####
                accordion_panel(
                  title = "Examining Housing Data",
                  icon = bsicons::bs_icon("menu-app"),
                  "Examining Housing Data",

                  fluidRow(column(8,
                                  selectInput("house_place", "Places",
                                              choices = house$`Area_description`)
                  ),
                  column(4,
                         selectInput("house_year", "Year",
                                     choices = house$`Year`)
                  )
                  ),

                  tableOutput("house_table"),

                  plotOutput("house_plot"),
                  hr(style = "text-align:center; border-top: 1px solid black; width: 95%;margin-top:10px;margin-bottom:10px")
                ),
                
                ##### Income #####
                accordion_panel(
                  title = "Incomes",
                  icon = bsicons::bs_icon("bar-chart"),
                  "How much are people making?",

                  fluidRow(column(8,
                                  selectInput("inc_place", "Places",
                                              choices = income$`Area_description`)
                      )
                  ),
                  tableOutput("inc_table"),

                  plotOutput("inc_plot"),
                  hr(style = "text-align:center; border-top: 1px solid black; width: 95%;margin-top:10px;margin-bottom:10px")
                  
                ),
                
                ###### Job #####
                accordion_panel(
                  title = "Employment",
                  icon = bsicons::bs_icon("calendar-date"),
                  "What is the work life",

                  fluidRow(column(8,
                                  selectInput("job_place", "Places",
                                              choices = job$`Area_description`)
                  ),
                  column(4,
                         selectInput("job_year", "Year",
                                     choices = job$`Year`)
                  )

                  ),

                  tableOutput("job_table"),

                  plotOutput("job_plot"),
                  hr(style = "text-align:center; border-top: 1px solid black; width: 95%;margin-top:10px;margin-bottom:10px")

                ),
                id = "acc",
                open = "Examining Housing Data"
              ) 
            ),
    
    
    ##### Smoking ####
    nav_panel("Smoking",
              h2("How prevalent is smoking?",
                 style = "text-align: left; font-style: bold; font-size: 40px; margin-top: 20px; margin-bottom:40px"
              ),
              
              fluidRow(
                layout_columns(
                  card(
                    card_header("Area"),
                    selectInput("smoking_place", "Places",
                                choices = smoking$`Area_description`)
                    
                    
                  ),
                  card(
                    card_header("Year"),
                    selectInput("smoking_year", "Year",
                                choices = smoking$`Year`)
                    
                  )
                )
              ),
                       
              tableOutput("smoking_table"),
              
              plotOutput("smoking_plot"),
              hr(style = "border-top: 1px solid black; text-align:center; width: 80%; margin-top:20px;margin-bottom:30px")
              
          ),
    
    ##### Transportation #####
    nav_panel("Transportation",
              h2("How do North Islanders commute?",
                 style = "margin-top: 10px; text-align-left; font-style: bold; font-size: 40px; margin-bottom: 25px"),
              
              fluidRow(
                layout_columns(
                  card("Region in North Island",
                       layout_columns(
                         card("",
                              selectInput("tran_place", "Places",
                                          choices = transport$Area_description)
                              ),
                         card("Depending on wether the region is urban or rural the means of travel will vary.")
                       )
                    )
                )
                  
              ),
              tableOutput("tran_table"),
              
              plotOutput("tran_plot"),
              hr(style = "border-top: 1px solid black; text-align:center; width: 80%; margin-top:20px;margin-bottom:30px")
              
              ),
    

      nav_panel("Citations",
              h2("References",
              style = "margin-top: 10px; text-align-left; font-style: bold; font-size: 40px; margin-bottom: 25px"),
              
              p("For coding:",
                style = "font-style: bold; font-size: 17px"),
              a("stackoverflow", href = "https://stackoverflow.com/questions/74334647/how-to-adjust-line-spacing-in-r-shiny"),
              p(),
              a("Building Shiny apps the BOAST way", href = "https://educationshinyappteam.github.io/Style_Guide/staticImages.html"),
              p(),
              a("Geeks for Geeks", href = "https://www.geeksforgeeks.org/how-to-read-many-files-in-r-with-loop/"),
              p(),
              a("Steve's Data Tips and Tricks", href = "https://www.spsanderson.com/steveondata/posts/2024-06-07/"),
              p(),
              a("Shiny Layouts", href = "https://shiny.posit.co/r/layouts/"),
              p(),
              a("Mastering Shiny in R", href = "https://mastering-shiny.org/basic-case-study.html"),
              
              p("For North Island Information:",
                style = "margin-top: 40px; font-style: bold; font-size: 15px, margin-bottom:20px"),
              a("New Zealand Census", href = "https://www.stats.govt.nz/2018-census/"),
              p(),
              a("NZ Tourism", href = "https://www.nz-tourism.com/list-of-new-zealand-regions/"),
              p(),
              a("Distant Journeys", href = "https://www.distantjourneys.co.uk/blog/ten-interesting-facts-auckland/"),
              p(),
              a("My Guide Wellington", href = "https://www.myguidewellington.com/usefulinfo/wellington-interesting-facts"),
              p(),
              a("Endace", href = "https://www.endace.com/assets/files/careers/hamiltonfactsheet.pdf"),
              p(),
              a("InterNations", href = "https://www.internations.org/new-zealand-expats/guide/living")
              
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
        focR(input$place) })
  
  output$edu_table = renderTable({
    edu_select() |> filter(`Year` == input$edu_year)
  })
  
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
  
  output$eth_table = renderTable({
    eth_select() |> filter(`Year` == input$eth_year)
  })
  
  output$eth_plot = renderPlot({
    options(scipen=10000)
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
  
  output$exp_table = renderTable({
    exp_select() |> filter(`Year` == input$exp_year) |>
      select(!`Years_since_arrival_in_New_Zealand_code`)
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
  
  output$house_table = renderTable({ house_select() |>
      filter(`Year` == input$house_year)})
  
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
  
  output$inc_table = renderTable({
    inc_select() |> select(!`Total_personal_income_code`)
  })
  
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
  
  output$job_table = renderTable({ job_select() |>
      filter(`Year` == input$job_year)
  })
  
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
  
  output$maori_table = renderTable({ maori_select() |>
      filter(`Year` == input$maori_year)
  })
  
  output$maori_plot = renderPlot({
    maori_select() |>
      ggplot() +
      aes(
        x = Year,
        y = Census,
        colour = `Maori Ethnicity`
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
  
  output$rel_table = renderTable({ rel_select()|>
      filter(`Year` == input$rel_year)
  })
  
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
  
  output$smoking_table = renderTable({ smoking_select() |>
      filter(`Year` == input$smoking_year) })
  
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
  
  output$tran_table = renderTable({
    tran_select()
  })
  
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
