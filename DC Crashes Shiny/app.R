###############
#### setup ####
###############

# load relevant packages
pacman::p_load(data.table, tidyverse, shiny, ggplot2, sf, tigris, rsconnect)

# set working directory
#setwd("/Users/katie/Documents/R Exploration/DC Car Crashes/")

# load data
dt <- fread("Data/Clean_Crashes.csv")

# convert to shapefile
sf <- st_as_sf(dt, coords = c('LONGITUDE', 'LATITUDE'))
sf <- st_set_crs(sf, 4269) # using NAD83 - what tigris uses

# plots of DC, roads from ACS (through tigris)
precincts <- voting_districts("District Of Columbia")
roads <- roads("District Of Columbia", "District of Columbia")


###################
#### set up ui ####
###################

ui <- fluidPage(
  # App title
  titlePanel(title = h1("Washington, DC Traffic Crashes Involving Pedestrians and Bicycles", align = "center"),
             windowTitle = "DC Crashes"),
  
  # use left sidebar/right main panel layout
  sidebarLayout(
    position="left",
    
    ### create sidebar layout ###
    sidebarPanel(
      width = 2,
      
      # choose the types of individuals impacted
      # preset to have all selected
      checkboxGroupInput("checkGroup",
                         label = h4("Show Crashes Involving:"),
                         choices = list("Pedestrians Only",
                                        "Cyclists Only",
                                        "Both Pedestrians and Cyclists"),
                         selected = c("Pedestrians Only", "Cyclists Only", 
                                      "Both Pedestrians and Cyclists")),
      
      # slider for # minor injuries
      sliderInput("sliderMinor", label = h4("Total Minor Injuries"), min = 0, 
                  max = 19, value = c(0, 19)),
      
      # slider for # major injuries
      sliderInput("sliderMajor", label = h4("Total Major Injuries"), min = 0, 
                  max = 7, value = c(0, 7)),
      
      # slider for # fatal injuries
      sliderInput("sliderFatal", label = h4("Total Fatalities"), min = 0, 
                  max = 2, value = c(0, 2))
    ),
    
    ### create main panel layout ###
    mainPanel(
      
      # create a row with the plot
      fluidRow(
        
        plotOutput("Plot",
                   width = 1000,
                   height = 500)
      ),
      
      # create a row with the two year inputs
      fluidRow(
        
        column(
          6, align = "center",
          
          #input year 1 2009-2022
          numericInput("yearOne",
                       label = h3("First Year of Interest:"),
                       value = 2018,
                       min = 2009,
                       max = 2022,
                       step = 1),
        ),
        
        column(
          6, align = "center",
          
          #input year 2 2009-2022
          numericInput("yearTwo",
                       label = h3("Second Year of Interest:"),
                       value = 2021,
                       min = 2009,
                       max = 2022,
                       step = 1),
        )
        
      )
      
    )
  )
)

#############################
#### define server logic ####
#############################

server <- function(input, output) {
  
  # first filter the dataset using the user inputs
  sfInput <- reactive({
    
    # input$slider returns a vector c(min,max) - need to pull the range c(min:max)
    sf %>% subset (TOTAL_MINOR_INJURIES %in% c(input$sliderMinor[[1]]:input$sliderMinor[[2]])
                   & TOTAL_MAJOR_INJURIES %in% c(input$sliderMajor[[1]]:input$sliderMajor[[2]])
                   & TOTAL_FATALITIES %in% c(input$sliderFatal[[1]]:input$sliderFatal[[2]])
                   & YEAR %in% c(input$yearOne,input$yearTwo) 
                   & GROUP_INVOLVED %in% c(input$checkGroup))
  })
  
  # plot
  output$Plot <- renderPlot({
    req(nrow(sfInput()) > 0) # otherwise shiny throws an error for empty plot
    
    sfy <- sfInput()
    
    ggplot() +
      geom_sf(data = precincts, fill = "grey70", col = "grey70") +
      geom_sf(data = roads, col = "grey95") +
      geom_sf(data = sfy, size = 2, aes(col = SPEEDING_INVOLVED)) +
      scale_color_discrete(name="") +
      theme_classic() + 
      facet_wrap(~YEAR)
  })
  
}

#######################
#### run shiny app ####
#######################
shinyApp(ui = ui, server = server)
