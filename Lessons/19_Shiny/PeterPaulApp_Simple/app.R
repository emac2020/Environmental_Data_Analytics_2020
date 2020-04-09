#### Load packages ----
library(shiny)
library(tidyverse)

getwd()
#### Load data ----
nutrient_data <- read_csv("Data/NTL-LTER_Lake_Nutrients_PeterPaul_Processed.csv") #go into data folder in the shiny app
nutrient_data$sampledate <- as.Date(nutrient_data$sampledate, format = "%Y-%m-%d")
nutrient_data <- nutrient_data %>%
  filter(depth_id > 0) %>%
  select(lakename, sampledate:po4)

#### Define UI ----
ui <- fluidPage(
  titlePanel("Nutrients in Peter Lake and Paul Lake"),
  sidebarLayout(
    sidebarPanel(
      
      # Select nutrient to plot
      selectInput(inputId = "y", 
                  label = "Nutrient", # label of sidebar
                  choices = c("tn_ug", "tp_ug", "nh34", "no23", "po4"), #5 choices it can choose from the dropdown menu
                  selected = "tp_ug"), # has to be one of the 5 options from above. This is the first one listed
  
      ),

    # Output, specify sidebar panel above, then main panel below. want main panel to be a scartterplot
    mainPanel(
      plotOutput("scatterplot")
    ))) # this is where 'fluidpage' ends. What page should look like and where it's getting data is all part of the User Interface

#### Define server  ---- anything interactive or that draws on the data above goes in server below
server <- function(input, output) {
     
    # Create a ggplot object for the type of plot you have defined in the UI  
       output$scatterplot <- renderPlot({ #we specified the output as scatterplot on line 29 so can call it up here. creates the object to actually populate the output
        ggplot(nutrient_data, #pull dataframe from line 7
               aes_string(x = "sampledate", y = input$y, #anything interactive will be input from line 20
                          fill = "depth_id", shape = "lakename")) + # x-axis, fill, and shape are hard coded and not interactive
          geom_point(alpha = 0.8, size = 2) +
          theme_classic(base_size = 14) +
          scale_shape_manual(values = c(21, 24)) +
          labs(x = "Date", y = expression(Concentration ~ (mu*g / L)), shape = "Lake", fill = "Depth ID") +
          scale_fill_distiller(palette = "YlOrBr", guide = "colorbar", direction = 1)
          #scale_fill_viridis_c(option = "viridis", begin = 0, end = 0.8, direction = -1)
      }) # create a plot and close it: lines 36-46
       
       
  } #anything that's included in server will be between these curly brackets on line 33 and 49


#### Create the Shiny app object ----
shinyApp(ui = ui, server = server)


