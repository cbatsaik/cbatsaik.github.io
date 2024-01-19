


# Packages and Data

library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(shinythemes)
library(naniar)
library(simputation)


final_opi <- read_csv("~/Documents/COMP 212/Opioid_Epidemic/data/final_opi.csv")

norm.presc_counts <- read_csv("~/Documents/COMP 212/Opioid_Epidemic/data/norm.presc_counts.csv")

prescriber_info_specialty <- read_csv("~/Documents/COMP 212/Opioid_Epidemic/data/james_prescriber.csv")

Vari_Imp <- read_csv("~/Documents/COMP 212/Opioid_Epidemic/data/Vari_Imp.csv")

death_and_presc_region <- read_csv("~/Documents/COMP 212/Opioid_Epidemic/data/death_and_presc_region.csv")

crude_map_data <- read_csv("~/Documents/COMP 212/Opioid_Epidemic/data/crude_map_data.csv")

mapdata <- read_csv("~/Documents/COMP 212/Opioid_Epidemic/data/mapdata.csv")

mapdata_south <- read_csv("~/Documents/COMP 212/Opioid_Epidemic/data/mapdata_south.csv")

mapdata_west <- read_csv("~/Documents/COMP 212/Opioid_Epidemic/data/mapdata_west.csv")

mapdata_northeast <- read_csv("~/Documents/COMP 212/Opioid_Epidemic/data/mapdata_northeast.csv")

mapdata_northcentral <- read_csv("~/Documents/COMP 212/Opioid_Epidemic/data/mapdata_northcentral.csv")


Wide_Master <- read_csv("~/Documents/COMP 212/Opioid_Epidemic/data/Wide_Master.csv")
opi_epi <- Wide_Master %>%
  select(State, County, Year, Population, Income, Unemployment, Total, Less_Than_HS, Non_US_Born)


# Changing the variable names for selection 
variable_names <- names(final_opi)
custom_labels <- c("Unemployment" = "state_unemploy", "Income" = "state_income", "Non High School Grad" = "less_than_hs", "Born Outside US" = "non_us_born")
custom_labels_graph <- c("state_unemploy" = "How Does State Unemployment Affect Overdose Deaths?" , "state_income" = "How Does State Income Affect Overdose Deaths?", "less_than_hs" = "How Does Not Graduating High School Affect Overdose Deaths?", "non_us_born" = "How Does being Born Outside US Affect Overdose Deaths?")


# Define UI for application that draws a histogram
ui <- navbarPage(
  title = h1("Viewing The Opioid Epidemic in The United States", style = 'color: #900D09; font-size: 40px; margin-top: -10px  '),
  id = "tabs",
  position = "static",
  fluid = TRUE,
  tags$head(
    tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                    height: 60px;
                    padding-top: 20px
                    }'))
  ),
  theme = shinytheme("simplex"),
  
  tabPanel("Purpose",
           mainPanel(
             h1('Our Research'),
             h2('Why the Opioid Epidemic?'),
             h1("'From 1999 to 2021, nearly 645,000 people died from an overdose involving any opioid'"),
             h3('We wanted a middle ground between health and social issues.'),
             h3('.                                        .'),
             h2('What were our questions?'),
             h3('How do rates of prescription opioids by doctors contribute to higher rates of overdose?'),
             h3('How have trends changed over the years?'),
             h3('.                                        .'),
             h2('From here, we created our dashboard to show various trends of the Opioid Epidemic.')
           )
    
  ),
  
  tabPanel("Prescriptions",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               h3('Accidental drug overdose fatalities are on the rise in the United States.'),
               h4('This dataset comprises prescription records for 250 common drugs, including opioids, issued by 25,000 licensed medical professionals in 2014. It is a subset of a larger dataset, sourced from cms.gov, containing 24 million prescription instances. The data has been streamlined to focus on 25,000 unique prescribers.

The data consists of the following characteristics for each prescriber:

NPI (National Provider Identifier) for each medical professional.
Gender, state, and credentials of the prescribers.
Specialty of each medical professional.
Detailed drug prescription data.
An indicator of whether a prescriber issued opiate drugs more than 10 times in a year.')
             ),
             mainPanel(
               fluidRow(
                 column(width = 12, plotlyOutput("myPlot"))
               )
             )
           )
  ),
  
  tabPanel("Overdose",
           tabsetPanel(
             tabPanel("Variable Importance",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          h3('Finding Important Variables'),
                          h4('To help make sense of patterns and connections within our data,
                             we created a random forest model. We focused on predictor variables such as average income,
                             proportion of foreign-born residents, education levels, and other indicators.'),
                          h4('To make things more understandable, we created a 500 tree forest to discover which factors play a 
                             major role in predicting opioid overdoses from the year 2014 data for
                             different states. 
                             '),
                          h4('As we can see, economic factors play a crucial role in the opioid crisis, with unemployment rates and state
                             GDP per capita as the two most important variables. Educational factors had some importance while proportion of 
                             foreign-born residents plays less of a role in overdose.
                             From these importance variables, we decided to make additional visuals to help understand the trends
                             within each state region and state.')
                        ),
                        mainPanel(plotOutput("importance"))
                      )),
             tabPanel("Fatal Overdose",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          h3('Viewing Overdose Fatality Trends in U.S. States and Regions 2011-2017'),
                          selectInput(
                            "selectedOption2", "Select Variable:", choices = custom_labels
                          ),
                          selectInput("regionInput", "First Select Region...", choices = unique(final_opi$region.x)),
                          selectInput("stateInput", "... Then Select a State", choices = NULL),
                          h4('By selecting variables above, you can interact with the visualizations to view the trends and patterns of fatal overdose in different regions and states in the United States.')
                        ),
                        mainPanel(
                          fluidRow(
                            column(width = 12, plotlyOutput("selectionPlot"))
                          ),
                          fluidRow(
                            column(width = 6, plotlyOutput("regionPlot"), height = 100),
                            column(width = 6, plotlyOutput("statePlot"), height = 100)
                          ),
                          fluidRow(
                            column(width = 10, plotlyOutput("james_map"))
                          )
                        )
                      )
             )
           )
  ),
  tabPanel("Spatial Analysis",
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput("mapType", "Select Map Type", choices = c("Opioid analysis map", "Crude death map"), selected = "Opioid analysis map"),
      conditionalPanel(
        condition = "input.mapType == 'Opioid analysis map'",
        selectInput("fillVariable", "Select Variable", choices = c("Population" = "Population",
                                                                   "Deaths" = "Deaths",
                                                                   "Total Prescription" = "state_ttl_presc",
                                                                   "Overdose Rate" = "overdose_by_pop",
                                                                   "Prescription Rate" = "presc_by_pop")),
        selectInput("region", "Select Region", choices = c("All", "South", "Northeast", "North Central", "West")),
        
        selectInput("gender", "Select Gender", choices = c("Both", "F", "M"), selected = "Both"),
        h4('Crude death map:1999-2016'),
        h4('Opioid Analysis Map: 2014'),
        helpText("Both: Displays data for both genders without applying a specific gender filter. If the map's fill color represents a variable that is different between males and females, displaying 'Both' would reflect average representation of that variable for both genders."
                 , style = "color: black;")
      )
    ),
    mainPanel(
      plotlyOutput("mapPlot")
    )
  )
),
  tabPanel("Limitations and Ethics",
           sidebarLayout(
             sidebarPanel(
               width = 4,
               h3('Project Limitations:'),
               h4('The time we had to produce and fine tune our visualizations and shiny app. There are some things we wish we got around to adding and fixing that just
                  was not possible with the time it takes to develop the knowledge of how to add/do it.'),
               h3('Data Limitations:'),
               h4('It was very hard to find as much temporal data as we wanted. The temporal data we do have is flawed as the data is by county, and with such
                  a small unit of measure there were many NA values for many of the variables. Therefore, the overall trends in our data do need to be taken with a grain of salt.'),
               h4('We did not want to impute or fill in this missing data since this is a sensitive topic, and not just harmless data.'),
               h3('Ethics:'),
               h4('With our visualizations and analysis on such a sensitive topic, we do want to empahsize that we do not intend to paint specific places
                  or people in a bad light. ')),
             mainPanel(
               fluidRow(
                 column(width = 10, plotOutput("missing_values"), height = 300),
                 column(width = 10, h5("*Where 'Total' is the total number of overdose fatalities."))
               )
             )
           )
  ),
  tabPanel("Future Work", 
           mainPanel(
             h3('- Go back and add the important variables to the visualizations'),
             h3('- Add a tree to the variable importance page to show example of what the forest means for someone that may not understand
                machine learning'),
             h3('- Our model is too overfit to our data, so in the future it would be better to generalize it more'),
             h3('- Add selection inputs for the map in the Overdose tab'),
             h3('- Explore outliers (like West Virginia)')
           ))
)

# Define server logic required to...
server <- function(input, output) {
  
  
  output$myPlot <- renderPlotly({
    
    
    plot <- norm.presc_counts %>%
      filter(norm >= 150) %>%
      filter(Opioid.Prescriber == 1) %>%
      ggplot(aes(x = reorder(Specialty1, -norm), y = norm, 
                 text = paste("Specialty: ", Specialty1, "\nAvg Opioids Prescribed: ", scales::comma(norm), "\nSpecialty total: ", doc_sum, "\nOpioid Prescribers: ", count_op_presc)))+
      geom_col(fill = "firebrick", alpha = 0.85) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Medical Specialty", y = "Opioids Prescribed (Normalized)", title = "Opioid Prescriptions by Medical Specialty 2014", subtitle = "Where 'Opioid Prescribers' refers to if an individual prescribed opioids to patients more than 10 times") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45))
    
    ggplotly(plot, tooltip = "text")
    
  })
  
  output$selectionPlot <- renderPlotly({
    
   

    # For my lables to be correct in the selection and graph
    selected_variable <- input$selectedOption2
    
    
    custom_label <- custom_labels_graph[selected_variable]
    
   
    selection <- final_opi %>%
      ggplot(aes(x = !!sym(selected_variable), y = od_rate, label = ifelse(final_opi$region.x == input$regionInput, paste0('\n', state_ab, '\n'), state_ab))) +
      geom_text(aes(fontface = ifelse(final_opi$region.x == input$regionInput, "bold", "plain"), text = paste("State:", State, "\nYear: ", Year, "\nSelection:", round(!!sym(selected_variable), 2), "\nOverdose: ", round(od_rate, 2))),color = ifelse(final_opi$region.x == input$regionInput, "black", "darkgray"),
                alpha = ifelse(final_opi$region.x == input$regionInput, 1, 0.6), size = ifelse(final_opi$region.x == input$regionInput, 3, 2.5)) +
      geom_smooth(se = FALSE, color = "firebrick") +
      facet_wrap(~Year) +
      labs(
        x = paste(custom_label),
        y = "Overdose Per 100,000 People",
        title = paste(custom_label)
      ) +
      theme_minimal()
    
    selection
    
    ggplotly(selection, tooltip = "text")
    
  })
  
  
  selected_states <- reactive({
    final_opi$State[final_opi$region.x == input$regionInput]
  })
  
  # Update state choices when the selected region changes
  observe({
    updateSelectInput(inputId = "stateInput", choices = selected_states())
  })
  
  output$statePlot <- renderPlotly({

    selected_state <- input$stateInput
    
    # Filter data for the selected state
    filtered_data <- final_opi %>%
      filter(State == selected_state)
    

    states <- filtered_data %>%
      ggplot(aes(x = Year, y = round(od_rate, 2))) +
      geom_point(aes(text = paste("Year: ", Year, "\nOverdose: ", round(od_rate, 2)))) +
      geom_line(aes(group = 1)) +
      labs(x = "Year", y = "Overdose Deaths per 100,000 Population", title = paste("Overdose Deaths in", selected_state))+
      theme_minimal()
    
    ggplotly(states, tooltip = "text")

  })
  
  # All states in the region plot
  output$regionPlot <- renderPlotly({
    
    region <- input$regionInput
    state <- input$stateInput
    
    filtered <- final_opi %>%
      filter(region.x == region)
    
    regions <- filtered %>%
      ggplot(aes(x = Year, y = round(od_rate, 2))) +
      geom_point(aes(text = paste("State:", State, "\nYear: ", Year, "\nOverdose: ", round(od_rate, 2))), color = ifelse(filtered$State == state, "black", "darkgray"), alpha = ifelse(filtered$State == state, 1, 0.5)) +
      geom_smooth(aes(x = Year, y = od_rate), se = FALSE, color = "firebrick") +
      geom_line(aes(group = State), color = ifelse(filtered$State == state, "black", "darkgray"), alpha = ifelse(filtered$State == state, 1, 0.5), show.legend = FALSE) +
      labs(x = "Year", y = "Overdose Deaths per 100,000 Population", title = paste("Overdose Deaths in", region, "States"))+
      theme_minimal()
    
    ggplotly(regions, tooltip = "text")
    
  })
  
# James' stuff
  
  output$importance <- renderPlot ({
    
    ggplot(Vari_Imp, aes(y = reorder(Variable, importance), x = importance)) +
      geom_point() +
      labs(title = "Importance of Variables in Predicting Fatal Opioid Death Rates",
           y = "Variable",
           x = "Importance") +
      theme_linedraw() +
      theme(
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 10)
      )
  })
  
  output$james_map <- renderPlotly ({
    p <- ggplot(data = death_and_presc_region,
                aes(text = paste("State: ", name,
                                  "\nOverdose rate per 1000 people: ", round(overdose_by_pop, 4),
                                  "\nRegion: ", region,
                                  "\nMean Overdose rate per 1000 people: ", round(mean_od, 4)))) +
      geom_polygon(
        aes(x = long, y = lat, group = group, fill = mean_od),
        color = "black",
        size = 0.0
      ) +
      scale_fill_gradient(
        low = "#F5BBA6",   # Color for low mean_od
        high = "firebrick",  # Color for high mean_od
        na.value = "grey"    # Color for NA values (if any)
      ) +
      theme_linedraw() +
      labs(title = "Mean Overdose Rates by Region")
    
    ggplotly(p, tooltip = "text")
  })
  
  #Data limitations
  
  output$missing_values <- renderPlot({
    vis_miss(opi_epi) +
      theme(axis.text.x = element_text(angle = 45, hjust = .25))
  })
  
  # Charles Stuff
  
  output$mapPlot <- renderPlotly({
    filtered_data <- mapdata
    
    if (input$gender != "Both") {
      filtered_data <- filtered_data[filtered_data$Gender == input$gender, ]
    }
    
    if (input$region != "All") {
      # Filter based on selected region
      if (input$region == "South") {
        filtered_data <- mapdata_south
      } else if (input$region == "Northeast") {
        filtered_data <- mapdata_northeast
      } else if (input$region == "North Central") {
        filtered_data <- mapdata_northcentral
      } else if (input$region == "West") {
        filtered_data <- mapdata_west
      }
    }
    
    if (input$mapType == "Opioid analysis map") {
      plot <- ggplotly(
        ggplot(filtered_data, aes(x = long, y = lat, group = group, fill = !!as.name(input$fillVariable))) +
          geom_polygon(color = "white") +
          scale_fill_gradient(name = input$fillVariable, low = "#F5BBA6", high = "firebrick") +
          labs(title = "Opioid Analysis Map", x = "", y = "") +
          {if (input$fillVariable %in% c("Deaths", "Population", "Overdose Rate")) {
            NULL  # No facet or gender selection for these variables
          } else {
            facet_wrap(~ Gender, scales = "free")  # Facet by gender for other variables
          }
          } +
          theme_minimal() +
          theme(panel.grid = element_blank(),   # Remove grid lines
                panel.background = element_rect(fill = "white"),  # Set background to white
                plot.background = element_rect(fill = "white")))
      
      return(plot)
      
    } else if (input$mapType == "Crude death map") {
      plot <- ggplotly(
        ggplot(crude_map_data, aes(x = long, y = lat, group = group, fill = avg_crude_death_rate)) +
          geom_polygon(color = "white") +
          scale_fill_gradient(low = "#F5BBA6", high = "firebrick", name = "Average Crude Death Rate per 100,000") +
          labs(title = "Average Crude Death Rate from 1999-2016", x = "", y = "") +
          theme_minimal() +
          theme(panel.grid = element_blank(),   # Remove grid lines
                panel.background = element_rect(fill = "white"),  # Set background to white
                plot.background = element_rect(fill = "white")))
      
      return(plot)
    }
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)