#
# This Shiny app supports the front-end for the 2020 primary scenario analysis
# Currently it is just the sample Shiny web application. You can run the application by clicking
# the 'Run App' button above (in RStudio).
#
# TO DO:
#   - See prototype.R for inputs and graphic
#   - Need interaction for:
#    1. Ability to customize which poll you use
#    2. Threshold below which candidates drop
#    3. Potential Reallocation rules
# 

library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)

# Read data in from 538
polls538 <- read.csv(url("https://projects.fivethirtyeight.com/polls-page/president_primary_polls.csv")) %>%
    filter(party=="DEM") %>%
    filter(cycle==2020)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("2020 Democratic Primary Scenarios"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            
            sliderInput("cutoff",
                        "Minimum poll cutoff:",
                        min = 0,
                        max = 20,
                        value = 3),
            
            # Input: Selector for choosing dataset ----
            # TO DO: Conditional choices
            selectInput(inputId = "poll",
                        label = "Choose a poll:",
                        choices = unique(polls538$poll_id)),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            # Output: Formatted text for caption ----
            h3(textOutput("caption", container = span)),
            
            plotlyOutput("resultPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {      
    
    # Create caption ----
    # The output$caption is computed based on a reactive expression
    # that returns input$caption. When the user changes the
    # "caption" field:
    #
    # 1. This function is automatically called to recompute the output
    # 2. New caption is pushed back to the browser for re-display
    #
    # Note that because the data-oriented reactive expressions
    # below don't depend on input$caption, those expressions are
    # NOT called when input$caption changes
    output$caption <- renderText({
        input$caption
    })

    output$resultPlot <- renderPlotly({
        # generate bins based on input$bins from ui.R 
        
        main_data <- polls538 %>%
            filter(poll_id==input$poll)
            
        main_data_new <- main_data %>%
            filter(pct>input$cutoff) 
        
        remaining <- (100-sum(main_data_new$pct))
        even_split_remaining <- remaining/count(main_data_new)[[1]]
        
        main_data$pct_new <- main_data$pct + (main_data$pct>=input$cutoff)*even_split_remaining
        
        #Plot results for each candidate
        cutoff <- data.frame(yintercept=input$cutoff, cutoff=factor(input$cutoff))
        ggplot(data = main_data) +
            geom_point(colour="black",fill="black", shape=21, size = 1, aes(x = candidate_name, y=pct)) +
            geom_point(colour="blue",fill="blue", shape=22, size = 1,aes(x = candidate_name, y=pct_new)) +
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            geom_hline(aes(yintercept=yintercept, linetype=cutoff), data=cutoff,show.legend = FALSE) 
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
