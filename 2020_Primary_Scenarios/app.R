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
    filter(cycle==2020) %>%
    unite("poll_summary", c("start_date","end_date","pollster","sponsors","fte_grade"), sep = " ", remove = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("2020 Democratic Primary Scenarios"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            
            # Input: Minimum cutoff for reallocation ----
            sliderInput("cutoff",
                        "Minimum poll cutoff:",
                        min = 0,
                        max = 25,
                        value = 3),
            
            # Input: Selector for choosing poll ----
            selectInput(inputId = "poll",
                        label = "Choose a poll:",
                        choices = unique(polls538$poll_summary)),
            
            # Input: Selector for reallocation method ----
            selectInput(inputId = "reallocMethod",
                        label = "How should it be reallocated:",
                        choices = c("even","all to selected remaining candidate")),
            
            # Input: Selector for choosing reallocation candidate ----
            selectInput(inputId = "selectedCandidate",
                        label = "Choose a candidate among the remaining:",
                        choices = unique(polls538$candidate_name)),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            plotlyOutput("resultPlot")
        )
    )
)

# Define server logic required to draw poll results under scenarios
server <- function(input, output, session) { 
    
    get_ddf <- reactive({
        polls538 %>%
            filter(poll_summary==input$poll) %>%
            filter(pct>input$cutoff) 
    })
    
    observe({
        updateSelectInput(session, 'selectedCandidate', choices   =levels(droplevels(get_ddf()$candidate_name)) )
    })
        

    output$resultPlot <- renderPlotly({
        
        main_data <- polls538 %>%
                     filter(poll_summary==input$poll)
        
        main_data_new <- main_data %>%
                         filter(pct>input$cutoff) 
        
        remaining <- (100-sum(main_data_new$pct))
        even_split_remaining <- remaining/count(main_data_new)[[1]]
        
        if (input$reallocMethod == "even") {
            main_data$pct_new <- main_data$pct + (main_data$pct>=input$cutoff)*even_split_remaining
        } else if (input$reallocMethod == "all to selected remaining candidate") {
            main_data$pct_new <- main_data$pct + (main_data$candidate_name==input$selectedCandidate)*remaining
        }
        main_data$pct_new[main_data$pct<input$cutoff] <- NA
        
        #Plot results for each candidate
        cutoff <- data.frame(yintercept=input$cutoff, cutoff=factor(input$cutoff))
        ggplot(data = main_data) +
            geom_point(colour="black",fill="black", shape=21, size = 1, aes(x = candidate_name, y=pct)) +
            geom_point(colour="blue",fill="blue", shape=22, size = 1,aes(x = candidate_name, y=pct_new)) +
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            geom_hline(aes(yintercept=yintercept, linetype=cutoff), data=cutoff,show.legend = FALSE) 
        
    })
    
    session$onSessionEnded(stopApp)
}

# Run the application 
shinyApp(ui = ui, server = server)
