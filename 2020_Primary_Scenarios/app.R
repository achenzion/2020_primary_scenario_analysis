#
# This Shiny app supports the front-end for the 2020 primary scenario analysis
# Currently it is just the sample Shiny web application. You can run the application by clicking
# the 'Run App' button above (in RStudio).
#
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
            
            
            # Input: Selector for choosing poll ----
            selectInput(inputId = "poll",
                        label = "What would a poll (select) look like...",
                        choices = unique(polls538$poll_summary)),
            
            # Input: Minimum cutoff for reallocation ----
            sliderInput("cutoff",
                        "if all candidates below a minimum cutoff (select) were removed...",
                        min = 0,
                        max = 25,
                        value = 3),
            
            
            # Input: Selector for reallocation method ----
            selectInput(inputId = "reallocMethod",
                        label = "and the shares were reallocated...",
                        choices = c("evenly","all to a single candidate...")),
            
           # Input: Selector for choosing reallocation candidate ----
           selectInput(inputId = "selectedCandidate",
                        label = "[IF 'all to...']",
                        choices = unique(polls538$candidate_name)),

        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            plotlyOutput("resultPlot"),
            
            tags$a(href="https://projects.fivethirtyeight.com/polls/president-primary-d/","Source: FiveThirtyEight Latest Polls")
            
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
        
        remaining <- (sum(main_data$pct)-sum(main_data_new$pct))
        even_split_remaining <- remaining/count(main_data_new)[[1]]
        
        if (input$reallocMethod == "evenly") {
            main_data$pct_new <- main_data$pct + (main_data$pct>=input$cutoff)*even_split_remaining
        } else if (input$reallocMethod == "all to a single candidate...") {
            main_data$pct_new <- main_data$pct + (main_data$candidate_name==input$selectedCandidate)*remaining
        }
        main_data$pct_new[main_data$pct<input$cutoff] <- NA
        
        orig <- main_data[,c("candidate_name","pct")]
        orig$type <- "Raw"
        new <- main_data[,c("candidate_name","pct_new")]
        names(new) <- c("candidate_name","pct")
        new$type <- "Reallocated"

        plot_data <- rbind(orig,new)
        
        #Plot results for each candidate
        cutoff <- data.frame(yintercept=input$cutoff, cutoff=factor(input$cutoff))
        ggplot(data = plot_data,aes(y = pct, x = candidate_name, 
                                    shape=type, colour=type)) +
            geom_point() +
            theme_minimal() + 
            labs(x="Candidates",y="Poll Result (%)",title="Poll Results") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            geom_hline(aes(yintercept=yintercept, linetype=cutoff), data=cutoff,show.legend = FALSE) 
        
    })
    
    session$onSessionEnded(stopApp)
}

# Run the application 
shinyApp(ui = ui, server = server)
