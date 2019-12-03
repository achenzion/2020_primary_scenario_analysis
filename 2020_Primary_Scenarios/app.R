#
# This Shiny app supports the front-end for the 2020 primary scenario analysis
# Currently it is just the sample Shiny web application. You can run the application by clicking
# the 'Run App' button above (in RStudio).
#
# 

library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)

# Read data in from 538
polls538 <- read.csv(url("https://projects.fivethirtyeight.com/polls-page/president_primary_polls.csv")) %>%
    filter(party=="DEM") %>%
    filter(cycle==2020) %>%
    filter(state=="") %>%
    unite("poll_summary", c("start_date","end_date","pollster","sponsors","question_id"), sep = " ", remove = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("2020 Democratic Primary Scenarios"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            
            useShinyjs(),
            
            tags$p("All the media coverage on who is winning the 2020 Democratic primary had me wondering..."),
            
            # Input: Minimum cutoff for reallocation ----
            sliderInput("cutoff",
                        "if all candidates below a minimum cutoff (select) were removed...",
                        min = 0,
                        max = 25,
                        value = 3),
            
            
            # Input: Selector for reallocation method ----
            selectInput(inputId = "reallocMethod",
                        label = "and the poll responses were reallocated (select) ...",
                        choices = c("to a single candidate (select)...","evenly")),
            
            box(id = "candidateBox", width = '800px',
                # Input: Selector for choosing reallocation candidate ----
                selectInput(inputId = "selectedCandidate",
                        label = "",
                        choices = unique(polls538$candidate_name)),
            ),
            
            tags$p("who would lead in the primary?"),
            
            # Input: Selector for choosing poll ----
            selectInput(inputId = "poll",
                        label = "Try out the scenarios using the latest national polls (select)",
                        choices = unique(polls538$poll_summary)),
            
            tags$p(
              tags$a(href="https://projects.fivethirtyeight.com/polls/president-primary-d/","Source: FiveThirtyEight Latest Polls\n"),
            ),
        ),

        mainPanel(
      
            # Show a plot of the generated distribution
            plotlyOutput("resultPlot"),
            
            # Output: HTML table with requested number of observations ----
            tableOutput("view"),
            
            tags$p(
              tags$a(href="https://github.com/achenzion/2020_primary_scenario_analysis","Code available in Git (contributions welcome)\n"),
            ),
            
            tags$p(
              tags$a(href="http://achenzion.github.io/","Copyright (c) 2019 Ayal Chen-Zion\n"),
            ),
            
        )
    )
)

# Define server logic required to draw poll results under scenarios
server <- function(input, output, session) { 
    
    main_data <- reactive({
        polls538 %>%
            filter(poll_summary==input$poll)  %>%
            arrange(desc(pct))
    })
    
    main_data_new <- reactive({
        main_data() %>%
            filter(pct>input$cutoff) 
    })
    
    observe({
        updateSelectInput(session, 'selectedCandidate', choices   =levels(droplevels(main_data_new()$candidate_name)) )
    })
    
    
    plot_data <- reactive({
        main_data <- main_data()
        main_data_new <- main_data_new()
        
        remaining <- (sum(main_data$pct)-sum(main_data_new$pct))
        even_split_remaining <- remaining/count(main_data_new)[[1]]
        
        if (input$reallocMethod == "evenly") {
            main_data$pct_new <- main_data$pct + (main_data$pct>input$cutoff)*even_split_remaining
        } else if (input$reallocMethod == "to a single candidate (select)...") {
            main_data$pct_new <- main_data$pct + (main_data$candidate_name==input$selectedCandidate)*remaining
        }
        main_data$pct_new[main_data$pct<=input$cutoff] <- NA
        
        ordered_names <- main_data[order(desc(main_data$pct_new)),]$candidate_name
        
        main_data$candidate_name2 <- factor(main_data$candidate_name, 
                                            levels = ordered_names)
        
        orig <- main_data[,c("candidate_name2","pct")]
        orig$type <- "Original"
        orig$rank <- order(desc(main_data$pct))
        new <- main_data[,c("candidate_name2","pct_new")]
        names(new) <- c("candidate_name2","pct")
        new$type <- "New"
        new$rank <- order(desc(main_data$pct_new))
        
        rbind(orig,new)
        
    })
    
    output$resultPlot <- renderPlotly({
        
        #Plot results for each candidate
        cutoff <- data.frame(yintercept=input$cutoff, cutoff=factor(input$cutoff))
        ggplot(data = plot_data(),aes(y = pct, x = candidate_name2, 
                                    shape=type, colour=type),show.legend = FALSE) +
            geom_point() + 
            theme_minimal() + 
            labs(x="Candidates",y="Poll Result (%)",title="Poll Results",shape="",colour="") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            geom_hline(yintercept=input$cutoff) +
            theme(legend.position = 'top')
        
    })
    
    # Show the first "n" observations ----
    output$view <- renderTable({
        plot_data() %>%
            pivot_wider(names_from = type, values_from = c(pct,rank)) %>%
            select(candidate_name2,rank_Original,pct_Original,pct_New) %>%
            rename(Candidate = candidate_name2) %>%
            rename(Original = pct_Original) %>%
            rename(New = pct_New) %>%
            rename('Original Rank' = rank_Original) %>%
            arrange(desc(New))
            
    })
    
    ## observe if selector is correct for toggling candidate box
    observeEvent(input$reallocMethod, {
        
        if (input$reallocMethod == "to a single candidate (select)...") {
            shinyjs::show(id = "candidateBox")
        } else {
            shinyjs::hide(id = "candidateBox")
        }
    })
    
    session$onSessionEnded(stopApp)
}

# Run the application 
shinyApp(ui = ui, server = server)
