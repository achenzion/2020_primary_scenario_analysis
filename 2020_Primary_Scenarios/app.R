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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            
            # Input: Text for providing a caption ----
            # Note: Changes made to the caption in the textInput control
            # are updated in the output area immediately as you type
            textInput(inputId = "caption",
                      label = "Caption:",
                      value = "Sample Text"),
            
            # Input: Selector for choosing dataset ----
            # TO DO: Conditional choices
            selectInput(inputId = "dataset",
                        label = "Choose a dataset:",
                        choices = c("rock", "pressure", "cars")),
            
            # Input: Numeric entry for number of obs to view ----
            numericInput(inputId = "obs",
                         label = "Number of observations to view:",
                         value = 10)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            # Output: Formatted text for caption ----
            h3(textOutput("caption", container = span)),
            
            plotOutput("distPlot")
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

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
