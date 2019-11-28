setRepositories(graphics = getOption("menu.graphics"),
                ind = NULL, addURLs = character())

install.packages('rsconnect')
install.packages('tidyr')
install.packages('dplyr')
install.packages('ggplot2')
install.packages("plotly")
install.packages("shiny")
install.packages("shinyjs")
install.packages("shinydashboard")

rsconnect::setAccountInfo(name='achenzion', token='B3ED29C9809AE25710864D9506DE196F', 
                          secret='<secret>')

