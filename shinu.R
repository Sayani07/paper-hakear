library(shiny)
library(plotly)
library(ggplot2)

ui <- fluidPage(
  plotlyOutput("mainplot"),
  plotlyOutput("hover")
)

server <- function(input, output) {
  output$mainplot <- renderPlotly({
    # https://plot.ly/r/
    d <- diamonds[sample(nrow(diamonds), 1000), ]
    plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity), mode = "markers", color = carat, size = carat, source="main")
  })
  
  output$hover <- renderPlotly({
    eventdat <- event_data('plotly_hover', source="main") # get event data from source main
    if(is.null(eventdat) == T) return(NULL)        # If NULL dont do anything
    point <- as.numeric(eventdat[['pointNumber']]) # Index of the data point being charted
    
    # draw plot according to the point number on hover
    plot_ly(  x = c(1,2,3), y = c(point, point*2, point*3), mode = "scatter")
  })
}
shinyApp(ui, server)

