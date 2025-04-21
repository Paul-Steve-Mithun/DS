install.packages("shiny")
library(shiny)

# Load iris 
data(iris)

ui <- fluidPage(
  titlePanel("Iris Shiny App"),
  selectInput("num", "Numerical Variable:", choices=names(iris)[1:4]),
  selectInput("cat", "Categorical Variable:", choices=c("Species")),
  verbatimTextOutput("summary"),
  selectInput("x", "X-axis Variable:", choices=names(iris)[1:4]),
  selectInput("y", "Y-axis Variable:", choices=names(iris)[1:4]),
  plotOutput("scatter"),
  plotOutput("boxplot")
)
server <- function(input, output) {
  output$summary <- renderPrint({
    summary(iris[[input$num]])
  })
  output$scatter <- renderPlot({
    plot(iris[[input$x]], iris[[input$y]], col=iris$Species, pch=19)
  })
  output$boxplot <- renderPlot({
    boxplot(iris[[input$num]] ~ iris[[input$cat]], main="Boxplot")
  })
}
shinyApp(ui = ui, server = server)