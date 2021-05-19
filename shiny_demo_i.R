library(shiny)



#########
### 1 ###
#########

ui <- fluidPage(
  
  
)

server <- function(input, output, session) {
  
  
}

shinyApp(ui, server)



#########
### 2 ###
#########

mtcars

plot(mtcars[, "cyl"], mtcars$mpg)


ui <- fluidPage(
  
  
  h1("Our App Title"),
  
  
  selectInput(inputId = "var",
              label = "Variable:",
              choices = c(names(mtcars))),

  plotOutput("chart1")
  
)

server <- function(input, output, session) {
  
  # output$chart1 <- renderPlot({
  # 
  #   plot(mtcars$cyl, mtcars$mpg)
  # 
  # 
  # })
  
  output$chart1 <- renderPlot({

    plot(mtcars[, input$var], mtcars$mpg)


  })

}

shinyApp(ui, server)



#########
### 3 ###
#########


ui <- fluidPage(
  h1("Our App Title"),
  selectInput(inputId = "var",
              label = "Variable:",
              choices = c(names(mtcars))),
  plotOutput("chart1")
)

server <- function(input, output, session) {
  output$chart1 <- renderPlot({
    plot(mtcars[, input$var], mtcars$wt)
  })
  
}

shinyApp(ui, server)