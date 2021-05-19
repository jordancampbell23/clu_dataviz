library(shiny)
library(echarts4r)
library(tidyverse)
library(quantmod)
library(zoo)


ui <- fluidPage(
  style = "background-color: #fff9f5; height: 1000px;",
  
  tags$style(HTML("
     @import url('https://fonts.googleapis.com/css2?family=Inria+Sans:wght@700&display=swap');

      h1, h4, #text {
        font-family: 'Inria Sans', sans-serif;
      }")),
  
  div(
    align = "center",
    
    h1("Inflation Hedge - Gold Mining Analysis", style = "font-weight: 800")
    
  ),
  
  
  hr(style = "background: #333"),
  
  fluidRow(
    
    column(3,
           
           selectInput(inputId = "ma",
                       label = "Variable:",
                       choices = c(30, 90, 180, 360))
    ),
    
    column(6),
    
    column(3,
           div(
             align = "left",
             selectInput(inputId = "ticker",
                         label = "Variable:",
                         choices = c("Barrick", "AngloGold Ashanti"))
           )
    )
  ),
  
  echarts4r::echarts4rOutput("chart1", height = "50vh"),
  
  br(),
  
  fluidRow(
    
    column(3),
    
    column(6,
           
           div(
             class = "panel panel-default",
             div(
               class = "panel-body",
               id = "#text",
               
               h4(strong("Gold Mining Firms"), align = "center"),
               
               tags$ul(
                 tags$li("Barrick Gold Corporation is a mining company that produces gold and copper with 16 operating sites in 13 countries. It is headquartered in Toronto, Ontario, Canada.", style = "font-family: 'Inria Sans', sans-serif; font-size: 15px"),
                 br(),
                 tags$li("AngloGold Ashanti Limited is a global gold mining company. It was formed in 2004 by the merger of AngloGold and the Ashanti Goldfields Corporation. It is now a global gold producer with 21 operations on four continents.", style = "font-family: 'Inria Sans', sans-serif; font-size: 15px")
               ),
               br(), br()
               
             ))),
    
    column(3)
    
  )
  
)

server <- function(input, output, session) {
  
  output$chart1 <- echarts4r::renderEcharts4r({
    
    if (input$ticker == "Barrick") {
      
      
      getSymbols("GOLD") 
      GOLD <- as.data.frame(GOLD)
      GOLD$date <- row.names(GOLD)
      
      GOLD <- GOLD %>%
        mutate(MA = rollapply(GOLD.Adjusted, as.numeric(input$ma), mean, align='right', fill=NA),
               l = MA * 0.75,
               u = MA * 1.25)
      
      
      GOLD %>% 
        e_charts(date) %>% 
        e_line(GOLD.Adjusted, name = "Barrick", zlevel = 99) %>% 
        e_line(MA, name = "Moving Average", zlevel = 100) %>% 
        e_datazoom(type = "slider") %>% 
        e_tooltip(trigger = "axis",
                  formatter = echarts4r::e_tooltip_pointer_formatter(style = "currency",
                                                                     digits = 3)) %>%
        echarts4r::e_y_axis(axisLabel = list(fontSize = 14),
                            formatter = echarts4r::e_axis_formatter(style = "currency",
                                                                    digits = 0))
      
    } else {
      
      
      getSymbols("AU")
      df <- as.data.frame(AU)
      df$date <- row.names(df)
      
      df <- df %>%
        mutate(MA = rollapply(AU.Adjusted, as.numeric(input$ma), mean, align='right', fill=NA),
               l = MA * 0.75,
               u = MA * 1.25)
      
      
      df %>% 
        e_charts(date) %>% 
        e_line(AU.Adjusted, name = "AngloGold Ashanti", zlevel = 99) %>% 
        e_line(MA, name = "Moving Average", zlevel = 100) %>% 
        e_datazoom(type = "slider") %>% 
        e_tooltip(trigger = "axis",
                  formatter = echarts4r::e_tooltip_pointer_formatter(style = "currency",
                                                                  digits = 3))  %>%
        echarts4r::e_y_axis(axisLabel = list(fontSize = 14),
                            formatter = echarts4r::e_axis_formatter(style = "currency",
                                                                    digits = 0))
      
    }
    
    
  })
  
}

shinyApp(ui, server)