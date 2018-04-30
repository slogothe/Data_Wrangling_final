library(shiny)
library(tidyverse)
library(rlang)
library(knitr)
source("rough.R")
ui <- fluidPage(tabsetPanel(type = "tabs", tabPanel("Game and Items Data",
  sidebarLayout(
    sidebarPanel(width = 2,
  textInput("game", "Enter Game Here", "Team Fortress 2"),
  selectInput(inputId = "plot_type",label = "Type of Plot:", choices = c("Scatterplot", "Bar Chart")),
  checkboxInput("logtransform", "Take Log Transform?", FALSE),
  sliderInput("range_price", "Range of Prices to Plot (cents)",min = 0, max = 2000, value = c(0,2000)),
  numericInput("numrows", "Number of Rows for Table to Display", 20, min = 1, max = 40),
  selectInput("arrangeby", "Arrange Table in order of", choices = c("Price" = "results.sell_price", "Number of Items for Sale" = "results.sell_listings"))
    ),
  mainPanel(
        fluidRow(column(8,plotOutput("plot1",  height = 800)),
                  column(4, tableOutput('table1'))))
            )),
                tabPanel("Stock Data", sidebarLayout(
                  sidebarPanel(width = 2, textInput("symbol", "Enter Stock Symbol Here", "MSFT"),
                               radioButtons("date_range", "Range of dates to plot",
                                            c("7 Days" = 7,
                                              "30 Days" = 30,
                                              "3 Months" = 90,
                                              "1 Year" = 365))), 
  mainPanel(plotOutput("plot2"))
                      ))
))

server <- function(input, output){
  output$plot1 <- renderPlot({
    if(input$plot_type == "Scatterplot"){
      if(input$logtransform){
        get_items_and_prices(input$game) %>% ggplot(aes(results.sell_price, log(results.sell_listings))) + geom_point() + xlim(input$range_price) + xlab("Price of Item (cents)")+
          ylab("Number of Items for Sale") + ggtitle(paste("Log Plot of Items For Sale in", input$game)) + theme(plot.title = element_text(size = 30)) + 
          theme(axis.title = element_text(size = 20)) + theme(axis.text = element_text(size = 15))
      }
      else{
        get_items_and_prices(input$game) %>% ggplot(aes(results.sell_price, results.sell_listings)) + geom_point() + xlim(input$range_price) + xlab("Price of Item (cents)")+
          ylab("Number of Items for Sale") +  ggtitle(paste("Items For Sale in", input$game)) + theme(plot.title = element_text(size = 30)) + 
          theme(axis.title = element_text(size = 20))+ theme(axis.text = element_text(size = 15))
      }

    }
    else if(input$plot_type == "Bar Chart"){
      if(input$logtransform){
      get_items_and_prices(input$game) %>% group_by(results.sell_price) %>%  summarise(n = sum(results.sell_listings)) %>% ggplot(aes(results.sell_price, log(n))) + 
        geom_bar(stat = "identity")+ xlim(input$range_price) + xlab("Price of Item (cents)") + ylab("Log(Number of Items for Sale)") + 
          ggtitle(paste("Log Plot of Items For Sale in", input$game)) + theme(plot.title = element_text(size = 30)) + theme(axis.title = element_text(size = 20)) + 
          theme(axis.text = element_text(size = 15))
      }
      else{
        get_items_and_prices(input$game) %>% group_by(results.sell_price) %>%  summarise(n = sum(results.sell_listings)) %>% ggplot(aes(results.sell_price, n)) + 
          geom_bar(stat = "identity")+ xlim(input$range_price) + xlab("Price of Item (cents)") + ylab("Number of Items for Sale") +  
          ggtitle(paste("Items For Sale in", input$game)) + theme(plot.title = element_text(size = 30)) + theme(axis.title = element_text(size = 20)) + 
          theme(axis.text = element_text(size = 15))
      }
    }
  })
  output$table1 <- renderTable({get_items_and_prices(input$game) %>% arrange_(input$arrangeby) %>% .[1:input$numrows,] %>%  
      subset(select = -c(results.app_icon, results.sell_price, results.app_name)) %>% 
      plyr::rename(replace = c("results.name" = "Name of Item", "results.sell_listings" = "Number for sale", "results.sell_price_text" = "Price"))})
  output$plot2 <- renderPlot({get_financial_info(input$symbol, days_to_plot = input$date_range) %>% na.omit() %>% ggplot(aes(Date, Open)) + geom_line() + 
      xlab("Time (days)") + ylab("Price of Stock") + ggtitle(paste("Value of Stock over the past", input$date_range, "Days")) + theme(plot.title = element_text(size = 30)) +
      theme(axis.title = element_text(size = 20)) + theme(axis.text = element_text(size = 15))})
  }

# Run the application 
shinyApp(ui = ui, server = server)

