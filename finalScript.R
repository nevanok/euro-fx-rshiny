library(shiny)
library(plotly)

# This function will take a url input that that points to a zipped csv file download.
# The file is downloaded, unzipped into temporary memory and read into R.
downloadCSV <- function(url){
  temp <- tempfile()
  download.file(url,temp)
  data = read.csv(unz(temp,"eurofxref-hist.csv"))
  return(data)
}

# This function subsets downloaded data frame to desired columns
dropCurrencys <- function(df){
  df = subset(df, select=c('Date','USD','CNY','GBP','JPY','CZK','INR','PLN','CHF','RUB','AUD','CAD','BRL','HKD','ZAR','ILS','MXN','NZD'))
  return(df)
}

# The function will convert all non-numerical columns to numerical 
# It will ignore the date column (col no. 1) as j=2
convertToNumerical <- function(df){
  j=2
  while (j <= ncol(df)){
    if(typeof(df[2,j]) != 'double'){
      df[j] = as.numeric(as.character(df[[j]]))
    }
    
    j = j+1
  }
  return(df)
}

ui <- fluidPage(
  titlePanel("Euro Forex Charts"),
  sidebarPanel(
    helpText("Up to date foreign exchange rates. All currencies are compared against Euro (EUR)."),
    selectInput("currency", "Select a currency to view:", 
                choices=c("United States Dollar"="USD", "Chinese Yuan"="CNY", "British Pound"="GBP", 
                          "Japanese Yen"="JPY", "Czech Koruna"="CZK", "Indian Rupee"="INR", "Polish Zloty"="PLN",
                          "Swiss Franc"="CHF", "Russian Ruble"="RUB", "Australian Dollar"="AUD", 
                          "Canadian Dollar"="CAD", "Brazilian Real"="BRL", "Hong Kong Dollar"="HKD",
                          "South African Rand"="ZAR", "Israeli Shekle"="ILS", "Mexican Peso"="MXN",
                          "New Zealand Dollar"="NZD")),
    radioButtons("timeRange","Select period of time to view:",
                 choices=c("1 Month"= 22,"3 Months"=65,"6 Months"=128,"1 Year"=260,
                           "2 Years"=521,"5 Years"=1303,"10 Years"=2607,"All"="All"),
                 selected="All"
    )  
  ),
  mainPanel(
    plotlyOutput("price1"),
    textOutput("changeText")
  )
)

server <- function(input, output){
  # When the app initialises, it will download the specified zip folder, open it, drop specified columns
  # and convert all columns to numerical (some are characters/factors).
  url = "https://www.ecb.europa.eu/stats/eurofxref/eurofxref-hist.zip"
  data <- downloadCSV(url)
  data <- dropCurrencys(data)
  data <- convertToNumerical(data)
  
  # Reactive variable to receive user selection of currency
  currencyChoice <- reactive({
    which(colnames(data)==input$currency)
  })
  
  # Reactive variable to receive selection of time period
  timePeriod <- reactive({
    if(input$timeRange=="All"){
      (nrow(data)-1)
    }
    else{
      as.numeric(input$timeRange)}})
  
  # Reactive variable that changes the x-axis ticks based on time period selected
  dateTicks <- reactive({
    if(input$timeRange==22){
      ticks=5
    }
    else if(input$timeRange==65){
      ticks=10
    }
    else if(input$timeRange==128){
      ticks=20
    }
    else if(input$timeRange==260){
      ticks=40
    }
    else if(input$timeRange==521){
      ticks=80
    }
    else if(input$timeRange==1303){
      ticks=240
    }
    else{
      ticks=480
    }
    ticks
  })
  
  # Reactive function that calculates year-to-date percentage change in selected currency
  yearToDateChange <- reactive({
    round((((data[1,currencyChoice()] - data[(1+260),currencyChoice()])/data[(1+260),currencyChoice()])*100),digits=2)
  })
  
  # Reactive variable that reduces the data frame to Date column and selected currency column
  df <- reactive({
    test <- data[1:timePeriod(),c(1,currencyChoice())]
    test[,2] = as.numeric(test[,2])
    test
  })
  
  # Plotting function via plotly
  output$price1 <- renderPlotly({
    plot_ly(df(), x=~Date, y=~df()[,2], type="scatter", mode="lines",
            line = list(color = 'rgb(51,51,255)', width = 3)) %>%
      layout(title = paste('Price of Euro in',colnames(df())[2]),
             hovermode="x",
             xaxis = list(title = 'Date', autotick=F, dtick=dateTicks(),
                          zeroline = TRUE),
             yaxis = list(title = colnames(df())[2]))
  })
  
  # Text output function to display the year-to-date change in selected currency
  output$changeText <- renderText(
    paste("The year-to-date change in EUR /", colnames(df())[2], "is", yearToDateChange(),"%") 
  )
}

shinyApp(ui=ui,server=server)
