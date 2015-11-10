library(shiny)

shinyServer(function(input, output) {
  datasetInput <- reactive({
    #checking to see if this works to update code
    # Fetch the appropriate data object, depending on the value
    # of input$dataset.
    switch(input$stocks,
           "AAPL" = read.csv(url("http://real-chart.finance.yahoo.com/table.csv?s=AAPL&a=08&b=14&c=2014&d=08&e=15&f=2015&g=d&ignore=.csv"), header = TRUE, sep = ","),
           "TSLA" = read.csv(url("http://real-chart.finance.yahoo.com/table.csv?s=TSLA&a=08&b=15&c=2014&d=08&e=15&f=2015&g=d&ignore=.csv"), header = TRUE, sep = ","),
           "YHOO" = read.csv(url("http://real-chart.finance.yahoo.com/table.csv?s=YHOO&a=08&b=15&c=2014&d=08&e=28&f=2015&g=d&ignore=.csv"), header = TRUE, sep = ","),
           "IBM" = read.csv(url("http://real-chart.finance.yahoo.com/table.csv?s=IBM&a=08&b=15&c=2014&d=08&e=15&f=2015&g=d&ignore=.csv"), header = TRUE, sep = ","),
           "GOOG" = read.csv(url("http://real-chart.finance.yahoo.com/table.csv?s=GOOG&a=08&b=15&c=2014&d=08&e=15&f=2015&g=d&ignore=.csvv"), header = TRUE, sep = ","),
           "FB" = read.csv(url("http://real-chart.finance.yahoo.com/table.csv?s=FB&a=08&b=15&c=2014&d=08&e=15&f=2015&g=d&ignore=.csv"), header = TRUE, sep = ","),
           "LNKD" = read.csv(url("http://real-chart.finance.yahoo.com/table.csv?s=LNKD&a=08&b=15&c=2014&d=08&e=15&f=2015&g=d&ignore=.csv"), header = TRUE, sep = ","),
           "MSFT" = read.csv(url("http://real-chart.finance.yahoo.com/table.csv?s=MSFT&a=08&b=15&c=2014&d=08&e=15&f=2015&g=d&ignore=.csv"), header = TRUE, sep = ","),
           "CSCO" = read.csv(url("http://real-chart.finance.yahoo.com/table.csv?s=CSCO&a=08&b=15&c=2014&d=08&e=15&f=2015&g=d&ignore=.csv"), header = TRUE, sep = ","),
           "MU" = read.csv(url("http://real-chart.finance.yahoo.com/table.csv?s=MU&a=08&b=15&c=2014&d=08&e=15&f=2015&g=d&ignore=.csv"), header = TRUE, sep = ",")
           )
  })
  
  output$hist <- renderPlot({ 
    ST = datasetInput()
    colu1 <- length(ST$Close)
    
    x    <- ST$Close# Old Faithful Geyser data
    #bins <- seq(1, 253, length.out = input$bins )
    bins <- input$bins
    
    stock <-c()
    
    for (i in 1:colu1){
      
      value1<- log(ST$Close[i]/ST$Close[i+1]) 
      stock<- c(value1,stock)}
      #hist (stock, breaks = bins)
    
      hist (stock, breaks = bins)
  })


  
  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$stocks, input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(datasetInput(), file, sep = sep,
                  row.names = FALSE)
    }
  )
})