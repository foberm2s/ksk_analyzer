library(shiny)
library(magrittr)
library(lubridate)

if (interactive()) {
  
  # Define UI for data upload app ----
  ui <- fluidPage(
    
    # App title ----
    titlePanel("Uploading Files"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        # Input: Select a file ----
        fileInput("file1", "Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        # Horizontal line ----
        tags$hr(),
        
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE)
        
        ,
        
        # Input: Select separator ----
        radioButtons("sep", "Separator",
                     choices = c(Semicolon = ";",
                                 Comma = ",",
                                 Tab = "\t"),
                     selected = ";"),
        
        # Input: Select quotes ----
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),
        
        # Horizontal line ----
        tags$hr(),
        
        # Input: Select number of rows to display ----
        radioButtons("disp", "Display",
                     choices = c(Head = "head",
                                 All = "all"),
                     selected = "head"),
        
        # Input: Date Range
        dateRangeInput(
          "dateRange",
          "Date range",
          start = "2018-03-01",
          end = "2021-03-31",
          min = "2018-03-01",
          max = "2021-03-31",
          format = "yyyy-mm-dd",
          startview = "month",
          weekstart = 0,
          language = "en",
          separator = " to ",
          width = NULL,
          autoclose = TRUE
        ),
        
        # Input: Enter a beneficiary
        textInput(
          "beneficiary",
          "Beneficiary",
          value= "",
          width = NULL,
          placeholder = NULL
        ),
        # Input: Enter a subject
        textInput(
          "subject",
          "Subject",
          value= "",
          width = NULL,
          placeholder = NULL
        ),
        
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        # Output: Data file ----
        h3(htmlOutput("ausgaben")),
        h3(htmlOutput("einnahmen")),
        htmlOutput("monthly_avg"),
        tableOutput("contents")
        
      )
      
    )
  )
  
  calcDf = function(df, beneficiary, subject, dateRange){
    if(beneficiary != ""){
      
      df = subset(df, grepl(tolower(beneficiary), tolower(Beguenstigter.Zahlungspflichtiger)) )
    }
    if(subject != ""){
      df = subset(df, grepl(tolower(subject), tolower(Verwendungszweck)) )
    }
    dates = as.Date(dateRange, '%d.%m.%y')
    df$Buchungstag = as.Date(df$Buchungstag, '%d.%m.%y')
    df = subset(df, Buchungstag >= dates[1] & Buchungstag <= dates[2])
    df$Buchungstag = format(df$Buchungstag, '%d.%m.%y')
    df = subset(df, select = -c(Auftragskonto, Valutadatum, Glaeubiger.ID, Mandatsreferenz, 
                                Kundenreferenz..End.to.End., Sammlerreferenz, Lastschrift.Ursprungsbetrag,
                                Auslagenersatz.Ruecklastschrift, Info))
    return (df);
  }
  
  getExpenses = function(df){
    my_df = subset(df, grepl('-', Betrag))
    options(digits=6)
    vals = as.double(gsub(",", ".",substring(my_df$Betrag, 2)))
    return (sum(vals));
    
  }
  
  getIncome = function(my_df){
    my_df = subset(my_df, !grepl('-', Betrag))
    options(digits=6)
    vals = as.double(gsub(",", ".",my_df$Betrag))
    return (sum(vals))
  }
  
  # Define server logic to read selected file ----
  server <- function(input, output) {
    dfR <- eventReactive(input$file1, {read.csv(input$file1$datapath,
                                                header = input$header,
                                                sep = input$sep,
                                                quote = input$quote)})
    
    output$contents <- renderTable({

      req(input$file1)
      df = dfR()
      df = calcDf(df, input$beneficiary, input$subject, input$dateRange);
      if(input$disp == "head") {
        return(head(df))
      }
      else {
        return(df)
      }
    })
    
    output$ausgaben = renderText({
      
      my_df = dfR()
      my_df = calcDf(my_df, input$beneficiary, input$subject, input$dateRange);
      expenses = getExpenses(my_df);
      paste("<font color=\"red\"><b>", "Ausgaben: ", "</b></font><b>", expenses, "€</b>")
      
      
    })
    
    output$einnahmen = renderText({
      my_df = dfR()
      my_df = calcDf(my_df, input$beneficiary, input$subject, input$dateRange);
      income = getIncome(my_df)
      paste("<font color=\"green\"><b>", "Einnahmen: ", "</b></font><b>", income, "€</b>")
    })
    
    output$monthly_avg = renderUI({
      my_df = dfR()
      my_df = calcDf(my_df, input$beneficiary, input$subject, input$dateRange);
      # calculate sum of expenses and income
      expenses = getExpenses(my_df);
      income = getIncome(my_df);
      # calculate amount of months in daterange 
      return(interval(input$dateRange[1], input$dateRange[2]) %/% months(1))    # Apply interval & months
      
      # divide sum of expenses/ income with amount of months
      
      
    })
    
  }
  
  
  # Run the app ----
  shinyApp(ui, server)
}
