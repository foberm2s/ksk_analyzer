#library(shiny)
library(magrittr)
library(lubridate)

  
  # Define UI for data upload app ----
  ui <- fluidPage(
    
    # App title ----
    titlePanel("Analysetool für Bankingdaten"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        # Input: Select bank ----
        radioButtons("bank", "1. Choose your Bank",
                     choices = c(Sparkasse = "ksk",
                                 N26 = "n26")
        ),
        # Input: Select a file ----
        fileInput("file1","2. Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        # Horizontal line ----
        tags$hr(),
        

        
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
        # Input: EXCLUDE for beneficiary
        checkboxInput("exclBen", "Exclude", value = FALSE, width = NULL),
        
        # Input: Enter a subject
        textInput(
          "subject",
          "Subject",
          value= "",
          width = NULL,
          placeholder = NULL
        ),
        # Input: EXCLUDE for subject
        checkboxInput("exclSub", "Exclude", value = FALSE, width = NULL),
        
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        # Output: Data file ----
        htmlOutput("ausgaben"),
        htmlOutput("einnahmen"),
        tableOutput("contents")
        
      )
      
      
    )
  )
  
  calcDf = function(df, beneficiary, subject, dateRange, bank, exclBen, exclSub){
    df = determineBankColumns(df, bank);
    if(beneficiary != ""){
      exclude = exclBen;
      if(exclude) df = subset(df, !grepl(tolower(beneficiary), tolower(Beguenstigter.Zahlungspflichtiger))) else df = subset(df, grepl(tolower(beneficiary), tolower(Beguenstigter.Zahlungspflichtiger)))
    }
    if(subject != ""){
      exclude = exclSub;
      if(exclude)df = subset(df, !grepl(tolower(subject), tolower(Verwendungszweck))) else df = subset(df, grepl(tolower(subject), tolower(Verwendungszweck)))
    }
    dates = as.Date(dateRange, '%d.%m.%y')
    if (bank == "ksk"){
      df$Buchungstag = as.Date(df$Buchungstag, '%d.%m.%y');
      
    } else {
      df$Buchungstag = as.Date(df$Buchungstag);
    }
    df = subset(df, Buchungstag >= dates[1] & Buchungstag <= dates[2])
    df$Buchungstag = format(df$Buchungstag, '%d.%m.%y');
    return (df);
  }
  
  
  determineBankColumns = function(df, bank){
    if (bank == "n26"){
      colnames(df) = c("Buchungstag", "Beguenstigter.Zahlungspflichtiger", "Kontonummer", "Transaktionstyp", "Verwendungszweck", "Kategorie", "Betrag", "Betrag (F)", "Fremdwaehrung", "Wechselkurs");
      return(df);
    } else if (bank == "ksk") {
      df = subset(df, select = -c(Auftragskonto, Valutadatum, Glaeubiger.ID, Mandatsreferenz, 
                                  Kundenreferenz..End.to.End., Sammlerreferenz, Lastschrift.Ursprungsbetrag,
                                  Auslagenersatz.Ruecklastschrift, Info))
      return(df);
    } else {
      return (NaN);
    }
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
  
  getAvgPerMonth = function(df, expOrInc, dateRange){
    # get first date in available data
    firstDate = as.Date(df$Buchungstag[1], "%d.%m.%y");
    # get last date in available data 
    lastDate = as.Date(df$Buchungstag[length(df$Buchungstag)], "%d.%m.%y");
    # get DateRange days to check if the given daterange is more specific than underlying data
    if (firstDate > lastDate){
      tmp = lastDate;
      lastDate = firstDate;
      firstDate = tmp;
    }
    if (dateRange[1] > firstDate){
      firstDate = dateRange[1];
    }
    if (lastDate > dateRange[2]){
      lastDate = dateRange[2];
    }
    # calculate amount of days in between
    days = lubridate::interval(firstDate, lastDate) %/% days(1)    # Apply interval & days
    if (days < 0){
      days = days*-1;
    }
    if (expOrInc == "expenses"){
      expenses = getExpenses(df);
      tmp = (expenses/days)*30;
      return(if (expenses > tmp) tmp else expenses);
    } else if (expOrInc == "income"){
      income = getIncome(df);
      tmp = (income/days)*30;
      return(if (income > tmp) tmp else income);
    } else {
      return ;
    }
  }
  
  getHighestPosition = function(df, expOrInc){
    if (expOrInc == "expenses"){
      my_df = subset(df, grepl('-', Betrag))
      options(digits=6)
      vals = as.double(gsub(",", ".",substring(my_df$Betrag, 2)))
    } else {
      my_df = subset(df, !grepl('-', Betrag))
      options(digits=6)
      vals = as.double(gsub(",", ".",my_df$Betrag))
    }
    print(vals);
    return(max(vals));
    
  }
  
  # Define server logic to read selected file ----
  server <- function(input, output) {
    dfR <- eventReactive(input$file1, {read.csv(input$file1$datapath,
                                                header = TRUE,
                                                sep = if(input$bank == "ksk") ";" else ",",
                                                quote = '"')})
    
    output$contents <- renderTable({
      req(input$file1)
      df = dfR()
      df = calcDf(df, input$beneficiary, input$subject, input$dateRange, input$bank, input$exclBen, input$exclSub);
      if(input$disp == "head") {
        return(head(df))
      }
      else {
        return(df)
      }
    })

    
    output$ausgaben = renderText({
      
      my_df = dfR()
      my_df = calcDf(my_df, input$beneficiary, input$subject, input$dateRange, input$bank, input$exclBen, input$exclSub);
      expenses = getExpenses(my_df);
      max = getHighestPosition(my_df, "expenses");
      paste("<h3><font color=\"red\"><b>", "Expenses: ", "</b></font><b>", expenses, "€</b></h3> <h4>Monthly avg: ", round(getAvgPerMonth(my_df, "expenses", input$dateRange), 2), "€</h4><h4>", "Largest Expense: ", max, "€</h4")
      
      
    })

    
    output$einnahmen = renderText({
      my_df = dfR()
      my_df = calcDf(my_df, input$beneficiary, input$subject, input$dateRange, input$bank, input$exclBen, input$exclSub);
      income = getIncome(my_df)
      max = getHighestPosition(my_df, "income");
      paste("<h3><font color=\"green\"><b>", "Income: ", "</b></font><b>", income, "€</b></h3> <h4>Monthly avg: ", round(getAvgPerMonth(my_df, "income", input$dateRange), 2), "€</h4><h4>", "Biggest income: ", max, "€</h4>")
    })

    

    }
    
  
  
  # Run the app ----
  shinyApp(ui, server)

