library(shiny)
library(ggplot2)
library(readr)
library(fpp3)
library(tidyverse)
library(shinyjs)
library(fabletools)
library(fable.prophet)
library(shinythemes)


# Load the dataset
# data <- read_csv("mortality_canada.csv")
# data<-data |>
#   mutate(Date=as.Date(Date),
#          week=yearweek(week))
# data$Date <- as.Date(data$Date)
# data_ts<-data |>
#   as_tsibble(index=week)


forcast_choices <- c(
  "Mean"="MEAN",
  "Naive"="NAIVE",
  "Seasonal Naive"="SNAIVE",
  "ETS"="ETS",
  "ARIMA"="ARIMA",
  "Neural Network Model"="NNETAR",
  "Prophet Model"="prophet"
)
reg_choices<-c(
  "Simple Regression Model"="TSLM",
  "Dynamic regression model"="ARIMA"
)
# Combine for time series selection
# variable_labels <- c(outcome_choices, covariate_choices)
variable_labels <- list(
  "Mortality Rate"="mor_rate",
  "Tempreture"="tem_CA",
  "Precipitation"="pre_CA",
  "Influenza A positive tests"="FluApos",
  "Influenza B positive tests"="FluBpos",
  "Influenza A + B positive tests"="FluPos",
  "Total Test"="FluTest",
  "Respiratory Syncytial Virus (RSV) tests"="RSVtest",
  "Respiratory Syncytial Virus (RSV) positive tests"="RSVpos",
  "Percentage of hits regrading flu in google search"="hits_CA",
  "Total number of hits regrading flu in google search"="GFT_Canada"
)

ui <- fluidPage(
  theme = shinytheme("superhero"),
  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
  useShinyjs(),
  
  # Flex container for full-page layout
  tags$div(
    
    style = "display: flex; flex-direction: column; min-height: 100vh;",
    # Main content area (scrollable)
    tags$div(
      style = "flex-grow: 1; overflow-y: auto; padding-bottom: 100px;",
      tags$div(
        style = "display: flex; justify-content: space-between; align-items: center;
             padding: 10px 20px;",
        
        tags$img(src = "usask_usask_colour.png", height = "60px"),
        tags$img(src = "usask_medicine_CHE_colour.png", height = "60px")
      ),
      # Your full UI content here (titlePanel, sidebarLayout, etc.)
      titlePanel(""),
      sidebarLayout(
        sidebarPanel(
          selectInput("datasetChoice", "Select Dataset:",
                      choices = c("Select Dataset" = "",
                                  "Weekly FluWatch Data in Canada" = "flu", 
                                  "Weekly Mortality Data in Canada" = "mortality",
                                  "Weekly Mortality Data in Saskatchewan" = "mortality_SK")),
          
          conditionalPanel( condition = "input.datasetChoice != null && input.datasetChoice != ''",
                            selectInput("plotType", "Choose Analysis Type:",
                                        choices = c("Choose Analysis Type"="",
                                                    "Time Series Plot" = "ts",
                                                    "Scatter Plot" = "scatter",
                                                    "Correlation matrix" = "corr",
                                                    "Forecasting methods"="Forecast",
                                                    "Rgression Model"="reg"),
                                        # selected = "ts"
                                        
                            )),  # Allow multiple selections
          # For time series plot
          conditionalPanel(
            condition = "input.plotType == 'ts'",
            selectInput("variable", "Select Variables to Plot:",
                        choices = NULL,
                        multiple = TRUE,
                        # selected = "mor_rate"
            )
          ),
          # For scatter plot
          conditionalPanel(
            condition = "input.plotType == 'scatter'",
            selectInput("outcomeVar", "Select Outcome Variable:",
                        choices = NULL),
            selectInput("covariateVar", "Select Covariate:",
                        choices = NULL)
          ),
          conditionalPanel(
            condition = "input.plotType == 'corr'",
            selectInput("CorrVar", "Select Variables to Plot:",
                        choices = NULL,
                        multiple = TRUE)
          ),
          conditionalPanel(
            condition = "input.plotType == 'Forecast'",
            selectInput("forcastvar", "Select Outcome Variable:",
                        choices = NULL),
            selectInput("forcastchoise", "Forecasting methods:",
                        choices = forcast_choices,
                        selected = "MEAN"),
            numericInput("forecastHorizon", 
                         "Forecast Horizon (weeks):", 
                         value = 12, min = 1, max = 52)
          ),
          conditionalPanel(
            condition = "input.plotType == 'reg'",
            selectInput("regoutcome", "Select Outcome Variable:",
                        choices = NULL),
            selectInput("regcovariate", "Select Covariate:",
                        choices = NULL,
                        multiple = TRUE),
            selectInput("regchoise", "Select Model:",
                        choices = reg_choices,
                        selected = ""),
            numericInput("regHorizon", 
                         "Forecast Horizon (weeks):", 
                         value = 12, min = 1, max = 52)
          ),
          conditionalPanel(
            condition = "input.datasetChoice != null && input.datasetChoice != ''",
            uiOutput("dateRangeUI")
          )
          
          # dateRangeInput("dateRange", "Select Date Range:",
          #                start = min(data$week),
          #                end = max(data$week),
          #                format = "yyyy-mm-dd")
        ),
        mainPanel(
          
          
          tags$div(
            style = "text-align: center; margin-bottom: 20px;",
            tags$h2("Time Series Analysis in Health Research"),
            tags$h4(textOutput("datasetTitle"))
          ),
          # 
          # tableOutput("dataPreview"),
          # verbatimTextOutput("selectedVars"),
          actionButton("run", "Run"),
          conditionalPanel(
            condition = "input.plotType == 'ts'",
            plotOutput("timeSeriesPlot")
          ),
          conditionalPanel(
            condition = "input.plotType == 'scatter'",
            plotOutput("scatterPlot")
          ),
          conditionalPanel(
            condition = "input.plotType == 'corr'",
            plotOutput("corrPlot")
          ),
          conditionalPanel(
            condition = "input.plotType == 'Forecast'",
            # tags$h4("Model Details"),
            tableOutput("ModelDetails"),
            tags$h4("Residual Plot"),
            plotOutput("ResidualPlot"),
            tags$h4("Residual Test"),
            tableOutput("ResidualTest"),
            tags$h4("Forecast Plot"),
            plotOutput("ForecastPlot"),
          ),
          conditionalPanel(
            condition = "input.plotType == 'reg'",
            # tags$h4("Model Details"),
            tableOutput("ModelDetails"),
            tags$h4("Residual Plot"),
            plotOutput("ResidualPlotreg"),
            tags$h4("Residual Test"),
            tableOutput("ResidualTestreg"),
            tags$h4("Fitted Plot"),
            plotOutput("fittedPlot"),
          ),
          # Logos at bottom left and right
          
        )
      )
    ),
    
    # Footer (not fixed, stays at bottom)
    tags$div(
      style = "text-align: center; background-color: #343a40; color: white; 
               padding: 10px; font-size: 14px; border-top: 1px solid #ccc;",
      HTML("&copy; 2025 | Assistant Professor: Dr. Erfan Hoque  
| Student: Syed Jafar Raza Rizvi |
           Department of Community Health and Epidemiology |
           University of Saskatchewan")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  output$datasetTitle <- renderText({
    switch(input$datasetChoice,
           "flu" = "Weekly FluWatch Data in Canada",
           "mortality" = "Weekly Mortality Data in Canada",
           "mortality_SK" = "Weekly Mortality Data in Saskatchewan",
           "")
  })
  
  # Reactive expression to load dataset based on selection
  selectedData <- reactive({
    req(input$datasetChoice)  # Ensures input is not NULL or empty
    
    switch(input$datasetChoice,
           "mortality" =  read_csv("mortality_canada.csv"),
           "flu"=  read_csv("flu_model.csv"),
           "mortality_SK" =  read_csv("mortality_SK.csv"))
  })
  output$dateRangeUI <- renderUI({
    req(selectedData())
    
    df <- selectedData()
    df<-df |>
      mutate(Date=as.Date(Date),
             week=yearweek(week))
    
    # Ensure 'week' is in Date format
    if (!inherits(df$week, "Date")) {
      df$week <- as.Date(df$week)
    }
    
    dateRangeInput("dateRange", "Select Date Range:",
                   start = min(df$week, na.rm = TRUE),
                   end = max(df$week, na.rm = TRUE),
                   format = "yyyy-mm-dd")
  })
  # Update variable list excluding date columns
  observeEvent(selectedData(), {
    df <- selectedData()
    # Labeling the variable 
    variable_labels <- list(
      mor_rate = "Mortality Rate",
      tem_CA = "Tempreture",
      pre_CA = "Precipitation",
      FluApos="Influenza A positive tests",
      FluBpos="Influenza B positive tests",
      FluPos="Influenza A + B positive tests",
      FluTest="Total Test",
      RSVtest="Respiratory Syncytial Virus (RSV) tests",
      RSVpos="Respiratory Syncytial Virus (RSV) positive tests",
      hits_CA="Percentage of hits regrading flu in google search",
      GFT_Canada="Total number of hits regrading flu in google search"
    )
    
    # Exclude date columns
    non_date_vars <- names(df)[!sapply(df, inherits, what = "Date")
                               & names(df) != "week"
                               ]
    # Apply labels only to non-date variables
    labeled_vars <- setNames(non_date_vars, sapply(non_date_vars, function(var) {
      variable_labels[[var]] %||% var  # Use label if available, else fallback to var name
    }))
    
    updateSelectInput(session, "variable",
                      choices = labeled_vars,
                      selected = NULL)
    updateSelectInput(session, "outcomeVar",
                      choices = labeled_vars,
                      selected = NULL)
    updateSelectInput(session, "covariateVar",
                      choices = labeled_vars,
                      selected = NULL)
    updateSelectInput(session, "CorrVar",
                      choices = labeled_vars,
                      selected = NULL)
    updateSelectInput(session, "forcastvar",
                      choices = labeled_vars,
                      selected = NULL)
    updateSelectInput(session, "regoutcome",
                      choices = labeled_vars,
                      selected = NULL)
    updateSelectInput(session, "regcovariate",
                      choices = labeled_vars,
                      selected = NULL)
    
  })
  selectedVariables <- reactive({
    req(input$variable)
    input$variable
  })
  selectedOutcomeVar <- reactive({
    req(input$outcomeVar)
    input$outcomeVar
  })
  selectedcovariateVar <- reactive({
    req(input$covariateVar)
    input$covariateVar
  })
  selectedCorrVar <- reactive({
    req(input$CorrVar)
    input$CorrVar
  })
  selectedforcastvar <- reactive({
    req(input$forcastvar)
    input$forcastvar
  })
  selectedregoutcome<- reactive({
    req(input$regoutcome)
    input$regoutcome
  })
  selectedregcovariate<- reactive({
    req(input$regcovariate)
    input$regcovariate
  })
  # output$selectedVars <- renderPrint({
  #   selectedforcastvar()
  # })
  # 
  # 1. Create the reactive trigger
  run_trigger <- reactiveVal(0)
  
  # 2. Reset trigger when inputs change
  observeEvent({
    input$plotType
    input$forcastvar
    input$outcomeVar
    input$covariateVar
    input$dateRange
    input$variable
    input$CorrVar
    input$forcastchoise
    input$regoutcome
    input$regcovariate
  }, {
    run_trigger(0)
  })
  
  # 3. Increment trigger when Run button is clicked
  observeEvent(input$run, {
    run_trigger(run_trigger() + 1)
  })
  # Display the dataset
  # output$dataPreview <- renderTable({
  #   selectedData()
  # })
  output$timeSeriesPlot <- renderPlot({
    req(run_trigger()>0 & length(input$variable)>0)  # Ensure at least one variable is selected
    data<-selectedData()
    data<-data |>
      mutate(Date=as.Date(Date),
             week=yearweek(week))
    data_ts<-data |>
      as_tsibble(index=week)
    
    filtered_ts <- subset(data_ts, week >= input$dateRange[1] & 
                            week <= input$dateRange[2])
    # print(filtered_ts)
    # print(all_of(c(selectedVariables(), "week")))
    
    filtered_ts <- filtered_ts |>
      select(all_of(c(selectedVariables(), "week"))) |>
      pivot_longer(-week)
    # print(filtered_ts)
    
      filtered_ts$name <- names(variable_labels)[match(filtered_ts$name, variable_labels)]

      ggplot(filtered_ts,aes(week, value, colour = name)) +
      geom_line() +
      facet_grid(name ~ ., scales = "free_y") +
      guides(colour = "none") +
      labs(y=" ")
  })
  output$scatterPlot <- renderPlot({
    req(run_trigger()>0 & input$plotType == "scatter", 
        length(selectedOutcomeVar())>0, length(selectedcovariateVar())>0)
    
    data<-selectedData()
    data<-data |>
      mutate(Date=as.Date(Date),
             week=yearweek(week))
    data_ts<-data |>
      as_tsibble(index=week)
    
    filtered_ts <- subset(data_ts, week >= input$dateRange[1] 
                          & week <= input$dateRange[2])
    
    
    ggplot(filtered_ts, aes_string(x = selectedcovariateVar(), 
                                   y = selectedOutcomeVar())) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(x = names(variable_labels)[variable_labels == selectedcovariateVar()],
           y = names(variable_labels)[variable_labels == selectedOutcomeVar()],
           title = paste("Scatter Plot of",
                         names(variable_labels)[variable_labels == selectedOutcomeVar()],
                         "vs",
                         names(variable_labels)[variable_labels == selectedcovariateVar()])) +
      theme_minimal()
  })
  output$corrPlot <- renderPlot({
    req(run_trigger()>0 & length(selectedCorrVar()) > 1)  # Ensure at least two variables are selected
    
    data<-selectedData()
    data<-data |>
      mutate(Date=as.Date(Date),
             week=yearweek(week))
    data_ts<-data |>
      as_tsibble(index=week)
    # Filter by date range
    filtered_ts <- subset(data, 
                          week >= input$dateRange[1] & 
                            week <= input$dateRange[2])
    filtered_ts <- filtered_ts |>
      select(-week)
    # print(filtered_ts)
    # Select only the variables chosen by the user
    filtered_ts <- filtered_ts |> select(all_of(selectedCorrVar()))
    
    colnames(filtered_ts) <- names(variable_labels)[match(colnames(filtered_ts), 
                                                          variable_labels)]
    
    # Now use this filtered data for correlation plot
    GGally::ggpairs(filtered_ts) +
      labs(title="Correlation Matrix")
  })
    filtered_ts_reactive <- reactive({
      req(run_trigger() > 0, input$plotType == "Forecast", length(selectedforcastvar()) > 0)
      # print(selectedforcastvar())
      data<-selectedData()
      data<-data |>
        mutate(Date=as.Date(Date),
               week=yearweek(week))
      data_ts<-data |>
        as_tsibble(index=week)
      filtered_ts <- subset(data_ts,
                            week >= input$dateRange[1] &
                              week <= input$dateRange[2])
      filtered_ts <- filtered_ts |> select(all_of(selectedforcastvar()))
      filtered_ts
    })
    fit_model_reactive <- reactive({
      req(filtered_ts_reactive())
      filtered_ts <- filtered_ts_reactive()
      fit_model <- filtered_ts |>
        model(
          model = switch(input$forcastchoise,
                         "MEAN" = MEAN(!!sym(selectedforcastvar())),
                         "NAIVE" = NAIVE(!!sym(selectedforcastvar())),
                         "SNAIVE" = SNAIVE(!!sym(selectedforcastvar()) ~ lag()),
                         "ETS" = ETS(!!sym(selectedforcastvar())),
                         "ARIMA" = ARIMA(!!sym(selectedforcastvar())),
                         "NNETAR" = NNETAR(!!sym(selectedforcastvar())),
                         "prophet" = prophet(!!sym(selectedforcastvar())~season(period = 52, order = 10))
          )
        )
      fit_model
    })
    incProgress(0.8, detail = "Rendaring residual plot....")
    output$ResidualPlot <- renderPlot({
      req(fit_model_reactive())
      fit_model_reactive() |> gg_tsresiduals(lag=52)
    })
    incProgress(0.9, detail = "rendarign residual test....")
    output$ResidualTest <- renderTable({
      req(fit_model_reactive())
      augment(fit_model_reactive()) |>
        features(.innov, ljung_box,lag = 52)
      
    })
    incProgress(0.9, detail = "genrating forecast plot....")
    
    output$ForecastPlot <- renderPlot({
      req(fit_model_reactive())
      
      # Generate forecast
      # print(input$forecastHorizon)
      fc <- forecast(fit_model_reactive(), h = input$forecastHorizon)  # `h` is forecast horizon
      # print(fc)
      # Plot forecast
      # autoplot(fc)
      start_week <- as.character(yearweek(min(filtered_ts_reactive()$week)))
      fc |>
        autoplot(level = NULL) +
        autolayer(
          filter_index(filtered_ts_reactive(), start_week ~ .),
          colour = "black"
        ) +
        labs(
          y = "Age-standardized mortality rates per 100,000",
          title = "Forecasts for weekly mortality rate in Canada"
        ) +
        guides(colour = guide_legend(title = "Forecast"))
    })
    
    filtered_ts_reactive_reg <- reactive({
      req(run_trigger() > 0, input$plotType == "reg", 
          length(selectedregoutcome()) > 0, 
          length(selectedregcovariate())>0)
      # print(selectedforcastvar())
      data<-selectedData()
      data<-data |>
        mutate(Date=as.Date(Date),
               week=yearweek(week))
      data_ts<-data |>
        as_tsibble(index=week)
      filtered_ts <- subset(data_ts,
                            week >= input$dateRange[1] &
                              week <= input$dateRange[2])
      
      filtered_ts <- filtered_ts |>
        select(all_of(c(selectedregoutcome(), selectedregcovariate())))
      
      filtered_ts
    })
    fit_model_reactive_reg <- reactive({
      req(filtered_ts_reactive_reg())
      filtered_ts <- filtered_ts_reactive_reg()
      
      formula_str <- paste(selectedregoutcome(), "~", 
                           paste(c(selectedregcovariate(), 
                                   "trend()", 
                                   "season()"), 
                                 collapse = " + "))
      model_formula <- as.formula(formula_str)
      # print(model_formula)
      
      fit_model <- filtered_ts |>
        model(
          model = switch(input$regchoise,
                         "TSLM" = TSLM(model_formula),
                         "ARIMA" = ARIMA(model_formula)
          )
        )
      fit_model
    })
    
    output$ResidualPlotreg <- renderPlot({
      req(fit_model_reactive_reg())
      fit_model_reactive_reg() |> gg_tsresiduals(lag=52)
      
    })
    output$ResidualTestreg<-renderTable({
      req(fit_model_reactive_reg())
      augment(fit_model_reactive_reg()) |>
        features(.innov, ljung_box,lag = 52)
    })
    output$fittedPlot <- renderPlot({
      req(fit_model_reactive_reg())
      withProgress(message = "Generating fitted plot...", value = 0, {
        augment(fit_model_reactive_reg()) |>
          ggplot(aes(x = week)) +
          geom_line(aes(y = !!sym(selectedregoutcome()), colour = "Data")) +
          geom_line(aes(y = .fitted, colour = "Fitted")) +
          labs(y = NULL,
               title = "Weekly Mortality rate in Canada"
          ) +
          scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
          guides(colour = guide_legend(title = NULL))
      })
      # t<-(augment(fit_model_reactive_reg()))
    })
}

# Run the app
shinyApp(ui = ui, server = server)
