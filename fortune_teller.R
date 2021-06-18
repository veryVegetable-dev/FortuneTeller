library(shiny)
library(rpart.plot)


fields <- c("date", "period", "sleeping", "resting_heart_rate", "task", "duration", "detail", "luck", "fixed_luck")
num_fields_before_fix = length(fields)-1
num_fields_input = length(fields)-2


saveData <- function(data, teller_model, logPath) {
  # parse sample 
  curr_line = t(data)
  test_data = data.frame(curr_line)
  names(test_data) = fields[1:num_fields_input]
  test_data$period = as.numeric(as.character(test_data$period))
  test_data$sleeping = as.numeric(as.character(test_data$sleeping))
  test_data$resting_heart_rate = as.numeric(as.character(test_data$resting_heart_rate))
  test_data$task = factor(test_data$task)
  test_data$duration = as.numeric(as.character(test_data$duration))
  test_data$luck = data.frame(predict(teller_model, newdata=test_data, type="prob"))[, 2]
  write(x = as.vector(t(test_data)), ncolumns = num_fields_before_fix, sep = "\t", file = logPath, append = TRUE)
}

fixLuck <- function(data, logPath) {
  predData = read.delim(logPath)
  predData$fixed_luck = t(strsplit(data, ","))[[1]]
  names(predData) = fields
  file.remove(logPath)
  file.create(logPath)
  write.table(predData, logPath, quote = FALSE, row.names = FALSE, sep = "\t")
}


loadData <- function(logPath) {
  data <- read.delim(logPath)
  data[order(data$date, decreasing = TRUE), ]
}


shinyApp(
  
  ui = fluidPage(
    DT::dataTableOutput("responses", width = 1300), tags$hr(),
    plotOutput("tree"), 
    textInput(fields[1], "date:", ""),
    textInput(fields[2], "period:", ""),
    textInput(fields[3], "sleep hrs last night:", ""),
    textInput(fields[4], "resting heart rate:", ""), 
    selectInput(fields[5], "task:", choices = c("job", "work_out", "housework", "outdoor")), 
    textInput(fields[6], "duration:", ""), 
    textInput(fields[7], "detail:", ""), 
    actionButton("submit_date_info", "Bless Me"), 
  ),
  
  server = function(input, output, session) {

    predPath = "pred"
    
    # load rpart model 
    teller_model = readRDS("teller.model")
    
    # plot the tree 
    output$tree <- renderPlot({
      rpart.plot(teller_model)
    })
    
    # get input data and make prediction
    formData <- reactive({
      data <- sapply(fields[1:num_fields_input], function(x) input[[x]])
      data
    })
    observeEvent(input$submit_date_info, {
      saveData(formData(), teller_model, predPath)
    })
    
    # fix luck after today 
    observeEvent(input$submit_fix, {
      fixLuck(reactive({input$fix_luck})(), predPath)
    })

    # return dataTable 
    output$responses <- DT::renderDataTable({
      input$submit_date_info
      input$submit_fix
      loadData(predPath)
    })
  }
)