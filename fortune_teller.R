library(shiny)



saveData <- function(data, teller_model, logPath) {
  # parse sample 
  curr_line = t(data)
  test_data = data.frame(curr_line)
  fields = c("date", "period", "sleeping", "resting_heart_rate", "task", "duration")
  names(test_data) = fields
  test_data$period = as.numeric(test_data$period)
  test_data$sleeping = as.numeric(test_data$sleeping)
  test_data$resting_heart_rate = as.numeric(test_data$resting_heart_rate)
  test_data$task = factor(test_data$task)
  test_data$duration = as.numeric(test_data$duration)
  test_data$pred = data.frame(predict(teller_model, newdata=test_data, type="prob"))[, 2]
  write(x = as.vector(t(test_data)), ncolumns = length(fields)+1, sep = "\t", file = logPath, append = TRUE)
}

fixLuck <- function(data, logPath) {
  predData = read.delim(logPath)
  predData$fixLuck = t(strsplit(data, ","))[[1]]
  fixedFields = c("date", "period", "sleeping", "resting_heart_rate", "task", "duration", "luck", "fixed_luck")
  names(predData) = fixedFields
  file.remove(logPath)
  file.create(logPath)
  write.table(predData, logPath, quote = FALSE, row.names = FALSE, sep = "\t")
}


loadData <- function(logPath) {
  # Read all the files into a list
  data <- read.delim(logPath)
  data
}


# Define the fields we want to save from the form
fields <- c("date", "period", "sleeping", "resting_heart_rate", "task", "duration")

shinyApp(
  
  ui = fluidPage(
    DT::dataTableOutput("responses", width = 300), tags$hr(),
    textInput(fields[1], "date:", ""),
    textInput(fields[2], "period:", ""),
    textInput(fields[3], "sleep hrs last night:", ""),
    textInput(fields[4], "resting heart rate:", ""), 
    selectInput(fields[5], "task:", choices = c("job", "work_out", "housework", "outdoor")), 
    textInput(fields[6], "duration:", ""), 
    actionButton("submit_date_info", "Bless Me"), 
    textInput("fix_luck", "fix luck (sep with comma):", ""),
    actionButton("submit_fix", "Fix Luck")
  ),
  
  server = function(input, output, session) {
    
    # write pred res 
    predPath = paste(c("pred.", Sys.time()), collapse = "")
    resFields = c("date", "period", "sleeping", "resting_heart_rate", "task", "duration", "luck")
    file.create(predPath)
    write(x = resFields, ncolumns = length(resFields), sep = "\t", file = predPath, append = TRUE)
    
    # load rpart model 
    teller_model = readRDS("teller.model")
    
    # get input data and make prediction
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
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