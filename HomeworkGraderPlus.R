# HomeworkGraderPlus: A Comprehensive Shiny App for Grading
# This versatile Shiny application is designed to streamline the grading process, 
# making it simpler and more efficient for educators and teaching assistants. 
# With a focus on flexibility and ease of use, HomeworkGraderPlus offers a suite of features:

## 1. Group Submission Support: Accommodates up to 4 students per group submission, 
## recording their scores and automatically identifying the first student's ID as the primary submitter.

## 2. Submission Type Tracking: Automatically distinguishes between individual and group submissions, 
## providing quick insights into submission patterns and average grades.

## 3. Persistent Grade Storage: Grades are saved locally in a CSV file, ensuring data is retained 
## even if the application is restarted, facilitating a seamless grading experience.

## 4. Excel Export: Enables the easy export of grades into a Microsoft Excel sheet, 
## allowing for further analysis or record-keeping outside the app.

## 5. Time Tracking for Grading: Introduces the ability to track and cache the time spent on grading tasks, 
## helping educators manage their workload more effectively.

library(shiny)
library(writexl)
library(shinythemes)

ui <- fluidPage(
    theme = shinytheme("superhero"),
    tags$head(
      tags$style(HTML("
    /* Target only the input fields directly */
    .main-content .form-control { 
      background-color: #607D8B; 
      color: #EEE; 
    }

    /* For text output, you might need to adjust or remove this part depending on your exact needs */
    .main-content .shiny-text-output { 
      background-color: #607D8B;
      color: #EEE; 
    }
  "))
  ),

    titlePanel("HomeworkGraderPlus"),

    sidebarLayout(
      sidebarPanel(
        downloadButton('downloadData', 'Download Grades', style = "color: lightgreen;"),
        actionButton("start_timer", "Start Timer"),
        actionButton("stop_timer", "Record Time"),
        p(id = "scenarioRuntime", tags$label(class = "minutes"), tags$label(class = "seconds")),
        tags$script(HTML(
          '
        $(function(){
            var timer;
            
            Shiny.addCustomMessageHandler("timer", function(data){
                if(data.event === "end") return clearInterval(timer);
                
                var minutesLabel = document.querySelector(`#${data.id} .minutes`);
                var secondsLabel = document.querySelector(`#${data.id} .seconds`);
                var totalSeconds = 0;

                function pad(val) {
                  var valString = val + "";
                  if (valString.length < 2) {
                    return "0" + valString;
                  } else {
                    return valString;
                  }
                }
                function setTime() {
                  ++totalSeconds;
                  secondsLabel.innerHTML = pad(totalSeconds % 60);
                  minutesLabel.innerHTML = `${pad(parseInt(totalSeconds / 60))} : `;
                }
                
                timer = setInterval(setTime, 1000);
            });
        });
        '
        )),
        textOutput("time_elapsed"),
        HTML("<div style='margin-top: 20px;'><strong>Author:</strong> Yichen Han<br><strong>Affiliation:</strong> Institut für Statistik, LMU</div>"),
        actionButton("reset", "Start Over", style = "color: orange;")
      ),
      
      mainPanel(
        div(class = "main-content",
            textInput("mat1", "Matrikelnummer 1"),
            textInput("mat2", "Matrikelnummer 2"),
            textInput("mat3", "Matrikelnummer 3"),
            textInput("mat4", "Matrikelnummer 4"),
            numericInput("note", "Note", value = 0, min = 0, max = 100),
            actionButton("submit", "Submit"),
            actionButton("undo", "Undo Last Submission"),
            tableOutput("record_table"),
            verbatimTextOutput("meanNote"),
            verbatimTextOutput("single_count"),
            verbatimTextOutput("group_count")
        )
      )
    )
)


server <- function(input, output, session) {
  timerStart <- reactiveVal(NULL)
  timerStop <- reactiveVal(NULL)
  counts <- reactiveValues(single = 0, group = 0)
  # initialize by reading cache
  if(file.exists('records.csv')) {
    records <- reactiveVal(read.csv('records.csv', stringsAsFactors = FALSE))
  } else {
    records <- reactiveVal(data.frame(Matrikelnummer = character(), Note = numeric(), Abgeber = character(), stringsAsFactors = FALSE))
  }
  
  if (file.exists("counts.rds")) {
    savedCounts <- readRDS("counts.rds")
    counts$single <- savedCounts$single
    counts$group <- savedCounts$group
  } else {
    counts$single <- 0  # Initialize to 0 if file does not exist
    counts$group <- 0
  }
  
  # Displaying the counts
  output$single_count <- renderText({
    paste("Anzahl der Enzelabgaben:", counts$single)
  })
  
  output$group_count <- renderText({
    paste("Anzahl der Gruppenabgaben:", counts$group)
  })
  
  # Observe the reset button
  observeEvent(input$reset, {
    # Reset the records to an empty data frame
    records(data.frame(Matrikelnummer = character(), Note = numeric(), Abgeber = character(), stringsAsFactors = FALSE))
    
    # delete the cached files
    if(file.exists('records.csv')) {
      file.remove('records.csv')
    }
    if(file.exists('timer.rds')) {
      file.remove('timer.rds')
    }
    if(file.exists('stop_time.rds')) {
      file.remove('stop_time.rds')
    }
    if(file.exists('total_elapsed.rds')) {
      file.remove('total_elapsed.rds')
    }
    if(file.exists('counts.rds')) {
      file.remove('counts.rds')
    }
    
    output$record_table <- renderTable({
      records()
    })
    session$reload()
  })
  
  observeEvent(input$submit, {
    new_records <- data.frame(
      Matrikelnummer = c(input$mat1, input$mat2, input$mat3, input$mat4),
      Note = rep(input$note, 4),
      Abgeber = c("*", NA, NA, NA),
      stringsAsFactors = FALSE
    )
    # Remove the rows where Matrikelnummer is empty
    new_records <- new_records[new_records$Matrikelnummer != "",]
    # Combine the new records with the existing ones
    records(rbind(records(), new_records))
    write.csv(records(), 'records.csv', row.names = FALSE)
    
    matInputs <- c(input$mat2, input$mat3, input$mat4)
    
    if (input$mat1 != "" && input$mat2 == "" && input$mat3 == "" && input$mat4 == "") {
      counts$single <- counts$single + 1
    } else {
      counts$group <- counts$group + 1
    }
    saveRDS(list(single = counts$single, group = counts$group), "counts.rds")
    
    # Clear the text inputs
    updateTextInput(session, "mat1", value = "")
    updateTextInput(session, "mat2", value = "")
    updateTextInput(session, "mat3", value = "")
    updateTextInput(session, "mat4", value = "")
  })
  
  observeEvent(input$undo, {
    current_records <- records()
    if (nrow(current_records) > 0) {
      records(current_records[-nrow(current_records),])
    }
  })
  
  output$record_table <- renderTable({
    records()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("grades-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      writexl::write_xlsx(records(), path = file)
    }
  )
  output$meanNote <- renderText({
    mean_notes <- mean(records()$Note, na.rm = TRUE)
    return(paste("Mean of current notes: ", round(mean_notes, 2)))
  })
  
  # Timer implementation
  # Load or initialize timer data
  
  if (file.exists("total_elapsed.rds")) {
    totalElapsed <- readRDS("total_elapsed.rds")
  } else {
    totalElapsed <- 0  # Initialize to 0 if file does not exist
  }
  
  # When starting the timer
  observeEvent(input$start_timer, {
  startTime <- Sys.time()
  timerStart(startTime)
  session$sendCustomMessage('timer', list(id = "scenarioRuntime", event = "start"))
  })
  
  # When stopping the timer
  observeEvent(input$stop_timer, {
    if (!is.null(timerStart())) {
      stopTime <- Sys.time()
      # Calculate the newly elapsed time
      newElapsed <- as.numeric(difftime(stopTime, timerStart(), units = "secs"))
      # Update total elapsed time
      totalElapsed <- totalElapsed + newElapsed
      saveRDS(totalElapsed, "total_elapsed.rds")
      # Reset the start time
      timerStart(NULL)
      saveRDS(NULL, "timer.rds")  # Indicate the timer is not currently running
    }
    session$reload()
  })
  
  # Dynamically update and display the timer
  output$time_elapsed <- renderText({
    if (!is.null(timerStart())) {
      # Calculate current elapsed time since last started plus any previously accumulated time
      currentElapsed <- as.numeric(difftime(Sys.time(), timerStart(), units = "secs")) + totalElapsed
      paste("Timer running: ", round(currentElapsed / 60, 2), "minutes")
    } else {
      # Display only the total elapsed time if the timer is not currently running
      paste("Timer not running.\nTotal time elapsed: ", round(totalElapsed / 60, 2), "minutes")
    }
  })
  
  observe({
    invalidateLater(60000, session)
  })
}

shinyApp(ui = ui, server = server)
