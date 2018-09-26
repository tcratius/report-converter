library(shiny)
library(tidyverse)
library(readr)


ui <- fluidPage(
  titlePanel('Raider Report Converter'),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                   selected = ","),
      radioButtons("quote", "Quote",
                   choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                   selected = '"'),
      tags$hr(),
      radioButtons("disp", "Display",
                   choices = c(Head = "head", All = "all"),
                   selected = "head"),
      tags$hr(),
      downloadButton('downloadData', 'Download')),
    
    mainPanel(tableOutput("table")
    )
  )
)





server <- function(input, output) {
  
  datasetInput <- reactive({
    req(input$file1)
    student_data = read_csv(input$file1$datapath)
    tidy_data = student_data %>%
      filter(!is.na(`Student_Name`)) %>%
      select(-mk5, -mk6, -mk7, -mk8, -Mark9, -mk10, -mk11, -mk12, 
             -abs5, -abs6, -abs7, -abs8, -abs9, -abs10, -abs11, -abs12,
             -gpa5, -gpa6, -gpa7, -gpa8, -gpa9, -gpa10, -gpa11, -gpa12)
    
    
    col_names = c('Name', 'Gender', 'Grade', 'ELL', 'Migrant', '504', 'SPED',
                  'TAG', 'Diploma', 'SIS Number', 'Race',
                  
                  'Course','Teacher', 'Subject', 'Mark 1', 'Mark 2',
                  'Abs 1', 'Abs 2', 'Course', 'Teacher', 'Subject',
                  'Mark 3', 'Mark 4', 'Abs 3', 'Abs 4',
                  
                  'Course','Teacher', 'Subject', 'Mark 1', 'Mark 2',
                  'Abs 1', 'Abs 2', 'Course', 'Teacher', 'Subject',
                  'Mark 3', 'Mark 4', 'Abs 3', 'Abs 4',
                  
                  'Course','Teacher', 'Subject', 'Mark 1', 'Mark 2',
                  'Abs 1', 'Abs 2', 'Course', 'Teacher', 'Subject',
                  'Mark 3', 'Mark 4', 'Abs 3', 'Abs 4',
                  
                  'Course','Teacher', 'Subject', 'Mark 1', 'Mark 2',
                  'Abs 1', 'Abs 2', 'Course', 'Teacher', 'Subject',
                  'Mark 3', 'Mark 4', 'Abs 3', 'Abs 4',
                  
                  'Course','Teacher', 'Subject', 'Mark 1', 'Mark 2',
                  'Abs 1', 'Abs 2', 'Course', 'Teacher', 'Subject',
                  'Mark 3', 'Mark 4', 'Abs 3', 'Abs 4',
                  
                  'Course','Teacher', 'Subject', 'Mark 1', 'Mark 2',
                  'Abs 1', 'Abs 2', 'Course', 'Teacher', 'Subject',
                  'Mark 3', 'Mark 4', 'Abs 3', 'Abs 4',
                  
                  'Course','Teacher', 'Subject', 'Mark 1', 'Mark 2',
                  'Abs 1', 'Abs 2', 'Course', 'Teacher', 'Subject',
                  'Mark 3', 'Mark 4', 'Abs 3', 'Abs 4',
                  
                  'GPA 1', 'GPA 2', 'GPA 3', 'GPA 4',
                  
                  'AUG Att', 'SEP Att', 'OCT Att', 'NOV Att', 'DEC Att',
                  'JAN Att', 'FEB Att', 'MAR Att', 'APR Att', 'MAY Att',
                  'JUN Att', 'JUL Att', 'YTD ATT RT', 'Incident Total',
                  'ISS Total', 'ISS Days', 'OSS Total', 'OSS Days',
                  'Exp Total')
    
    final_df = data.frame(matrix(data=NA, nrow=0, ncol=length(col_names)))
    
    new_line = data.frame(matrix(data=NA, nrow=1, ncol=length(col_names)))
    new_line[1, c(1:11, 110:132)] = tidy_data[1, c(1:11, 23:45)]
    
    stu_id = tidy_data$sis_number[1]
    
    for (row_num in 1:nrow(tidy_data)) {
      row = tidy_data[row_num, ]
      
      if (stu_id == row[10]) {
        if (!is.na(row[15]) | !is.na(row[16])) {
          if (startsWith(row[[12]], '1')) {
            new_line[1, 12:18] = row[c(12:16, 19:20)]
          } else if (startsWith(row[[12]], '2')) {
            new_line[1, 26:32] = row[c(12:16, 19:20)]
          } else if (startsWith(row[[12]], '3')) {
            new_line[1, 40:46] = row[c(12:16, 19:20)]
          } else if (startsWith(row[[12]], '4')) {
            new_line[1, 54:60] = row[c(12:16, 19:20)]
          } else if (startsWith(row[[12]], '5')) {
            new_line[1, 68:74] = row[c(12:16, 19:20)]
          } else if (startsWith(row[[12]], '6')) {
            new_line[1, 82:88] = row[c(12:16, 19:20)]
          } else if (startsWith(row[[12]], '7')) {
            new_line[1, 96:102] = row[c(12:16, 19:20)]
          }
        } else if (!is.na(row[17]) | !is.na(row[18])) {
          if (startsWith(row[[12]], '1')) {
            new_line[1, 19:25] = row[c(12:14, 17:18, 21:22)]
          } else if (startsWith(row[[12]], '2')) {
            new_line[1, 33:39] = row[c(12:14, 17:18, 21:22)]
          } else if (startsWith(row[[12]], '3')) {
            new_line[1, 47:53] = row[c(12:14, 17:18, 21:22)]
          } else if (startsWith(row[[12]], '4')) {
            new_line[1, 61:67] = row[c(12:14, 17:18, 21:22)]
          } else if (startsWith(row[[12]], '5')) {
            new_line[1, 75:81] = row[c(12:14, 17:18, 21:22)]
          } else if (startsWith(row[[12]], '6')) {
            new_line[1, 89:95] = row[c(12:14, 17:18, 21:22)]
          } else if (startsWith(row[[12]], '7')) {
            new_line[1, 103:109] = row[c(12:14, 17:18, 21:22)]
          }
        }
      } else if (stu_id != row[10]) {
        stu_id = row[10]
        final_df = rbind(final_df, new_line)
        new_line = data.frame(matrix(data=NA, nrow=1, ncol=length(col_names)))
        new_line[1, c(1:11, 110:132)] = row[c(1:11, 23:45)]
        
        if (startsWith(row[[12]], '1')) {
          new_line[1, 12:18] = row[c(12:16, 19:20)]
        } else if (startsWith(row[[12]], '2')) {
          new_line[1, 26:32] = row[c(12:16, 19:20)]
        } else if (startsWith(row[[12]], '3')) {
          new_line[1, 40:46] = row[c(12:16, 19:20)]
        } else if (startsWith(row[[12]], '4')) {
          new_line[1, 54:60] = row[c(12:16, 19:20)]
        } else if (startsWith(row[[12]], '5')) {
          new_line[1, 68:74] = row[c(12:16, 19:20)]
        } else if (startsWith(row[[12]], '6')) {
          new_line[1, 82:88] = row[c(12:16, 19:20)]
        } else if (startsWith(row[[12]], '7')) {
          new_line[1, 96:102] = row[c(12:16, 19:20)]
        }
        
      } else if (!is.na(row[17]) | !is.na(row[18])) {
        if (startsWith(row[[12]], '1')) {
          new_line[1, 19:25] = row[c(12:14, 17:18, 21:22)]
        } else if (startsWith(row[[12]], '2')) {
          new_line[1, 33:39] = row[c(12:14, 17:18, 21:22)]
        } else if (startsWith(row[[12]], '3')) {
          new_line[1, 47:53] = row[c(12:14, 17:18, 21:22)]
        } else if (startsWith(row[[12]], '4')) {
          new_line[1, 61:67] = row[c(12:14, 17:18, 21:22)]
        } else if (startsWith(row[[12]], '5')) {
          new_line[1, 75:81] = row[c(12:14, 17:18, 21:22)]
        } else if (startsWith(row[[12]], '6')) {
          new_line[1, 89:95] = row[c(12:14, 17:18, 21:22)]
        } else if (startsWith(row[[12]], '7')) {
          new_line[1, 103:109] = row[c(12:14, 17:18, 21:22)]
        }
      } 
    }
    
    final_df = rbind(final_df, new_line)
    colnames(final_df) = col_names
    
    if(input$disp == "head") {
      return(head(final_df))
    }
    else {
      return(final_df)
    }
  })
  
  output$table <- renderTable({
    datasetInput()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {'raider_report.csv'},
    content = function(file) {
      write_csv(datasetInput(), file, na='')
    }
  )
  
}

shinyApp(ui, server)