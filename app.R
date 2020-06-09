library(rmarkdown)
library(shiny)
library(callr)

if (interactive()) {
  labels_button <- c("Render", "Cancel")
  flag_render <- reactiveValues(flag = FALSE)
  flag_render_finish <- reactiveValues(flag = FALSE)
  flag_nopreview <- reactiveValues(flag = TRUE)
  flag_download <- reactiveValues(flag = TRUE)
  wd <- getwd()
  tmpdir <- gsub("\\\\", "", tempfile(pattern = "/tmp_",tmpdir = wd))
  if (!dir.exists(tmpdir)) {
    dir.create(tmpdir)
  }
  tmpfile_rmd <- paste(tmpdir, "/tmp.Rmd", sep = "")
  tmpfile_output <- paste(tmpdir, "/tmp.html", sep = "")
  rs <- r_session$new()
  text_initial <- '---
title: "Title"
author: "author"
date: "`r Sys.Date()`"
output: html_document
---

text
'
  
  ui <- fluidPage(
    
    title = "Rmarkdown Previewer",
    
    br(),
    
    fluidRow(
      column(1,
        actionButton("preview_button", labels_button[1])
      ),
      column(1,
        actionButton("save_button", "Save")
      ),
      column(10)
    ),
    
    br(),
    
    tags$head(tags$script('var dim_box = [0, 0];
                           $(document).on("shiny:connected", function(e) {
                             dim_box[0] = window.innerWidth;
                             dim_box[1] = window.innerHeight;
                             Shiny.onInputChange("dim_box", dim_box);
                           });')
    ),
    
    fluidRow(
      column(6,
        uiOutput("text_input")
      ),
      column(6,
        uiOutput("render_output")
      )
    )
  )
  
  server <- function(input, output, session) {
    session$onSessionEnded(function() {
      unlink(tmpdir, recursive = TRUE)
      rs$close()
      stopApp()
    })
    
    output$text_input <- renderUI({
      get_box_width <- function() ifelse(length(input$dim_box[1]) == 0, "auto", as.numeric(input$dim_box[1]) * 0.45)
      get_box_height <- function() ifelse(length(input$dim_box[2]) == 0, "auto", as.numeric(input$dim_box[2]) * 0.85)
      textAreaInput("text_rmarkdown", NULL, value = text_initial, width = get_box_width(), height = get_box_height(), resize = "both")
    })
    
    observeEvent(input$preview_button, {
      flag_render$flag <- !flag_render$flag
      updateActionButton(session, "preview_button", label = labels_button[flag_render$flag + 1])
      if (flag_render$flag) {
        write.table(input$text_rmarkdown, tmpfile_rmd, quote = FALSE, row.names = FALSE, col.names = FALSE)
        rs$call(function(tmpfile_rmd, tmpfile_output) {
          tryCatch({
            rmarkdown::render(tmpfile_rmd, output_format = "html_document", output_file = tmpfile_output)
            list("SUCCESS")
          }, error = function(e) list("ERROR", as.character(e)), warning = function(e) list("WARNING", as.character(e)))
        }, args = list(tmpfile_rmd = tmpfile_rmd, tmpfile_output = tmpfile_output))
      } else {
        rs$initialize()
      }
    })
    observe({
      invalidateLater(10, session)
      tmpvalue_rs <- rs$read()
      if (!is.null(tmpvalue_rs)) {
        if (flag_nopreview$flag) {
          flag_nopreview$flag <- FALSE
        }
        flag_render$flag <- !flag_render$flag
        updateActionButton(session, "preview_button", label = labels_button[flag_render$flag + 1])
        if (tmpvalue_rs$result[[1]] == "SUCCESS") {
          flag_render_finish$flag <- !flag_render_finish$flag
        } else if (tmpvalue_rs$result[[1]] == "WARNING") {
          showModal(modalDialog(title = "WARNING", renderText(as.character(tmpvalue_rs$result[[2]])), footer = modalButton("Close"), easyClose = TRUE))
        } else {
          showModal(modalDialog(title = "ERROR", renderText(as.character(tmpvalue_rs$result[[2]])), footer = modalButton("Close"), easyClose = TRUE))
        }
      }
    })
    observeEvent(flag_render_finish$flag, {
      if (flag_nopreview$flag) {
        output$render_output <- renderUI(HTML("no preview"))
      } else {
        output$render_output <- renderUI(includeHTML(tmpfile_output))
      }
    })
    
    observeEvent(input$save_button, {
      showModal(modalDialog(
        title = "Enter output filename and output directory",
        selectInput("select_output_format", "output format:", list("html", "Rmd"), selected = "html"),
        textInput("output_filename", "output filename", value = "output.html"),
        downloadButton("save_button2", "Save"),
        footer = modalButton("Close"), easyClose = TRUE
      ))
    })
    observeEvent(input$select_output_format, {
      newfilename <- strsplit(input$output_filename, ".", fixed = TRUE)[[1]]
      if (input$select_output_format == "html") {
        if (newfilename[length(newfilename)] == "Rmd") {
          newfilename[length(newfilename)] <- "html"
        }
      } else {
        if (newfilename[length(newfilename)] == "html") {
          newfilename[length(newfilename)] <- "Rmd"
        }
      }
      newfilename <- paste(newfilename, collapse = ".")
      updateTextInput(session, "output_filename", value = newfilename)
    })
    output$save_button2 <- downloadHandler(
      filename = function() {
        input$output_filename
      },
      content = function(file) {
        if (input$select_output_format == "html") {
          file.copy(tmpfile_output, file)
        } else {
          write.table(input$text_rmarkdown, file, quote = FALSE, row.names = FALSE, col.names = FALSE)
        }
        flag_download$flag <- !flag_download$flag
      }
    )
  }
  
  shinyApp(ui = ui, server = server)
}
