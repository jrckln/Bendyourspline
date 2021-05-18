# Module UI
codeUI <- function(id) {
  ns <- NS(id)
  div(class="Rcode", wellPanel(h4("Code"),
      actionButton(ns("showcode"), label ="Show R code", class="reset_btn"), 
      downloadButton(ns("downloadcode"), label = "Download", class="reset_btn")
    )
  )
}

# Module Server
codeServer <- function(id, filenames_code, variable) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      settngsModal <- function() {
        modalDialog(
          modalInnerUI(ns("code")),
          title = "R code",
          footer = modalButton("Dismiss"),
          size = "l",
          easyClose = FALSE,
          fade = TRUE)
      }
      observeEvent(input$showcode,{
        modalInner("code", filenames_code[1])
        showModal(settngsModal())
      })
      output$downloadcode <- downloadHandler(
          filename = function() {
            paste0("output-",Sys.Date(), ".zip")
          },
          content = function(file) {
            var <- paste0(gsub("~", "", gsub(" ", "", variable)), ".RData")
            files <- list.files(path="data/")
            datafile <- match(var, files)
            datafile <- paste0("data/", files[datafile])
            zip(zipfile = file, files = c(filenames_code, datafile), flags = "-j")
          },
      contentType = "application/zip")
    }
  )
}

#Inner Module UI to show R code as uiOuput
modalInnerUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("code_text"))
}
#Inner module server
modalInner <- function(id, filename_toshow){
  moduleServer(
    id,
    function(input, output, session) {
      output$code_text <- renderUI({
        code <- readChar(filename_toshow, file.info(filename_toshow)$size)
        prismCodeBlock(code)
      })
    }
  )
}