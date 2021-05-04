# Module UI
codeUI <- function(id) {
  ns <- NS(id)
  div(class="code_R",
      uiOutput(ns("code_text")), 
      downloadButton(ns("downloadcode"), label = "Download")
  )
}

# Module Server
codeServer <- function(id, filename_code) {
  moduleServer(
    id,
    function(input, output, session) {
      output$code_text <- renderUI({
          code <- readasHtml(filename_code)
          HTML(code)
      })
      output$downloadcode <- downloadHandler(
          filename = function() {
            paste("output", "zip", sep=".")
          },
          content = function(file) {
            #file.copy(filename_code, file)
            zip(zipfile = file, files = filename_code)
          },
      contentType = "application/zip")
    }
  )
}