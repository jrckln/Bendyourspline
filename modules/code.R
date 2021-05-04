# Module UI
codeUI <- function(id) {
  ns <- NS(id)
  div(div(class="code_R",
      uiOutput(ns("code_text"))), 
      downloadButton(ns("downloadcode"), label = "Download")
  )
}

# Module Server
codeServer <- function(id, filenames_code) {
  moduleServer(
    id,
    function(input, output, session) {
      output$code_text <- renderUI({
          code <- readChar(filenames_code[1], file.info(filenames_code[1])$size)
          prismCodeBlock(code)
      })
      output$downloadcode <- downloadHandler(
          filename = function() {
            paste0("output-",Sys.Date(), ".zip")
          },
          content = function(file) {
            #filenames_code <- sub('.*/', '', filenames_code)
            zip(zipfile = file, files = filenames_code, flags = "-j")
          },
      contentType = "application/zip")
    }
  )
}