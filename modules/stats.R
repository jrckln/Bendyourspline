statsUI <- function(id) {
  ns <- NS(id)
  
  htmlTemplate(
    filename = "www/modules/stats.html",
    r2adjusted = uiOutput(ns("r2_out")), maxr2adjusted = uiOutput(ns("r2max_out"))
  )
}

stats <- function(id, stats) {
  moduleServer(
    id,
    function(input, output, session) {
      output$r2_out <- renderUI({
          HTML(as.character(round(stats[1], 3)))
      })
      output$r2max_out <- renderUI({
          HTML(as.character(round(stats[2], 3)))
      })
    }
  )
}  