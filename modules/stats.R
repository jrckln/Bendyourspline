statsUI <- function(id) {
  ns <- NS(id)
  
  htmlTemplate(
    filename = "www/modules/stats.html",
    r2 = uiOutput(ns("r2_out")), r2adjusted = uiOutput(ns("r2adjusted_out")), 
    r2max = uiOutput(ns("r2max_out")), intercept = uiOutput(ns("intercept_out"))
  )
}

stats <- function(id, stats, intercept) {
  moduleServer(
    id,
    function(input, output, session) {
      output$r2_out <- renderUI({
          HTML(as.character(round(stats[1], 3)))
      })
      output$r2adjusted_out <- renderUI({
          HTML(as.character(round(stats[2], 3)))
      })
      output$r2max_out <- renderUI({
          HTML(as.character(round(stats[3], 3)))
      })
      output$intercept_out <- renderUI({
          HTML(as.character(round(intercept, 3)))
      })
      return("")
    }
  )
}  