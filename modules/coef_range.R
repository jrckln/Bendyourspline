# Module UI
coef_rangeUI <- function(id) {
  ns <- NS(id)
  tagList(
      p("Coefficient range:"),
      actionButton(ns("decrease_range"), icon("minus")), 
      actionButton(ns("increase_range"), icon("plus")) 
  )
}

# Module Server
coef_range <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      range.coefs <- reactiveVal(1)
      observeEvent(input$increase_range, {
                new <- range.coefs() + 10
                range.coefs(new)
             })
      observeEvent(input$decrease_range, {
              if(range.coefs() != 0){
                new <- range.coefs() - 10
                range.coefs(new)

              }
            })
     return(range.coefs)
    }
  )
}