# Module UI
coef_rangeUI <- function(id) {
  ns <- NS(id)
  div(class = 'coefranges', 
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
        if(range.coefs() == 1){
          new <- range.coefs() + 9
        } else {
          new <- range.coefs() + 10
        }
        range.coefs(new)
      })
      observeEvent(input$decrease_range, {
        if(range.coefs() == 10){
          new <- range.coefs() - 9
        } else if(range.coefs() != 1){
          new <- range.coefs() - 10
        }
        range.coefs(new)
      })
     return(range.coefs)
    }
  )
}