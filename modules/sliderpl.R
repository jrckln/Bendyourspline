# Module UI
sliderplUI <- function(id) {
  ns <- NS(id)
  div(
      actionButton(ns("minus"), "", icon = icon("minus-square"), style='padding:4px; font-size:80%;
                   vertical-align: -150%;background: #FFFFFF; width: 10%;display: inline-block;'),
      sliderInput(ns("coef"),label="",min =-1, max = 1, value = 0, step = 0.01, width= "75%", ticks = FALSE),
      actionButton(ns("plus"), "", icon = icon("plus-square"), style='padding:4px; font-size:80%; 
                   vertical-align: -150%;background: #FFFFFF; width: 10%;display: inline-block;'), id = id)
}

# Module Server
sliderpl <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      coef <- reactiveVal(0)
      ns <- session$ns
      observeEvent(input$coef, {
          coef(input$coef)
      })
      observeEvent(input$minus, {
                new <- coef() - 0.01
                updateSliderInput(session, "coef", value = new)
                coef(new)
             })
      observeEvent(input$plus, {
                new <- coef() + 0.01
                updateSliderInput(session, "coef", value = new)
                coef(new)
            })
     return(coef)
    }
  )
}