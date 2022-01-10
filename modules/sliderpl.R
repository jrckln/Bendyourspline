# Module UI
sliderplUI <- function(id, range_slider = 1, label = "") {
  ns <- NS(id)
  tagList(
    withMathJax(), 
     div(
      actionButton(ns("minus"), "", icon = icon("minus"), style='padding:1%; font-size:80%;
                   vertical-align: -150%;background: #FFFFFF; width: 8%;display: inline-block;'),
      sliderInput(ns("slider"),label= label,min = (-1)*range_slider, max = range_slider, value = 0, step = 0.01, width= "75%", ticks = FALSE),
      actionButton(ns("plus"), "", icon = icon("plus"), style='padding:1%; font-size:80%; 
                   vertical-align: -150%;background: #FFFFFF; width: 8%;display: inline-block;'), id = id)
  )
}

# Module Server
sliderpl <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      coef <- reactiveVal(0)
      ns <- session$ns
      observeEvent(input$slider, {
          coef(input$slider)
      })
      observeEvent(input$minus, {
                new <- coef() - 0.01
                updateSliderInput(session, "slider", value = new)
                coef(new)
             })
      observeEvent(input$plus, {
                new <- coef() + 0.01
                updateSliderInput(session, "slider", value = new)
                coef(new)
            })
     return(coef)
    }
  )
}