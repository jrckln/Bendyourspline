# Module UI
sliderplUI <- function(id, range_slider = 1, label = "") {
    if(length(range_slider)==1){
        minrange <- (-1)*range_slider
        maxrange <- range_slider
    } else {
        minrange <- range_slider[1]
        maxrange <- range_slider[2]
    }
  ns <- NS(id)
  tagList(
    withMathJax(), 
     div(
      actionButton(ns("minus"), "", icon = icon("minus"), style='padding:1%; font-size:80%;
                   vertical-align: -150%;background: #FFFFFF; width: 8%;display: inline-block;'),
      sliderInput(ns("slider"),label= label,min = minrange, max = maxrange, value = 0, step = 0.01, width= "75%", ticks = FALSE),
      actionButton(ns("plus"), "", icon = icon("plus"), style='padding:1%; font-size:80%; 
                   vertical-align: -150%;background: #FFFFFF; width: 8%;display: inline-block;'), id = id)
  )
}

# Module Server
sliderpl <- function(id, step) {
  moduleServer(
    id,
    function(input, output, session) {
      coef <- reactiveVal(0)
      ns <- session$ns
      bindEvent(
          observe(coef(input$slider)), 
          input$slider
      )
      bindEvent(
          observe({
                new <- coef() - step
                updateSliderInput(session, "slider", value = new)
                coef(new)
          }), 
          input$minus, ignoreInit = TRUE
      )
      bindEvent(
          observe({
                new <- coef() + step
                updateSliderInput(session, "slider", value = new)
                coef(new)
          }), 
          input$plus, ignoreInit = TRUE
      )
    }
  )
}