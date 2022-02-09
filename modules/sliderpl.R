individualsliderUI <- function(id, range_slider, label) {
    if(length(range_slider)==1){
        minrange <- (-1)*range_slider
        maxrange <- range_slider
    } else {
        minrange <- range_slider[1]
        maxrange <- range_slider[2]
    }
    ns <- NS(id)
    tagList(div(id = id,style='display: flex;align-items:center;',
      div(actionButton(ns("minus"), "", icon = icon("minus"), style='background: #FFFFFF; float: left;'), style="flex: 0.05"),
      div(style="flex: 0.9; padding: 5px;", sliderInput(ns("slider"),label= label,min = minrange, max = maxrange, value = 0, step = 0.01, ticks = FALSE, width = "100%")),
      div(actionButton(ns("plus"), "", icon = icon("plus"), style='background: #FFFFFF; float: right;'), style="flex: 0.05")
    ))
}



individualslider <- function(id, step = 0.01) {
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
      return(coef)
    }
  )
}

sliderPLUI <- function(id, label) {
    ns <- NS(id)
    if(missing(label)) label <- ''
    
    div(
        div(style = 'margin-bottom: 20px;',
            span(label),
            actionButton(ns('finetuningmenu'), "", icon = icon("bars"), 
                         style='background: #FFFFFF; float: right; border: none;'), 
        ), 
        uiOutput(ns('sliders'))
    )
}

sliderPL <- function(id, number=1, labelsindividual) {
  if(missing(labelsindividual)) labelsindividual <- rep_len('', number)
  moduleServer(
    id,
    function(input, output, session) {
        ns <- session$ns
        finetuningstep <- reactiveVal(0.01)
        finetuningrangemin <-reactiveVal(-1)
        finetuningrangemax <-reactiveVal(1)
        slidervalues <- reactiveValues()
        bindEvent(
          observe({
              showModal(
                  modalDialog(
                      title='Fine tuning of coefficient slider', 
                      HTML('<h3>Step size</h3><br><p>Adjust step for <button id="" type="button" class="btn btn-default action-button"
                            style="background: #FFFFFF; display: inline-block;"><i class="fa fa-minus" 
                           role="presentation" aria-label="minus icon"></i></button> and <button id="" type="button" class="btn btn-default action-button"
                            style="background: #FFFFFF; display: inline-block;"><i class="fa fa-plus" 
                           role="presentation" aria-label="minus icon"></i></button> Button for coefficient slider:</p>'), 
                      numericInput(ns('finetuning'), '', value = finetuningstep(), min = 0.01, max = 10, step = 0.01),
                      HTML('<h3>Step size</h3><br><p>Adjust minimum and maximum for coefficient slider:</p>'),
                      numericInput(ns('rangemin'), 'Minimum', value = finetuningrangemin()), 
                      numericInput(ns('rangemax'), 'Maximum', value = finetuningrangemax()),
                      easyClose = TRUE, 
                      footer = modalButton("Close")
                  )
              )
          }),
          input$finetuningmenu
        )
    
        observe({
            req(input$finetuning)
            finetuningstep(input$finetuning)
        })
        observe({
            req(input$rangemin)
            finetuningrangemin(input$rangemin)
        })
        observe({
            req(input$rangemax)
            finetuningrangemax(input$rangemax)
        })
        
        output$sliders <- renderUI({
            min <- finetuningrangemin()
            max <- finetuningrangemax()
            sapply(
                1:number, 
                function(i){
                    idindividual <- paste0(id, i)
                    individualsliderUI(id = ns(idindividual), range_slider = c(min, max), label = labelsindividual[i])
                }
            )
        })
        
        observe({
            step <- finetuningstep()
            tmp <- sapply(
                1:number, 
                function(i){
                    idindividual <- paste0(id, i)
                    individualslider(id = idindividual, step = step)
                    }
                )
            for(i in 1:number){
                slidervalues[[paste0('value', i)]] <- tmp[[i]]
            }
        })
        return(slidervalues)
    }
)}