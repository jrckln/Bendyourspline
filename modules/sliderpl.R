plbutton <- function(inputId,
                     icon = NULL) {
    value <- shiny::restoreInput(id = inputId, default = NULL)
    Bttn <- tags$button(
        id = inputId,
        type = "button",
        class = "btn btn-default action-button plbtn",
        `data-val` = value, list(icon)
  )
}


individualsliderUI <- function(id, range_slider, label, value = 0) {
    if(length(range_slider)==1){
        minrange <- (-1)*range_slider
        maxrange <- range_slider
    } else {
        minrange <- range_slider[1]
        maxrange <- range_slider[2]
    }
    ns <- NS(id)
    tagList(div(id = id,style='display: flex;align-items:center;',
      div(plbutton(ns('minus'), icon('minus')), style="flex: 0.05"),
      div(style="flex: 0.9; padding: 5px;", 
          sliderInput(ns("slider"),label= label,min = minrange, 
                      max = maxrange, value = value, step = 0.01, ticks = FALSE, width = "100%")),
      div(plbutton(ns('plus'), icon('plus')), style="flex: 0.05")
    ))
}



individualslider <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      coef <- reactiveVal(0)
      step <- reactiveVal(0.01)
      bindEvent(
          observe(coef(input$slider)), 
          input$slider
      )
      
      bindEvent(
          observe({
                new <- coef() - step()
                updateSliderInput(session, "slider", value = new)
                coef(new)
          }), 
          input$minus, ignoreInit = TRUE
      )
      bindEvent(
          observe({
                new <- coef() + step()
                updateSliderInput(session, "slider", value = new)
                coef(new)
          }), 
          input$plus, ignoreInit = TRUE
      )
      return(
          list(
              value = coef, 
              setStep = function(newstep){
                  step(newstep)
              },
              setValue = function(newvalue){
                  updateSliderInput(session, "slider", value = newvalue)
              }, 
              setRange = function(min, max){
                  updateSliderInput(session, "slider", min = min, max = max)
              }
          )
      )
    }
  )
}

sliderPLUI <- function(id, label) {
    ns <- NS(id)
    if(missing(label)) label <- ''
    
    div(
        div(style = 'margin-bottom: 20px;',
            span(label, style = 'word-break: break-all'),
            actionButton(ns('finetuningmenu'), '',icon('option-horizontal', lib ='glyphicon'),
                         style='background: #FFFFFF; float: right; border: none;'), 
        ), 
        uiOutput(ns('sliders'))
    )
}

sliderPL <- function(id, number, labelsindividual, ranges, values) {
  if(missing(labelsindividual)) labelsindividual <- ''
  if(missing(ranges)) ranges <- c(-1,1)
  moduleServer(
    id,
    function(input, output, session) {
        ns <- session$ns
        finetuningstep <- reactiveVal(0.01)
        finetuningrangemin <-reactiveVal(ranges[1])
        finetuningrangemax <-reactiveVal(ranges[2])
        slidervalues <- reactiveValues()
        slider <- reactiveValues()

        finetuningmodal <- reactive({
                modalDialog(
                      title='Fine tuning of coefficient slider', 
                      HTML('<h3>Step size</h3><br><p>Adjust step for <button id="" type="button" class="btn btn-default action-button"
                            style="background: #FFFFFF; display: inline-block;"><i class="fa fa-minus" 
                           role="presentation" aria-label="minus icon"></i></button> and <button id="" type="button" class="btn btn-default action-button"
                            style="background: #FFFFFF; display: inline-block;"><i class="fa fa-plus" 
                           role="presentation" aria-label="minus icon"></i></button> Button for coefficient slider:</p>'), 
                      numericInput(ns('finetuning'), '', value = finetuningstep(), min = 0.01, max = 10, step = 0.01),
                      HTML('<h3>Range</h3><br><p>Adjust minimum and maximum for coefficient slider:</p>'),
                      numericInput(ns('rangemin'), 'Minimum', value = finetuningrangemin()), 
                      numericInput(ns('rangemax'), 'Maximum', value = finetuningrangemax()),
                      easyClose = TRUE, 
                      footer = modalButton("Cancel")
                  )})
        
        bindEvent(
          observe({
              showModal(
                  finetuningmodal()
              )
          }),
          input$finetuningmenu
        )
        
        bindEvent(
            observe({
                #req(input$finetuning)
                finetuningstep(input$finetuning)
            }), 
            input$finetuning, ignoreInit = TRUE
        )
        bindEvent(
            observe({
                sapply(
                    1:isolate(number()),
                    function(i){
                        idindividual <- paste0(id, i,'-slider')
                        updateSliderInput(session, inputId = idindividual, min = input$rangemin)
                    }
                )
                finetuningrangemin(input$rangemin)
            }), 
            input$rangemin, ignoreInit = TRUE
        )
        bindEvent(
            observe({
                sapply(
                    1:isolate(number()),
                    function(i){
                        idindividual <- paste0(id, i,'-slider')
                        updateSliderInput(session, inputId = idindividual, max = input$rangemax)
                    }
                )
                finetuningrangemax(input$rangemax)
            }), 
            input$rangemax, ignoreInit = TRUE
        )
        
        
        output$sliders <- renderUI({
            min <- isolate(finetuningrangemin())
            max <- isolate(finetuningrangemax())
                sapply(
                    1:number(),
                    function(i){
                        value <- values[[paste0('value',i)]]()
                        idindividual <- paste0(id, i)
                        individualsliderUI(id = ns(idindividual), range_slider = c(min, max), 
                                           label = labelsindividual, value = value)
                    }
                )
        })
        
        observe({
            tmp <- sapply(simplify = FALSE,
                1:number(), 
                function(i){
                    idindividual <- paste0(id, i)
                    individualslider(id = idindividual)
                    }
                )
            for(i in 1:number()){
                slidervalues[[paste0('value', i)]] <- tmp[[i]]$value
                slider[[as.character(i)]] <- tmp[[i]]
            }
        }, priority = 20)
        
        bindEvent(
            observe({
                sapply(
                        1:number(), 
                        function(x){
                            slider[[as.character(x)]]$setStep(finetuningstep())
                        }
                    )
            }),
            finetuningstep(), ignoreInit = TRUE
        )
        
        return(
            list(
                values = slidervalues, 
                setRange = function(min, max) {
                    sapply(
                        1:number(), 
                        function(x){
                            slider[[as.character(x)]]$setRange(min, max)
                        }
                    )
                    finetuningrangemin(min)
                    finetuningrangemax(max)
                    }, 
                setValues = function(values) {
                    sapply(
                        1:length(values), 
                        function(x){
                            slider[[as.character(x)]]$setValue(values[x])
                        }
                    )
                    }
            )
        )
    }
)}