fp <- tabPanel("Fractional Polynomials", id="fp",class="active",value="fp",br(),
            sidebarPanel(class="input_class", id = "inputs.fp",
                fluidRow(column(7, offset=0, h4("Input parameters")), 
                         column(5, offset=0, actionButton("increase_range.fp", "Increase coefficient range"))
                ),
                fluidRow(
                    column(4, offset=4, 
                        div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                            HTML("Power:")
                        )
                    ),
                    column(4, offset=0, 
                        div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                            HTML("Coefficient:")
                        )
                    )
                ),
                fluidRow(
                    column(4, offset=0, 
                           sliderTextInput(inputId = "power1.fp", label="First",choices = c(-2, -1, -0.5, 0, 0.5, 1, 2, 3), selected=1)
                    ), 
                    column(1,actionButton("minus_val_coef1.fp", "", icon = icon("minus-square"), style='padding:4px; font-size:80%')), 
                    column(6, uiOutput("slider.coef1.fp")),
                    column(1, actionButton("add_val_coef1.fp", "", icon = icon("plus-square"), style='padding:4px; font-size:80%'))
                ),
                fluidRow(
                    column(4, offset=0, 
                           sliderTextInput(inputId = "power2.fp",label="Second", choices = c(-2, -1, -0.5, 0, 0.5, 1, 2, 3), selected=1)
                    ), 
                    column(1,actionButton("minus_val_coef2.fp", "", icon = icon("minus-square"), style='padding:4px; font-size:80%')), 
                    column(6, uiOutput("slider.coef2.fp")),
                    column(1, actionButton("add_val_coef2.fp", "", icon = icon("plus-square"), style='padding:4px; font-size:80%'))
                ), 
                fluidRow(
                    column(12, offset=0, 
                           sliderInput("intercept.fp",label="Intercept",min = 0, max = 40, value = 0, step = 0.1)
                    )
                ), 
                fluidRow(
                    column(3, offset=0, 
                           materialSwitch(inputId = "add_y.fp", label = "Add response", status = "primary", right = FALSE)
                    ), 
                    column(3, offset=0, 
                           materialSwitch(inputId = "add_mean.fp", label = "Add mean", status = "primary", right = FALSE)
                    ),
                    column(6, offset=0, 
                           materialSwitch(inputId = "add_CI.fp", label = "Add confidence band", status = "primary", right = TRUE)
                    )
                ), 
                fluidRow(
                  column(12, offset=0, actionButton("reset_input.fp", "Reset inputs"))
                )
),mainPanel(
    fluidRow(
        column(2, offset = 0, h5("Transformation: "),
               wellPanel(uiOutput("transformation.fp"))),
        column(4, offset = 0, h5("FP-formula: "),
               wellPanel(uiOutput("formula.fp"))), 
        column(4, offset = 2, 
               uiOutput("stats.fp")
        )
    ),
    plotlyOutput("plot.FP")
  )
) 