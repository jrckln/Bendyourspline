fp <- tabPanel("Fractional Polynomials", id="fp",class="active",value="fp",br(),
            sidebarPanel(class="input_class", 
                fluidRow(column(9, offset=0, h4("Input parameters"))
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
                    column(5, offset=0, 
                           sliderTextInput(inputId = "power1.fp", label="First",choices = c(-2, -1, -0.5, 0, 0.5, 1, 2, 3), selected=1)
                    ), 
                    column(5, offset=0, 
                           sliderInput("coef1.fp",label="", min = -3, max = 3, value = 0, step = 0.01)
                    )
                ),
                fluidRow(
                    column(5, offset=0, 
                           sliderTextInput(inputId = "power2.fp",label="Second", choices = c(-2, -1, -0.5, 0, 0.5, 1, 2, 3), selected=1)
                    ), 
                    column(5, offset=0, 
                           sliderInput("coef2.fp",label="",min = -3, max = 3, value = 0, step = 0.01)
                    )
                ), 
                fluidRow(
                    column(12, offset=0, 
                           sliderInput("intercept.fp",label="Intercept",min = 0, max = 40, value = 0, step = 0.1)
                    )
                ), 
                fluidRow(
                    column(6, offset=0, 
                           materialSwitch(inputId = "add_y.fp", label = "Add response", status = "primary", right = FALSE)
                    ), 
                    column(6, offset=0, 
                           materialSwitch(inputId = "add_mean.fp", label = "Add Mean", status = "primary", right = FALSE)
                    )
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