bsplines <- tabPanel("B-Splines", id="bsplines", class="fade", value = "bsplines",br(),
            sidebarPanel(class="input_class", id = "inputs.bs",
                fluidRow(column(7, offset=0, h4("Input parameters"))
                ), 
                fluidRow(
                    column(6,
                        sliderInput("degree.bs", "Degree", min=1, max=4, value=1, step=1)
                    ),
                    column(6,
                        numericInput("nknots.bs", "Number of internal knots", min=1, value = 2)
                    )
                ), 
                fluidRow(
                    column(6,offset=6,
                        materialSwitch(inputId = "adjust_intercept.bs", label = "Adjust intercept automatically:", 
                                       status = "primary", right = FALSE),
                        p("or: ")
                        ),
                    column(6,offset=6,
                        sliderInput("intercept.bs", "Intercept", min=1, max=100, value=0, step=0.1)
                    )
                ), 
                fluidRow(
                    column(6, tags$div(id = 'placeholder_pos_bs')), 
                    column(6, tags$div(id = 'placeholder_coef_bs'))
                ),
                fluidRow(
                    column(3, offset=0, 
                           materialSwitch(inputId = "add_y.bs", label = "Add response", status = "primary", right = FALSE)
                    ), 
                    column(3, offset=0, 
                           materialSwitch(inputId = "add_mean.bs", label = "Add mean", status = "primary", right = FALSE)
                    ),
                    column(3, offset=0, 
                           materialSwitch(inputId = "add_knots_pos.bs", label = "Add knots position", status = "primary", right = FALSE)
                    )
                ), 
                fluidRow(
                  column(12, offset=0, actionButton("reset_input.bs", "Reset inputs"))
                )
),mainPanel(
    plotlyOutput("plot.bs")
  )
) 