bsplines <- tabPanel("B-Splines", id="bsplines", class="fade", value = "bsplines",br(),
            sidebarPanel(class="input_class", id = "inputs.fp",
                fluidRow(column(7, offset=0, h4("Input parameters"))
                ), 
                fluidRow(
                    column(6,
                        sliderInput("degree.bs", "Degree", min=1, max=4, value=1, step=1)
                    ),
                    column(6,
                        
                    )
                ), 
                fluidRow(
                    column(6,
                        sliderInput("intercept.bs", "Intercept", min=1, max=100, value=0, step=0.1)
                    ),
                    column(6,
                        
                    )
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
                  column(12, offset=0, actionButton("reset_input.fp", "Reset inputs"))
                )
),mainPanel(
    plotlyOutput("plot.bs")
  )
) 