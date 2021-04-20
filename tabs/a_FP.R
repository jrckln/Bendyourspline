fp <- tabPanel("Fractional Polynomials", id="fp",class="active",value="fp",br(),
            sidebarPanel(class="input_class", id = "inputs.fp",
                fluidRow(column(7, offset=0, h4("Input parameters")), 
                         column(5, offset=0, 
                                div(p("Coefficient range:"),
                                  actionButton("decrease_range.fp", icon("minus")), 
                                  actionButton("increase_range.fp", icon("plus"))
                                  )
                                )
                ),
                fluidRow(
                    column(4, 
                        div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                            HTML("Power:")
                        )
                    ),
                    column(8, offset=0, 
                        div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                            HTML("Coefficient:")
                        )
                    )
                ),
                fluidRow(
                    column(4, offset=0, 
                           sliderTextInput(inputId = "power1.fp", label="First",choices = c(-2, -1, -0.5, 0, 0.5, 1, 2, 3), selected=1)
                    ), 
                    column(8, offset=0,
                             actionButton("minus_val_coef1.fp", "", icon = icon("minus-square"), style='padding:4px; font-size:80%; 
                                          vertical-align: -150%;background: #D6D6D6;display:inline-block;'), 
                             uiOutput("slider.coef1.fp", style="width: 80%; display: inline-block;"), 
                             actionButton("add_val_coef1.fp", "", icon = icon("plus-square"), style='padding:4px; font-size:80%; 
                                           vertical-align: -150%;background: #D6D6D6;display:inline-block;')
                           )
                ),
                fluidRow(
                    column(4, offset=0, 
                           sliderTextInput(inputId = "power2.fp",label="Second", choices = c(-2, -1, -0.5, 0, 0.5, 1, 2, 3), selected=1)
                    ), 
                    column(8, offset=0,
                             actionButton("minus_val_coef2.fp", "", icon = icon("minus-square"), style='padding:4px; font-size:80%; 
                                          vertical-align: -150%;background: #D6D6D6;display:inline-block;'), 
                             uiOutput("slider.coef2.fp", style="width: 80%; display: inline-block;"), 
                             actionButton("add_val_coef2.fp", "", icon = icon("plus-square"), style='padding:4px; font-size:80%; 
                                           vertical-align: -150%;background: #D6D6D6;display:inline-block;')
                           )
                ), 
                fluidRow(
                  div(style="display:inline-block; width: 20%; vertical-align: -150%;padding-left: 3%;",
                      materialSwitch(inputId = "adjust_intercept.fp", label = "Adjust intercept automatically:", 
                                       status = "primary", right = FALSE)),
                      p("or: ", style="display:inline-block; width: 4%;"),
                      div(style="display:inline-block; width: 65%;",sliderInput("intercept.fp",label="Intercept",min = 0, max = 40, value = 0, step = 0.1))
                ), 
                fluidRow(
                    column(6, offset=0, 
                           materialSwitch(inputId = "add_y.fp", label = "Add response", status = "primary", right = FALSE)
                    ), 
                    column(6, offset=0, 
                           materialSwitch(inputId = "add_mean.fp", label = "Add mean", status = "primary", right = FALSE)
                    )
                    # ),
                    # column(6, offset=0, 
                    #        materialSwitch(inputId = "add_CI.fp", label = "Add confidence band", status = "primary", right = FALSE, width="70%")
                    # )
                ), 
                fluidRow(
                  column(12, offset=0, actionButton("reset_input.fp", "Reset inputs"))
                )
),mainPanel(
  column(8,
    wellPanel(plotlyOutput("plot.FP"))
  ), 
  column(4,
        wellPanel( h5("Transformation: "),uiOutput("transformation.fp")), 
        wellPanel(uiOutput("stats.fp")),
        wellPanel(h5("Formula: "),uiOutput("formula.fp"))
  )
)
)
