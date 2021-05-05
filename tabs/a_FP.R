fp <- tabPanel("Fractional Polynomials", id="fp",class="active",value="fp",br(),
               tags$head(
                        tags$style(HTML(paste(paste0("[for=val_coef", 1:2,"_fp-coef]+span>.irs>.irs-single, [for=val_coef", 1:2, "_fp-coef]+span>.irs-bar-edge, [for=val_coef", 1:2, "_fp-coef]+span>.irs-bar {background: ", col[1:2], ";}"), collapse = " ")))
                    ),
            sidebarPanel(class="input_class", id = "inputs.fp",
                fluidRow(column(7, offset=0, h4("Input parameters")), 
                         column(5, offset=0, 
                                coef_rangeUI("fp")
                                )
                ),
                fluidRow(
                    column(4, 
                        div(style = "padding: 10px 0px; margin:0%",
                            HTML("Power:")
                        )
                    ),
                    column(8, offset=0, 
                        div(style = "padding: 10px 0px; margin:0%",
                            HTML("Coefficient:")
                        )
                    )
                ),
                fluidRow(
                    column(4, offset=0, 
                           wellPanel(
                            sliderTextInput(inputId = "power1.fp", label="First",choices = c(-2, -1, -0.5, 0, 0.5, 1, 2, 3), selected=1), 
                            sliderTextInput(inputId = "power2.fp",label="Second", choices = c(-2, -1, -0.5, 0, 0.5, 1, 2, 3), selected=1)
                            )
                    ), 
                    column(8, offset=0,
                           wellPanel(
                             sliderplUI("val_coef1_fp"), 
                             sliderplUI("val_coef2_fp")
                           )
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
                           materialSwitch(inputId = "add_y.fp", label = "Response", status = "primary", right = FALSE, value=TRUE)
                    ), 
                    column(6, offset=0, 
                           materialSwitch(inputId = "add_loess.fp", label = "Loess Smoother", status = "primary", right = FALSE, value=TRUE)
                    ),
                    column(6, offset=0,
                           materialSwitch(inputId = "add_optfit.fp", label = "Optimal fit", status = "primary", right = FALSE, value=TRUE)
                    )
                ), 
                fluidRow(
                  column(12, offset=0, actionButton("reset_input.fp", "Reset inputs", class = "btn reset_btn"))
                )
),mainPanel(
  column(8,
    wellPanel(h4("Response function"), plotlyOutput("plot.fp")), 
    wellPanel(h4("Fractional polynomials"), plotlyOutput("basis_plot.fp", height = "200px")), 
    wellPanel(h4("Code"), codeUI("code_fp"))
  ), 
  column(4,
        wellPanel(h4("Transformation: "),uiOutput("transformation.fp")), 
        wellPanel(h4("Goodness of fit"), statsUI("stats_fp")),
        wellPanel(h4("Formula: "),uiOutput("formula.fp"))
  )
)
)
