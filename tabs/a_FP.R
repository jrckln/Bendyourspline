fp <- tabPanel("Fractional Polynomials", id="fp",class="active",value="fp",br(),
               tags$head(
                        tags$style(HTML(paste(paste0("[for=val_coef", 1:2,"_fp-coef]+span>.irs>.irs-single, [for=val_coef", 1:2, "_fp-coef]+span>.irs-bar-edge, [for=val_coef", 1:2, "_fp-coef]+span>.irs-bar {background: ", col[1:2], ";}"), collapse = " "))),
                        tags$style(HTML(paste0(".label-primary[for=add_optfit_fp] {background: ", optfitcol,";}"))), 
                        tags$style(HTML(paste0(".label-primary[for=add_loess_fp] {background: ", loesscol,";}")))
                        ),
            column(4,
                   fluidRow(
            sidebarPanel(class="input_class", id = "inputs.fp", width = 12,
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
                  column(12,
                  wellPanel(
                      uiOutput("intercept_slider_fp"),
                      actionButton(inputId = "adjust_intercept.fp", label = "Adjust automatically", class='btn reset_btn')
                  )
                  )
                ), 
                fluidRow(
                    column(6, offset=0, 
                           materialSwitch(inputId = "add_y.fp", label = "Response", status = "primary", right = FALSE, value=TRUE)
                    ), 
                    column(6, offset=0, 
                           materialSwitch(inputId = "add_loess_fp", label = "Loess Smoother", status = "primary", right = FALSE, value=TRUE)
                    ),
                    column(6, offset=0,
                           materialSwitch(inputId = "add_optfit_fp", label = "Optimal fit", status = "primary", right = FALSE, value=TRUE)
                    )
                ), 
                fluidRow(
                  column(12, offset=0, actionButton("reset_input.fp", "Reset inputs", class = "btn reset_btn"))
                )
)),
fluidRow(codeUI("code_fp"))),
column(8,
mainPanel(width = 12,
  column(8,
    wellPanel(h4("Response function"), withSpinner(plotlyOutput("plot.fp"), color = colors3[1], size = 1)), 
    wellPanel(h4("Fractional polynomials"), withSpinner(plotlyOutput("basis_plot.fp", height = "200px"), color = colors3[1], size = 1))
  ), 
  column(4,
        wellPanel(h4("Transformation: "),uiOutput("transformation.fp")), 
        wellPanel(h4("Goodness of fit"), statsUI("stats_fp")),
        wellPanel(h4("Formula: "),uiOutput("formula.fp"))
  )
))
)
