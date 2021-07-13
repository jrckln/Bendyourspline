bsplines <- tabPanel("B-Splines",br(),
                     tags$head(
                        tags$style(HTML(paste(paste0("[for=bs_coef", 1:length(col),"-coef]+span>.irs>.irs-single, [for=bs_coef", 1:length(col), "-coef]+span>.irs-bar-edge, [for=bs_coef", 1:length(col), "-coef]+span>.irs-bar {background: ", col, ";}"), collapse = " "))),
                        tags$style(HTML(paste0(".label-primary[for=add_optfit_bs] {background: ", optfitcol,";}"))), 
                        tags$style(HTML(paste0(".label-primary[for=add_loess_bs] {background: ", loesscol,";}")))
                    ),
            column(4, 
            fluidRow(
             sidebarPanel(class="input_class", id = "inputs.bs", width = 12,
                h4("Input parameters"),
                br(), br(),
                fluidRow(
                    column(3,
                        numericInput("degree.bs", "Degree", min=1, max=4, value=1),
                    ), 
                    column(3,
                        numericInput("nknots.bs", "Number of knots", min=1, max = 10, value = 2)
                    ), 
                    column(6, align="center", 
                        coef_rangeUI("bs")
                    )
                ),
                fluidRow(
                    column(6, wellPanel(tags$div(id = 'placeholder_pos_bs')), style="padding: 2px;"),
                    column(6, wellPanel(tags$div(id = 'placeholder_coef_bs'), style="padding: 2px;"))
                ),
                fluidRow(
                  column(12,
                  wellPanel(
                      uiOutput("intercept_slider_bs"),
                      actionButton(inputId = "adjust_intercept.bs", label = "Adjust automatically", class='btn reset_btn')
                  )
                  )
                ), 
                fluidRow(
                    column(3, offset=0,
                           div(
                             span('Response'),
                             materialSwitch(inputId = "add_y_bs", label = "", right = FALSE, value=TRUE)
                           )
                    ),
                    column(3, offset=0,
                           div(
                             span('Loess Smoother'),
                             materialSwitch(inputId = "add_loess_bs", label = "", right = FALSE, value=TRUE)
                           )
                    ),
                    column(3, offset=0, 
                           div(
                             span('Knot position'),
                             materialSwitch(inputId = "add_knots_pos.bs", label = "", right = FALSE, value=TRUE)
                           )
                    ),
                    column(3, offset=0,
                           div(
                             span('Optimal fit'),
                             materialSwitch(inputId = "add_optfit_bs", label = "", right = FALSE, value=TRUE)
                           )
                    )
                ),
                fluidRow(
                  column(6, offset=0, actionButton("reset_input.bs", "Reset inputs", class = "btn reset_btn")),
                  column(6, offset=0, actionButton("set_optfit_bs", "Set optimal fit", class = "btn reset_btn", style="float:right"))
                )
))),
column(8,
mainPanel(width = 12, 
  fluidRow(
    column(8, 
           jqui_sortable(div(
           wellPanel(h4("Response function"), withSpinner(plotlyOutput("plot.bs"), color = spinnercol, size = 1)),
           wellPanel(h4("Spline basis functions"), withSpinner(plotlyOutput("basis_plot.bs", height = "200px"), color = spinnercol, size = 1))
           ))
           ), 
    column(4, 
           jqui_sortable(div(
              wellPanel(id = 'bs', h4("Exercise"),
              selectInput("exercise_bs", "", names(exercises[['bs']]), selected="Basic"),
              actionButton('start_exercise_bs', 'Start'),
              uiOutput("next_exercise_bs")
              ),
              conditionalPanel('input.add_y_bs | input.add_loess_bs | input.add_optfit_bs', wellPanel(h4("Goodness of fit"), statsUI("stats_bs"))),
              codeUI("code_bs")
         ))
         )
  )
) )
)