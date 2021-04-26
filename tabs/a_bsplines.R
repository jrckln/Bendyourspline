bsplines <- tabPanel("B-Splines", id="bsplines", class="fade", value = "bsplines",br(),
                     tags$head(
                        tags$style(HTML(paste(paste0("[for=bs_coef", 1:length(col),"-coef]+span>.irs>.irs-single, [for=bs_coef", 1:length(col), "-coef]+span>.irs-bar-edge, [for=bs_coef", 1:length(col), "-coef]+span>.irs-bar {background: ", col, ";}"), collapse = " ")))
                    ),
             sidebarPanel(class="input_class", id = "inputs.bs",
                fluidRow(column(7, offset=0, h4("Input parameters")),
                         column(5, offset=0,
                                coef_rangeUI("bs")
                                )
                ),
                br(), br(),
                fluidRow(
                    column(6,
                        sliderInput("degree.bs", "Degree", min=1, max=4, value=1, step=1),
                        numericInput("nknots.bs", "Number of internal knots", min=1, max = 10, value = 2, width="100px")
                    )
                ),
                fluidRow(
                    column(6, wellPanel(tags$div(id = 'placeholder_pos_bs')), style="padding: 2px;"),
                    column(6, wellPanel(tags$div(id = 'placeholder_coef_bs'), style="padding: 2px;"))
                ),
                fluidRow(
                  div(style="display:inline-block; width: 20%; vertical-align: -150%;padding-left: 3%;",
                      materialSwitch(inputId = "adjust_intercept.bs", label = "Adjust intercept automatically:", 
                                       status = "primary", right = FALSE)),
                      p("or: ", style="display:inline-block; width: 4%;"),
                      div(style="display:inline-block; width: 65%;",sliderInput("intercept.bs",label="Intercept",min = 0, max = 40, value = 0, step = 0.1))
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
  fluidRow(
    column(8,
      wellPanel(plotlyOutput("plot.bs"))
    ), 
    column(4,
          popify(wellPanel(statsUI("stats_bs")), "Note", "Maximal R2 value is based on the set number of knots and set degree." )
    )
  ),
  fluidRow(
    wellPanel(plotlyOutput("basis_plot.bs", height = "200px"))
  )
  )
) 