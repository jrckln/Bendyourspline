naturalsplines <- tabPanel("Natural Splines", id="nsplines", class="fade", value = "nsplines",br(),
                    tags$head(
                      tags$style(HTML(paste(paste0("[for=nsp_coef", 1:length(col),"-coef]+span>.irs>.irs-single, [for=nsp_coef", 1:length(col), "-coef]+span>.irs-bar-edge, [for=nsp_coef", 1:length(col), "-coef]+span>.irs-bar {background: ", col, ";}")), collapse = " "))
                    ),
            column(4,
            fluidRow(
            sidebarPanel(class="input_class", id = "inputs.nsp", width = 12,
                h4("Input parameters"),  
                br(), br(),
                fluidRow(
                    column(6,
                        numericInput("nknots.nsp", "Number of internal knots", min = 1, max = 10, value = 2)
                    ), 
                    column(6, align="center", 
                           coef_rangeUI("nsp")
                    )
                ), 
                fluidRow(
                    column(6, 
                           wellPanel(uiOutput("boundary_knots.nsp"),
                           tags$div(id = 'placeholder_pos_nsp')), style="padding: 2px;"), 
                    column(6, wellPanel(tags$div(id = 'placeholder_coef_nsp'), style="padding: 2px;"))
                ),
                fluidRow(
                  div(style="display:inline-block; width: 20%; vertical-align: -150%;padding-left: 3%;",
                      materialSwitch(inputId = "adjust_intercept.nsp", label = "Adjust intercept automatically:", 
                                       status = "primary", right = FALSE)),
                      p("or: ", style="display:inline-block; width: 4%;"),
                      div(style="display:inline-block; width: 65%;",sliderInput("intercept.nsp",label="Intercept",min = 0, max = 40, value = 0, step = 0.1))
                ),
                fluidRow(
                    column(3, offset=0, 
                           materialSwitch(inputId = "add_y.nsp", label = "Response", status = "primary", right = FALSE, value=TRUE)
                    ), 
                    column(3, offset=0, 
                           materialSwitch(inputId = "add_loess.nsp", label = "Loess Smoother", status = "primary", right = FALSE, value=TRUE)
                    ),
                    column(3, offset=0, 
                           materialSwitch(inputId = "add_knots_pos.nsp", label = "Knot position", status = "primary", right = FALSE, value=TRUE)
                    ),
                    column(3, offset=0,
                           materialSwitch(inputId = "add_optfit.nsp", label = "Optimal fit", status = "primary", right = FALSE, value=TRUE)
                    )
                ), 
                fluidRow(
                  column(12, offset=0, actionButton("reset_input.nsp", "Reset inputs", class = "btn reset_btn"))
                )
)),
fluidRow(codeUI("code_nsp"))),
column(8,
mainPanel(width = 12, 
  fluidRow(
    column(8, 
           wellPanel(h4("Response function"), plotlyOutput("plot.nsp")),
           wellPanel(h4("Spline basis functions"), plotlyOutput("basis_plot.nsp", height = "200px"))
    ), 
    column(4, 
         popify(wellPanel(h4("Goodness of fit"), statsUI("stats_nsp")), 
             "Note", "Maximal R2 value is based on the set number of knots." )
         )
  )
) )
)