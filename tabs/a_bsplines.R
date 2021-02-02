bsplines <- tabPanel("B-Splines", id="bsplines", class="fade", value = "bsplines",br(),
                    tags$head(
                        tags$script(
                            #to opt out of shiny default optimiation (cares only of last value) but we want to trigger
                            #everytime button is clicked but get the id: {priority: 'event'}
                            HTML(
                            "$(document).on('click', '.minus', function () {
                                Shiny.setInputValue('last_btn_minus',this.id, {priority: 'event'});
                             });
                             $(document).on('click', '.plus', function () {
                                Shiny.setInputValue('last_btn_plus',this.id, {priority: 'event'});
                             });
                            "
                    )), 
                        tags$style(HTML("
                                        #placeholder_coef_bs .form-group.shiny-input-container{
                                        display: inline-block;
                                        }
                                        "))
                    ),
                     #gets the id of the last button clicked of class "minus" - to determine which coefficient id should be changed
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
    plotlyOutput("plot.bs"), 
    uiOutput("intercept.bs")
  )
) 