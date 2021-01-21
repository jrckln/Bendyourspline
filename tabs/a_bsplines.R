bsplines <- tabPanel("B-Splines", id="bsplines", class="fade", value = "bsplines",br(),
            sidebarPanel(class="input_class", 
                fluidRow(column(9, offset=0, h4("Input parameters"), 
                            numericInput("order.bsplines", "Order:",min=1, max=3,value = 1)
                                )
                ), 
                fluidRow(column(9, offset=0,
                            actionButton('insertBtn', 'Insert knot'), 
                            actionButton('removeBtn', 'Remove knot'), 
                                )
                ),
                fluidRow(
                    tags$div(id = 'placeholder_bsplines_min') #here comes the default number of input sliders one for position and one for coef
                ), 
                fluidRow(
                    tags$div(id = 'placeholder_bsplines') #here comes additional input slider
                )
            ),
            mainPanel(
                plotlyOutput("plot.bsplines")
            )
)