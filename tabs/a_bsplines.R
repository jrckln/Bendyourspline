bsplines <- tabPanel("B-Splines", id="bsplines", class="fade", value = "bsplines",br(),
            sidebarPanel(class="input_class", 
                fluidRow(column(9, offset=0, h4("Input parameters"), 
                            numericInput("order.bsplines", "Order:",min=1, max=3,value = 1)
                                )
                )
            ),
            mainPanel(
                plotlyOutput("plot.bsplines")
            )
)