bsplines <- tabPanel("B-Splines", id="bsplines", class="fade", value = "bsplines",br(),
            sidebarPanel(class="input_class", 
                fluidRow(column(9, offset=0, h4("Input parameters"))
                ),
                fluidRow(column(5, offset = 0,
                    div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                        selectInput("variable", "Choose a variable:",names(data.FP))
                    )
                ),column(5, offset = 0,
                    div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                        uiOutput("transformation")
                    )
                )),
                h5("Transformation"),
                fluidRow(
                    div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                        sliderInput("bspline.order", "Order:",min = 1, max = 3, value = 1))
                )
),mainPanel(plotlyOutput("plot.bspline")))