fp <- tabPanel("Fractional Polynomials", id="fp",class="active",value="fp",br(),
            sidebarPanel(class="input_class", 
                fluidRow(column(9, offset=0, h4("Input parameters"))
                ),
                h5("Transformation"),
                fluidRow(
                    div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                        sliderInput("shift", "Shift:",min = 0, max = 40, value = 0))
                ), 
                fluidRow(
                    div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                        sliderInput("scale", "Scale:",min = 1, max = 100, value = 0))
                )
),mainPanel(plotlyOutput("plot.FP"))) 