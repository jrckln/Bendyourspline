methods <- 
    tabPanel("Methods", id="methods", 
            fluidRow(column(4,
                div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                            selectInput("variable", "Choose a variable:",names(data.FP))
                        )), 
                    column(8, div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                            uiOutput("transformation")
                        ))), 
            fluidRow(
                column(width = 12,
                    tabsetPanel(
                         fp, 
                         bsplines, 
                         naturalsplines
                    )
                      )
            )
    )