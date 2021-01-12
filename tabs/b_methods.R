methods <- 
    tabPanel("Methods", id="methods", 
            fluidRow(column(2,
                div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                            selectInput("variable", "Choose a variable:",names(data_list))
                        )), 
                    column(2, div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                            uiOutput("transformation")
                        )), 
                    column(8, div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                            selectInput("sample.size", "Choose a sample size:",names(sample.sizes))
                        ))
                
                ), 
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