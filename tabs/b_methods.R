methods <- 
    tabPanel("Methods", id="methods", 
             # Details to input parameter especially possible values etc in data.R
            fluidRow(column(2,
                div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                            selectInput("variable", "Choose a variable:",names(data_list))
                        )), 
                    column(2, div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                            uiOutput("transformation")
                        )), 
                    column(2, div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                            selectInput("sample.size", "Choose a sample size:",names(sample.sizes))
                        )), 
                    column(2, div(style = "font-size: 13px; padding: 10px 0px; margin: 0%", 
                            radioGroupButtons(inputId = "gender", label = "", choices = names(gender),status = "primary")
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