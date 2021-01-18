methods <- 
    tabPanel("Methods", id="methods", shinyjs::useShinyjs(),
             # Details to input parameter especially possible values etc in data.R
            fluidRow(column(2,
                div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                            selectInput("variable", "Choose a variable pair:",names(data_list))
                        )), 
                    column(2, div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                            selectInput("sample.size", "Choose a sample size:",names(sample.sizes))
                        )), 
                    column(2, div(style = "font-size: 13px; padding: 10px 0px; margin: 0%", 
                            radioGroupButtons(inputId = "gender", label = "Sex:", choices = names(gender),status = "primary")
                        )), 
                    column(2, div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                            uiOutput("information.vars")
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