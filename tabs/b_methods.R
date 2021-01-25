methods <- 
    tabPanel("Methods", id="methods", shinyjs::useShinyjs(),
            tags$head(
                  tags$style(
                    HTML("
                        .shiny-notification { 
                        top: 0; 
                        bottom: unset; 
                        left: 0; 
                        right: 0; 
                        margin-left: auto; 
                        margin-right: auto; 
                        width: 100%; 
                        max-width: 450px;
                        background: #4bd67b;
                        color: #fff;
                         }
                         "
                        )
                    )
         ),
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
                column(4, 
                       switchInput(inputId = "adv_settings",value = FALSE, label="Advanced settings", labelWidth = "120px")
                       ), 
                column(8,
                       conditionalPanel("input.adv_settings", 
                                 numericInput("seed", "Set seed:", value=14)
                                 )
                       )
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