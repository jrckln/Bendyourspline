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
        bsCollapse(id = "collapseData", open = "Data Options",
                   bsCollapsePanel("Data Options", 
                                fluidRow(
                                    column(4,
                                        div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                                            selectInput("variable", "Choose a variable pair:",names(data_list)) %>% 
                                                helper(type = "markdown",
                                                       content = "VariablePairs")
                                        )), 
                                    column(7, div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                                            uiOutput("information.vars")
                                        ))
                                    ),
                                fluidRow(
                                    column(4, 
                                           switchInput(inputId = "adv_settings", value = FALSE, label="Advanced settings", labelWidth = "120px")
                                           ), 
                                    column(8,
                                           conditionalPanel("input.adv_settings", 
                                                    column(4,popify(numericInput("seed", "Set seed:", value=14), "Seed", 
                                                            "Initializes random number generator for drawing random samples")), 
                                                    column(4, 
                                                        selectInput("sample.size", "Choose a sample size:",names(sample.sizes))),
                                                    column(4, 
                                                            radioGroupButtons(inputId = "gender", label = "Sex:", choices = names(gender),status = "primary")
                                                        ), 
                                                     )
                                           )
                                ),
                        style = "primary")),
            fluidRow(
                column(width = 12,
                    tabsetPanel(
                         fp, 
                         bsplines, 
                         naturalsplines, 
                         about_sub
                    )
                      )
            )
    )