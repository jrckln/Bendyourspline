methods <- 
    tabPanel("Methods", id="methods", shinyjs::useShinyjs(),
                bsCollapse(id = "collapseData", open = "Data Options",
                           bsCollapsePanel("Data Options", 
                                        fluidRow(
                                            column(2,
                                                div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                                                    selectInput("variable", "Choose a variable pair:",names(data_list)) %>% 
                                                        helper(type = "markdown", title= "Data information",
                                                               content = "VariablePairs")
                                                )), 
                                            column(3, 
                                                   materialSwitch(inputId = "adv_settings", value = FALSE, label="Advanced settings")
                                                   ), 
                                            column(5,
                                                   conditionalPanel("input.adv_settings", 
                                                            column(4,popify(numericInput("seed", "Set seed:", value=14), "Seed", 
                                                                    "Initializes random number generator for drawing random samples")), 
                                                            column(4, 
                                                                selectInput("sample.size", "Choose a sample size:",names(sample.sizes), selected = "20%")),
                                                            column(4, 
                                                                    radioGroupButtons(inputId = "gender", label = "Sex:", choices = names(gender),status = "primary", selected = "Both")
                                                                ), 
                                                             )
                                                   ),
                                            column(1, div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                                                    uiOutput("information.vars")
                                                ))
                                            ),
            style = "primary")),
            fluidRow(
                column(width = 12,
                    tabsetPanel(id="tabsetmethods",
                        #introBox("", data.step = 2, data.intro = "Select a nonlinear modelling technique.", data.position = "bottom"),
                         fp, 
                         bsplines, 
                         naturalsplines, 
                         about_sub
                    )
                      )
            )
    )