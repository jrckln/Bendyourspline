out <- column(
    8,
    
    column(
        8,
        bsCollapse(
            id = "collapseData",
            bsCollapsePanel(
                "Data Options",
                fluidRow(
                    column(
                        2,
                        div(
                            style = "font-size: 13px; padding: 10px 0px; margin:0%",
                            selectInput(
                                "variable",
                                "Choose a variable pair:",
                                names(data_list),
                                selected = 'No data'
                            )
                        )
                    ),
                    column(3, uiOutput("information_vars")),
                    column(
                        2,
                        conditionalPanel(
                            "input.variable != 'No data'",
                            materialSwitch(
                                inputId = "adv_settings",
                                value = FALSE,
                                label = "Advanced settings"
                            )
                        )
                    ),
                    column(
                        5,
                        conditionalPanel(
                            "input.adv_settings & input.variable != 'No data'",
                            column(
                                4,
                                popify(
                                    numericInput("seed", "Set seed:", value = 14),
                                    "Seed",
                                    "Initializes random number generator for drawing random samples"
                                )
                            ),
                            column(
                                4,
                                selectInput(
                                    "sample.size",
                                    "Choose a sample size:",
                                    names(sample.sizes),
                                    selected = "20%"
                                )
                            ),
                            column(
                                4,
                                radioGroupButtons(
                                    inputId = "gender",
                                    label = "Sex:",
                                    choices = names(gender),
                                    status = "primary",
                                    selected = "Both"
                                )
                            ),
                        )
                    )
                ),
                style = "primary"
            )
        ),
        div(
            wellPanel(
                id = 'response',
                div(
                    class = "headerinfo",
                    h4("Response function"),
                    tags$a(icon('info-circle'), href = '#') %>% bs_attach_modal(id_modal = "modal_help_response_function")
                ),
                withSpinner(
                    plotOutput("responseplot"),
                    color = spinnercol,
                    size = 1
                )
            ),
            wellPanel(
                id = 'basis_fp',
                div(
                    class = "headerinfo",
                    h4("Basis functions"),
                    tags$a(icon('info-circle'), href = '#') %>% bs_attach_modal(id_modal = "modal_help_basis_FP")
                ),
                withSpinner(
                    plotOutput("basisplot", height = "200px"),
                    color = spinnercol,
                    size = 1
                )
            )
        )
    ),
    column(
        4,
        wellPanel(
            id = 'fp',
            div(
                class = "headerinfo",
                h4("Exercise"),
                tags$a(icon('info-circle'), href = '#') %>%
                    bs_attach_modal(id_modal = "modal_help_exercises")
            ),
            selectInput("exercise_fp", "", names(exercises[['fp']]), selected = "Basic"),
            actionButton('start_exercise_fp', 'Start'),
            uiOutput("next_exercise_fp")
        ),
        conditionalPanel(
            "input.variable != 'No data'",
            wellPanel(
                id = 'statspanel',
                div(
                    class = 'headerinfo',
                    h4("Goodness of fit"),
                    tags$a(icon('info-circle'), href = '#') %>%
                        bs_attach_modal(id_modal = "modal_help_goodnessfit")
                ),
                statsUI("stats")
            )
        ),
        conditionalPanel(
            "input.inputindividual == 'Fractional Polynomials'",
            wellPanel(
                id = 'formula_fp',
                div(
                    class = 'headerinfo',
                    h4("Formula: "),
                    tags$a(icon('info-circle'), href = '#') %>% bs_attach_modal(id_modal = "modal_help_formula_FP")
                ),
                uiOutput("formula.fp")
            )
        ),
        codeUI("code_fp")
    )
)