out <- column(
    8,
    column(
        8,
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
            uiOutput('exerciseselection'),
            uiOutput("exerciseout")
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