bsplines <- tabPanel(
    "B-Splines",
    br(),
    modal_help_input_BS,
    modal_help_basis_BS_NSP,
    tags$head(tags$style(HTML(
        paste(
            paste0(
                "[for=bs_coef",
                1:length(col),
                "-coef]+span>.irs>.irs-single, [for=bs_coef",
                1:length(col),
                "-coef]+span>.irs-bar-edge, [for=bs_coef",
                1:length(col),
                "-coef]+span>.irs-bar {background: ",
                col,
                ";}"
            ),
            collapse = " "
        )
    )),
    tags$style(HTML(
        paste0(
            ".label-primary[for=add_optfit_bs] {background: ",
            optfitcol,
            ";}"
        )
    )),
    tags$style(HTML(
        paste0(
            ".label-primary[for=add_loess_bs] {background: ",
            loesscol,
            ";}"
        )
    ))),
    sidebarPanel(
        class = "input_class",
        id = "inputs_bs",
        div(
            class = 'headerinfo',
            h4("Input parameters"),
            tags$a(icon('info-circle'), href = '#') %>% bs_attach_modal(id_modal = "modal_help_input_BS")
        ),
        fluidRow(
            column(4,
                   numericInput(
                       "degree.bs",
                       "Degree",
                       min = 1,
                       max = 4,
                       value = 2
                   ), ),
            column(
                4,
                numericInput(
                    "nknots.bs",
                    "Number of knots",
                    min = 1,
                    max = 10,
                    value = 2
                )
            ),
            column(4, align = "center",
                   coef_rangeUI("bs"))
        ),
        fluidRow(
            column(6, wellPanel(tags$div(id = 'placeholder_pos_bs')), style = "padding: 2px;"),
            column(6, wellPanel(
                tags$div(id = 'placeholder_coef_bs'), style = "padding: 2px;"
            ))
        ),
        fluidRow(column(12,
                        wellPanel(
                            uiOutput("intercept_slider_bs"),
                            actionButton(
                                inputId = "adjust_intercept.bs",
                                label = "Adjust automatically",
                                class = 'btn reset_btn'
                            )
                        ))),
        fluidRow(
            column(3, offset = 0,
                   div(
                       span('Data points'),
                       materialSwitch(
                           inputId = "add_y_bs",
                           label = "",
                           right = FALSE,
                           value = TRUE
                       )
                   )),
            column(3, offset = 0,
                   div(
                       span('LOESS Smoother'),
                       materialSwitch(
                           inputId = "add_loess_bs",
                           label = "",
                           right = FALSE,
                           value = TRUE
                       )
                   )),
            column(3, offset = 0,
                   div(
                       span('Knot position'),
                       materialSwitch(
                           inputId = "add_knots_pos.bs",
                           label = "",
                           right = FALSE,
                           value = TRUE
                       )
                   )),
            column(3, offset = 0,
                   div(
                       span('Optimal fit'),
                       materialSwitch(
                           inputId = "add_optfit_bs",
                           label = "",
                           right = FALSE,
                           value = TRUE
                       )
                   ))
        ),
        fluidRow(
            column(
                6,
                offset = 0,
                actionButton("reset_input_bs", "Reset inputs", class = "btn reset_btn"),
                bsTooltip(
                    id = "reset_input_bs",
                    title = "You mave have to click twice to reset dynamically inserted inputs.",
                    placement = "bottom",
                    trigger = "hover"
                )
            ),
            column(
                6,
                offset = 0,
                actionButton(
                    "set_optfit_bs",
                    "Set optimal fit",
                    class = "btn reset_btn",
                    style = "float:right"
                )
            )
        )
    ),
    column(8,
           mainPanel(width = 12,
                     fluidRow(
                         column(8,
                                jqui_sortable(div(
                                    wellPanel(
                                        div(
                                            class = 'headerinfo',
                                            h4("Response function"),
                                            tags$a(icon('info-circle'), href = '#') %>% bs_attach_modal(id_modal = "modal_help_response_function")
                                        ),
                                        withSpinner(plotlyOutput("plot.bs"), color = spinnercol, size = 1)
                                    ),
                                    wellPanel(
                                        div(
                                            class = 'headerinfo',
                                            h4("Spline basis functions"),
                                            tags$a(icon('info-circle'), href = '#') %>% bs_attach_modal(id_modal = "modal_help_basis_BS_NSP")
                                        ),
                                        withSpinner(
                                            plotlyOutput("basis_plot.bs", height = "200px"),
                                            color = spinnercol,
                                            size = 1
                                        )
                                    )
                                ))),
                         column(4,
                                jqui_sortable(div(
                                    wellPanel(
                                        id = 'bs',
                                        div(
                                            class = 'headerinfo',
                                            h4("Exercise"),
                                            tags$a(icon('info-circle'), href = '#') %>% bs_attach_modal(id_modal = "modal_help_exercises")
                                        ),
                                        selectInput("exercise_bs", "", names(exercises[['bs']]), selected =
                                                        "Basic"),
                                        actionButton('start_exercise_bs', 'Start'),
                                        uiOutput("next_exercise_bs")
                                    ),
                                    wellPanel(
                                        div(
                                            class = 'headerinfo',
                                            h4("Goodness of fit"),
                                            tags$a(icon('info-circle'), href = '#') %>% bs_attach_modal(id_modal = "modal_help_goodnessfit")
                                        ),
                                        statsUI("stats_bs")
                                    ),
                                    codeUI("code_bs")
                                )))
                     )))
)