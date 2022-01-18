dataoptions <- bsCollapse(
            id = "collapseData",
            bsCollapsePanel(
                "Data Options",
                fluidRow(
                    column(
                        5,
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
                    column(5, uiOutput("information_vars")),
                    column(2,
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
                        12,
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
        )