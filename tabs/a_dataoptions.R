dataoptions <- div(
    id = "collapseData",
    class = 'well',
    div(
        class = 'headerinfo',
        HTML(
            '
        <button id="showdataoptions" style="width:95%; text-align: left; border-style: none none solid; padding: 0px;
                border-radius: 0px;" type="button" class="btn btn-default action-button">
        <h4>Data Options</h4>
        </button>'
        ),
        tags$a(icon('info-circle'), href = '#') %>%
            bs_attach_modal(id_modal = "modal_help_input_data")
    ),
    conditionalPanel(
        'input.showdataoptions % 2 == 1',
        fluidRow(column(
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
        column(7, uiOutput("information_vars"))),
        conditionalPanel(
                "input.variable != 'No data'",
                materialSwitch(
                    inputId = "adv_settings",
                    value = FALSE,
                    label = "Advanced settings"
                )
            ),
        fluidRow(
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
        ))
    )
)
