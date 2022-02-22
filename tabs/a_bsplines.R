bsplines <- tabPanel(
    "B-Splines",
    modal_help_input_BS,
    modal_help_basis_BS_NSP,
    tags$head(tags$style(HTML(
        paste(
            paste0(
                "[for=bs_coef",
                1:length(col),
                "-slider]+span>.irs>.irs-single, [for=bs_coef",
                1:length(col),
                "-slider]+span>.irs-bar-edge, [for=bs_coef",
                1:length(col),
                "-slider]+span>.irs-bar {background: ",
                col,
                ";}"
            ),
            collapse = " "
        )
    ))),
    div(
        class = "well",
        id = "inputs_bs",
        div(
            class = 'headerinfo',
            h4("Input parameters"),
            tags$a(icon('info-circle'), href = '#') %>% 
                bs_attach_modal(id_modal = "modal_help_input_BS")
        ),
        fluidRow(
            column(3,
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
            )
        ),
        fluidRow(column(6, wellPanel(
            span('Knot positions (quantiles)'), 
            tags$div(id = 'placeholder_pos_bs'), 
             materialSwitch(inputId = "showknots_bs",
                            label = "Show knots",
                            right = FALSE,
                            value = FALSE
                            )
        )),
        column(6, wellPanel(
            sliderPLUI('bs', label = 'Coefficients')
        ))),
        
    )
)