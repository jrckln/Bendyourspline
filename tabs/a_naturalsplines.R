naturalsplines <- tabPanel(
  "Natural Splines",
  modal_help_input_NSP,
  tags$head(tags$style(HTML(
    paste(
      paste0(
        "[for=nsp-nsp",
        1:length(col),
        "-slider]+span>.irs>.irs-single, [for=nsp-nsp",
        1:length(col),
        "-slider]+span>.irs-bar-edge, [for=nsp-nsp",
        1:length(col),
        "-slider]+span>.irs-bar {background: ",
        col,
        ";}"
      )
    ), collapse = " "
  ))
  ),
  div(
    class = "well",
    id = "inputs_nsp",
    div(
      class = 'headerinfo',
      h4("Input parameters"),
      tags$a(icon('info-circle'), href = '#') %>%
        bs_attach_modal(id_modal = "modal_help_input_NSP")
    ),
    fluidRow(
      column(
        4,
        numericInput(
          "nknots.nsp",
          "Number of internal knots",
          min = 1,
          max = 10,
          value = 2
        )
      )
    ),
    fluidRow(column(6,
                    wellPanel(
                      span('Boundary knot positions (quantiles)'), 
                      sliderInput("boundary1.nsp", '', min=0, max=100, 
                                  value=5, step=1, ticks = FALSE),
                      sliderInput("boundary2.nsp", '', min=0, 
                                  max=100, value=95, step=1, ticks = FALSE),
                      span('Internal knot positions (quantiles)'), 
                      tags$div(id = 'placeholder_pos_nsp'), 
                      materialSwitch(inputId = "showknots_nsp",
                            label = "Show knots",
                            right = FALSE,
                            value = FALSE
                            )
                    )),
             column(6, wellPanel(
               sliderPLUI('nsp', label = 'Coefficients')
             )))
  )
)