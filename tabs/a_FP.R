fp <- tabPanel(
  "Fractional Polynomials",
  modal_help_input_FP,
  modal_help_basis_FP,
  modal_help_formula_FP,
  tags$head(tags$style(HTML(
    paste(
      paste0(
        "[for=fp-fp",
        1:2,
        "-slider]+span>.irs>.irs-single, [for=fp-fp",
        1:2,
        "-slider]+span>.irs-bar-edge, [for=fp-fp",
        1:2,
        "-slider]+span>.irs-bar {background: ",
        col[1:2],
        ";}"
      ),
      collapse = " "
    )
  )),
  tags$style(HTML(
    paste0(
      ".label-primary[for=add_optfit_fp] {background: ",
      optfitcol,
      ";}"
    )
  )),
  tags$style(HTML(
    paste0(
      ".label-primary[for=add_loess_fp] {background: ",
      loesscol,
      ";}"
    )
  ))),
  div(
    class = "well",
    id = "inputs_fp",
    div(
      class = 'headerinfo',
      h4("Input parameters"),
      tags$a(icon('info-circle'), href = '#') %>%
        bs_attach_modal(id_modal = "modal_help_input_FP")
    ),
    fluidRow(
      column(
        4,
        offset = 0,
        wellPanel(
            span('Powers'),
          id = "powers_fp",
          sliderTextInput(
            inputId = "power1_fp",
            label = "First",
            choices = c(-2, -1, -0.5, 0, 0.5, 1, 2, 3),
            selected = 1
          ),
          sliderTextInput(
            inputId = "power2_fp",
            label = "Second",
            choices = c(-2, -1, -0.5, 0, 0.5, 1, 2, 3),
            selected = 1
          )
        )
      ),
      column(
        8,
        offset = 0,
        wellPanel(id = "coefficients_fp",
                  sliderPLUI('fp', label = 'Coefficients')
        )
      )
    )
  )
)
