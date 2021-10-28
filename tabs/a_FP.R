fp <- tabPanel(
  "Fractional Polynomials",
  br(),
  modal_help_input_FP,
  modal_help_basis_FP,
  modal_help_transformation_FP, 
  modal_help_formula_FP, 
  tags$head(tags$style(HTML(
    paste(
      paste0(
        "[for=val_coef",
        1:2,
        "_fp-coef]+span>.irs>.irs-single, [for=val_coef",
        1:2,
        "_fp-coef]+span>.irs-bar-edge, [for=val_coef",
        1:2,
        "_fp-coef]+span>.irs-bar {background: ",
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
  column(4,
         fluidRow(
           sidebarPanel(
             class = "input_class",
             id = "inputs_fp",
             width = 12,
             div(
               class = 'headerinfo',
               h4("Input parameters"),
               coef_rangeUI("fp"),
               tags$a(icon('info-circle'), href = '#') %>% bs_attach_modal(id_modal = "modal_help_input_FP")
             ),
             fluidRow(column(
               4,
               div(style = "padding: 10px 0px; margin:0%",
                   HTML("Power:"))
             ),
             column(
               8, offset = 0,
               div(style = "padding: 10px 0px; margin:0%",
                   HTML("Coefficient:"))
             )),
             fluidRow(
               column(
                 4,
                 offset = 0,
                 wellPanel(
                   id = "powers_fp",
                   sliderTextInput(
                     inputId = "power1.fp",
                     label = "First",
                     choices = c(-2, -1, -0.5, 0, 0.5, 1, 2, 3),
                     selected = 1
                   ),
                   sliderTextInput(
                     inputId = "power2.fp",
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
                           sliderplUI("val_coef1_fp"),
                           sliderplUI("val_coef2_fp"))
               )
             ),
             fluidRow(column(
               12,
               wellPanel(
                 id = "intercept_fp_all",
                 uiOutput("intercept_slider_fp"),
                 actionButton(
                   inputId = "adjust_intercept.fp",
                   label = "Adjust automatically",
                   class = 'btn reset_btn'
                 )
               )
             )),
             fluidRow(
               column(4, offset = 0,
                      div(
                        span('Data points'),
                        materialSwitch(
                          inputId = "add_y_fp",
                          label = "",
                          right = FALSE,
                          value = TRUE
                        )
                      )),
               column(4, offset = 0,
                      div(
                        span('LOESS Smoother'),
                        materialSwitch(
                          inputId = "add_loess_fp",
                          label = "",
                          right = FALSE,
                          value = TRUE
                        )
                      )),
               column(4, offset = 0,
                      div(
                        span('Optimal fit'),
                        materialSwitch(
                          inputId = "add_optfit_fp",
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
                 actionButton("reset_input_fp", "Reset inputs", class = "btn reset_btn"),
                 bsTooltip(
                   id = "reset_input_fp",
                   title = "You mave have to click twice to reset dynamically inserted inputs.",
                   placement = "bottom",
                   trigger = "hover"
                 )
               ),
               column(
                 6,
                 offset = 0,
                 actionButton(
                   "set_optfit_fp",
                   "Set optimal fit",
                   class = "btn reset_btn",
                   style = "float:right"
                 )
               )
             )
           )
         )),
  column(8,
         column(8,
                jqui_sortable(
                  div(
                    wellPanel(
                      id = 'response_fp',
                      div(
                        class = "headerinfo",
                        h4("Response function"),
                        tags$a(icon('info-circle'), href = '#') %>% bs_attach_modal(id_modal = "modal_help_response_function")
                      ),
                      withSpinner(plotlyOutput("plot.fp"), color = spinnercol, size = 1)
                    ),
                    wellPanel(
                      id = 'basis_fp',
                      div(
                        class = "headerinfo",
                        h4("Fractional polynomials"),
                        tags$a(icon('info-circle'), href = '#') %>% bs_attach_modal(id_modal = "modal_help_basis_FP")
                      ),
                      withSpinner(
                        plotlyOutput("basis_plot.fp", height = "200px"),
                        color = spinnercol,
                        size = 1
                      )
                    )
                  )
                )),
         column(4,
                jqui_sortable(
                  div(
                    wellPanel(
                      id = 'fp',
                      div(
                        class = "headerinfo",
                        h4("Exercise"),
                        tags$a(icon('info-circle'), href = '#') %>% bs_attach_modal(id_modal = "modal_help_exercises")
                      ),
                      selectInput("exercise_fp", "", names(exercises[['fp']]), selected = "Basic"), 
                      actionButton('start_exercise_fp', 'Start'),
                      uiOutput("next_exercise_fp")
                    ),
                    wellPanel(
                      id = 'transformation_fp',
                      div(
                        class = 'headerinfo',
                        h4("Transformation: "), 
                        tags$a(icon('info-circle'), href = '#') %>% bs_attach_modal(id_modal = "modal_help_transformation_FP")
                      ),
                      uiOutput("transformation.fp")
                    ),
                    wellPanel(
                      id = 'goodness_fit_fp', 
                      div(
                        class = 'headerinfo', 
                        h4("Goodness of fit"), 
                        tags$a(icon('info-circle'), href = '#') %>% bs_attach_modal(id_modal = "modal_help_goodnessfit")
                      ),
                      statsUI("stats_fp")
                    ), 
                    wellPanel(
                      id = 'formula_fp', 
                      div(
                        class = 'headerinfo', 
                        h4("Formula: "), 
                        tags$a(icon('info-circle'), href = '#') %>% bs_attach_modal(id_modal = "modal_help_formula_FP")
                      ), 
                      uiOutput("formula.fp")
                    ),
                    codeUI("code_fp")
                  )
                )))
)
