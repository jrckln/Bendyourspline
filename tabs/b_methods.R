methods <- 
    tabPanel("Methods", shinyjs::useShinyjs(), 
            HTML('<canvas id="canvas_confetti"></canvas>
            <div id="exercise_modal" class="modal fade" tabindex="-1" data-backdrop="static" data-keyboard="false">
                 <div class="modal-dialog">
                    <!-- Modal content -->
                    <div class="modal-content">
                        <div class="modal-header">
                            <h4 class="modal-title">Congratulations!</h4>
                        </div>
                        <div class="modal-body"> <p> You finished this exercise successfully. Please select another exercise to continue. </p>
                        </div>
                        <div class="modal-footer">
                            <button type="button" class="btn btn-default" id="stop_modal_confetti" data-dismiss="modal">Dismiss</button>
                        </div>
                    </div>
                </div>
            </div>
                 '),
            bsCollapse(id = "collapseData", open = "Data Options",
                           bsCollapsePanel("Data Options",
                                        fluidRow(
                                            column(2,
                                                div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                                                    selectInput("variable", "Choose a variable pair:",names(data_list)) #%>% 
                                                        #helper(type = "markdown", title= "Data information",
                                                        #       content = "../www/VariablePairs")
                                                )),
                                            column(3, uiOutput("information_vars")),
                                            column(2, 
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
                                                   )
                                            ),
            style = "primary")),
            fluidRow(
                column(width = 12,
                    tabsetPanel(id="tabsetmethods",
                         fp, 
                         bsplines, 
                         naturalsplines
                    )
                      )
            )
    )