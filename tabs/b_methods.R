methods <-
    tabPanel(
        "Methods",
        shinyjs::useShinyjs(),
        tags$head(tags$style(type = 'text/css', 
            paste0(
                ".label-default[for='addoptfit'] {background-color: ",
                optfitcol,
                ";}", 
                ".label-default[for='addloess'] {background-color: ",
                loesscol,
                ";}"
            )
        )),
        modal_help_exercises,
        modal_help_goodnessfit,
        modal_help_response_function,
        modal_help_basis_BS_NSP,
        modal_help_input_data,
        HTML(
            '<canvas id="canvas_confetti"></canvas>
            <div id="exercise_modal" class="modal fade" tabindex="-1" data-backdrop="static" data-keyboard="false">
                 <div class="modal-dialog">
                    <!-- Modal content -->
                    <div class="modal-content">
                        <div class="modal-header">
                            <h4 class="modal-title">Congratulations!</h4>
                        </div>
                        <div class="modal-body"> <p> You finished this exercise successfully. Please select another modelling method to continue. </p>
                        </div>
                        <div class="modal-footer">
                            <button type="button" class="btn btn-default" id="stop_modal_confetti" data-dismiss="modal">Dismiss</button>
                        </div>
                    </div>
                </div>
            </div>
                 '
        ),
        
        fluidRow(column(
            width = 4,
            dataoptions,
            tabsetPanel(id = "inputsindividual",
                        fp,
                        bsplines,
                        naturalsplines
                        ),
            wellPanel(
                conditionalPanel(
                    "input.variable != 'No data'",
                    wellPanel(
                        id = "intercept_all",
                        sliderPLUI('interceptslider'),
                        actionButton(
                            inputId = "adjustintercept",
                            label = "Adjust automatically",
                            class = 'btn reset_btn'
                        )
                    )
                ),
                fluidRow(conditionalPanel(
                    "input.variable != 'No data'",
                    div(id = 'options',
                        div(style = 'width: 20%;',
                            span('Data points'),
                            materialSwitch(
                                inputId = "addy",
                                label = "",
                                value = FALSE
                            )
                        ),
                        div(style = 'width: 20%;',
                            span('LOESS Smoother'),
                            materialSwitch(
                                inputId = "addloess",
                                label = "",
                                value = FALSE
                            )
                        ),
                        div(style = 'width: 20%;',
                            span('Optimal fit'),
                            materialSwitch(
                                inputId = "addoptfit",
                                label = "",
                                value = FALSE
                            )
                        ),
                        div(style = 'width: 20%;',
                            actionButton(
                                "setoptfit",
                                "Set optimal fit",
                                class = "btn reset_btn",
                                style = "float:right"
                            )
                        )
                    )
                )),
                
                fluidRow(
                    actionButton("resetinput", "Reset inputs", class = "btn reset_btn")
                    ,
                    bsTooltip(
                        id = "resetinput",
                        title = "You mave have to click twice to reset dynamically inserted inputs.",
                        placement = "bottom",
                        trigger = "hover"
                    )
                ),
                id = "inputall"
            )
        ),
        out)
    )