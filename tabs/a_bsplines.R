bsplines <- tabPanel("B-Splines", id="bsplines", class="fade", value = "bsplines",br(),
                    tags$head(
                        tags$script(
                            #to opt out of shiny default optimiation (cares only of last value) but we want to trigger
                            #everytime button is clicked but get the id: {priority: 'event'}
                            HTML(
                            "$(document).on('click', '.minus_bs', function () {
                                Shiny.setInputValue('last_btn_minus_bs',this.id, {priority: 'event'});
                             });
                             $(document).on('click', '.plus_bs', function () {
                                Shiny.setInputValue('last_btn_plus_bs',this.id, {priority: 'event'});
                             });
                            "
                    )),
                        tags$style(HTML("
                                        #placeholder_coef_bs .form-group.shiny-input-container{
                                        display: inline-block;
                                        }
                                        ")), 
                    tags$style(HTML("[for=bs_coef1_inner]+span>.irs>.irs-single, [for=bs_coef1_inner]+span>.irs-bar-edge, [for=bs_coef1_inner]+span>.irs-bar {
                                                  background: #4E84C4;}")),
                    tags$style(HTML("[for=bs_coef2_inner]+span>.irs>.irs-single, [for=bs_coef2_inner]+span>.irs-bar-edge, [for=bs_coef2_inner]+span>.irs-bar {
                                                  background: #E7B800;}")),
                    tags$style(HTML("[for=bs_coef3_inner]+span>.irs>.irs-single, [for=bs_coef3_inner]+span>.irs-bar-edge, [for=bs_coef3_inner]+span>.irs-bar {
                                                  background: #FC4E07;}")),
                    tags$style(HTML("[for=bs_coef4_inner]+span>.irs>.irs-single, [for=bs_coef4_inner]+span>.irs-bar-edge, [for=bs_coef4_inner]+span>.irs-bar {
                                                  background: #C4961A;}")),
                    tags$style(HTML("[for=bs_coef5_inner]+span>.irs>.irs-single, [for=bs_coef5_inner]+span>.irs-bar-edge, [for=bs_coef5_inner]+span>.irs-bar {
                                                  background: #D16103;}")),
                    tags$style(HTML("[for=bs_coef6_inner]+span>.irs>.irs-single, [for=bs_coef6_inner]+span>.irs-bar-edge, [for=bs_coef6_inner]+span>.irs-bar {
                                                  background: #F4EDCA;}")),
                    tags$style(HTML("[for=bs_coef7_inner]+span>.irs>.irs-single, [for=bs_coef7_inner]+span>.irs-bar-edge, [for=bs_coef7_inner]+span>.irs-bar {
                                                  background: #FFDB6D;}")),
                    tags$style(HTML("[for=bs_coef8_inner]+span>.irs>.irs-single, [for=bs_coef8_inner]+span>.irs-bar-edge, [for=bs_coef8_inner]+span>.irs-bar {
                                                  background: #C3D7A4;}")),
                    tags$style(HTML("[for=bs_coef9_inner]+span>.irs>.irs-single, [for=bs_coef9_inner]+span>.irs-bar-edge, [for=bs_coef9_inner]+span>.irs-bar {
                                                  background: #52854C;}")), 
                    tags$style(HTML("[for=bs_coef10_inner]+span>.irs>.irs-single, [for=bs_coef10_inner]+span>.irs-bar-edge, [for=bs_coef10_inner]+span>.irs-bar {
                                                  background: #293352;}")), 
                    tags$style(HTML("[for=bs_coef11_inner]+span>.irs>.irs-single, [for=bs_coef11_inner]+span>.irs-bar-edge, [for=bs_coef11_inner]+span>.irs-bar {
                                                  background: #00AFBB;}"))
                    ),
                     #gets the id of the last button clicked of class "minus" - to determine which coefficient id should be changed
             sidebarPanel(class="input_class", id = "inputs.bs",
                fluidRow(column(7, offset=0, h4("Input parameters")),
                         column(5, offset=0,
                                div(p("Coefficient range:", style="display: inline-block;"),
                                  actionButton("decrease_range.bs", icon("minus"), style="display: inline-block;"),
                                  actionButton("increase_range.bs", icon("plus"), style="display: inline-block;")
                                  )
                                )
                ),
                br(), br(),
                fluidRow(
                    column(6,
                        sliderInput("degree.bs", "Degree", min=1, max=4, value=1, step=1),
                        numericInput("nknots.bs", "Number of internal knots", min=1, value = 2, width="100px")
                    ),
                    column(6,
                    div(style="padding-left: 3%;",
                      materialSwitch(inputId = "adjust_intercept.bs", label = "Adjust intercept automatically:",
                                       status = "primary", right = FALSE)),
                      p("or: ", style=""),
                      div(style="",sliderInput("intercept.bs",label="Intercept",min = 0, max = 40, value = 0, step = 0.1))
                    )
                ),
                fluidRow(
                    column(6, tags$div(id = 'placeholder_pos_bs')),
                    column(6, tags$div(id = 'placeholder_coef_bs'))
                ),
                fluidRow(
                    column(3, offset=0,
                           materialSwitch(inputId = "add_y.bs", label = "Add response", status = "primary", right = FALSE)
                    ),
                    column(3, offset=0,
                           materialSwitch(inputId = "add_mean.bs", label = "Add mean", status = "primary", right = FALSE)
                    ),
                    column(3, offset=0,
                           materialSwitch(inputId = "add_knots_pos.bs", label = "Add knots position", status = "primary", right = FALSE)
                    )
                ),
                fluidRow(
                  column(12, offset=0, actionButton("reset_input.bs", "Reset inputs"))
                )
),mainPanel(
    plotlyOutput("plot.bs"),
    br(),
     fluidRow(
        column(3,
               wellPanel(uiOutput("intercept.bs"), style="height:100px;")
        ),
        column(4,
               wellPanel(uiOutput("stats.bs"), style="height:100px;")
        )
    ),
    # br(),
    plotlyOutput("basis_plot.bs", height = "200px")
  )
) 