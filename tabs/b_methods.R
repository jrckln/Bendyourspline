methods <- 
    tabPanel("Methods", id="methods", 
             sidebarLayout(
                 sidebarPanel(
                     fluidRow(column(12, offset = 0,
                        div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                            selectInput("variable", "Choose a variable:",names(data.FP), selected = "X")
                        ))
                    ), 
                    fluidRow(column(12, offset = 0,
                        div(style = "font-size: 13px; padding: 10px 0px; margin:0%",
                            uiOutput("transformation")
                        )))
                 , width=2),
                 mainPanel(
                     tabsetPanel(
                     fp, 
                     bsplines, 
                     naturalsplines
                    )
                 , width=10)
             )
             
)