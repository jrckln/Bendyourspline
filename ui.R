shinyUI(bootstrapPage(theme = "simplex.min.css",useShinyjs(), 
                      tags$head(tags$script(type="text/javascript", src = "code.js")),
                      navbarPage("Bend your spline!",
                          home, 
                          methods,
                          about
                      ),
    footer
)# end bootstrappage
)