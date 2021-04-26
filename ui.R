bootstrapPage(withMathJax(),includeCSS("www/style.css"),
              tags$head(HTML("<script type='text/javascript' src='nav.js'></script>")),
    navbarPage("Bend your (sp)line!", id = "navbar", position = "fixed-top",
                          home, 
                          methods,
                          about
                      )
)# end bootstrappage