bootstrapPage(includeCSS("www/style.css"),use_cicerone(), #introjsUI(),
              tags$head(HTML("<script type='text/javascript' src='nav.js'></script>
                              <script type='text/javascript' src='animation.js'></script>
                              <script src='https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/prism.min.js'></script>
                              <link rel='stylesheet' type='text/css' href='https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/themes/prism.min.css'/>
                              <script src='https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/components/prism-r.min.js'></script>
                              <script type='text/javascript' src='confetti.js'></script>
                             ")),
    navbarPage("Bend your (sp)line!", id = "navbar", position = "fixed-top",
                home, 
                methods,
                about, 
                tags$script(
                   HTML("var header = $('.navbar > .container-fluid');
                        header.append('<div style=\"float:right; padding-top: 8px; visibility: hidden;\"><button id=\"help\" type=\"button\" class=\"btn action-button\">Tour</button></div>')")
                 )
                      )
)# end bootstrappage