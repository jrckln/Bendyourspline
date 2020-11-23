shinyUI(bootstrapPage(theme = shinytheme("simplex"),
                tags$head( #to include github issues; wanted?
                      HTML('<script async defer src="https://buttons.github.io/buttons.js"></script>')
                ), 
                tags$head(tags$style(HTML("
                            body {color:black;padding-top:5%; padding-bottom:10%;}
                            .navbar {height: 7%; min-height:7% !important;} 
                             #navbar {height: 100%; float:right; text-align:center; font-size: large;text-align: center;}
                            .container-fluid {width: 100%; height: 100%;}
                            .container-fluid .navbar-header {height: 100%;}
                            .container-fluid .navbar-header .navbar-brand {height: 100%;}
                            .container-fluid .tab-content {width: 100%;}
                            }")), 
                          tags$script('
                                        var width = 0;
                                        $(document).on("shiny:connected", function(e) {
                                            width = window.innerWidth;
                                            Shiny.onInputChange("width", width);
                                        });
                                        $(window).resize(function(e) {
                                            width = window.innerWidth;
                                            Shiny.onInputChange("width", width);
                                        });
                                    ')
                 ),
    navbarPage(title = HTML("
                            <div style='height:100%; width:100%;'>
                            <a href='https://cemsiis.meduniwien.ac.at/kb/'>
                                    <img alt='Meduni Vienna CeMSIIS KB' src='logo_institute.png' style='max-height:100%; max-width:100%;'>
                            </a>
                            </div>
                            "),
               position = "fixed-top",
               id = "navbar",
               home,
               FP,
               splines,
               about
    ), #end navpage
    footer
)# end bootstrappage
)