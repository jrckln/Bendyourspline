about <- tabPanel("About", value="about", id='about',
            includeHTML("www/about.html")
)

explanatory_comments <- tabPanel("Explanatory comments", value="explanatory_comments", id='explanatory_comments',
                                 br(), br(),
            includeMarkdown("www/explanatory_comments.md")
)