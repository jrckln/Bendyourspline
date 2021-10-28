# Bendyourspline

The aim of this project to is show how nonlinear associations can be estimated with a statistical model using only simple extensions of the basic linear model. Moreover the shiny app allows for comparisons between fractional polynomials, b-splines and natural splines. Parameters and coefficients of each method can be changed and the resulting nonlinear effect is shown. In this way, one can explore how the coefficients affect the functional form of the association between the explanatory variable and the outcome.

In case of crashes or unexpected behavior please report the issue in this repository.
Requirements

The app is implemented using the shiny framework in R. To install all required dependencies, you can use

``` 
library(pacman)
p_load(c('shiny', 'plotly', 'shinyBS', 'shinyWidgets', 'shinyjs', 'splines',
    'mfp', 'stringr', 'reshape2', 'tidyverse', 'shinyhelper', 'shinycssloaders',
    'shinyjqui', 'cicerone', 'ggplot2', 'magrittr'))
``` 

## Links
https://clinicalbiometrics.shinyapps.io/Bendyourspline/
