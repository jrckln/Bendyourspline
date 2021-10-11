rm(list = ls(), envir = globalenv()) ## to prevent cross over from old runs

library(pacman)

# requirements <- c('shiny', 'plotly', 'shinyBS', 'shinyWidgets', 'shinyjs', 'splines', 'mfp', 
#                   'stringr', 'reshape2', 'tidyverse', 'shinyhelper', 'shinycssloaders', 'shinyjqui', 
#                   'cicerone', 'ggplot2', 'magrittr')
# p_load(char = requirements)

library(shiny)
library(plotly)
library(shinyBS)
library(shinyWidgets)
library(shinyjs)
library(splines)
library(mfp)
library(stringr)
library(reshape2)
library(tidyverse)
library(shinyhelper)
library(shinycssloaders)
library(shinyjqui)
library(cicerone)
library(ggplot2)
library(magrittr)

#colors for plotly
#col <- c(colors3[c(1, 2, 4, 5)], colors4[c(11,2, 9)])
col <- c("#e6194B", "#ffe119", "#3cb44b", "#4363d8", "#f032e6", "#f58231", "#bfef45", "#42d4f4", "#911eb4", "#a9a9a9", 
         "#9A6324", "#808000", "#469990")
optfitcol <- "#dcbeff" 
#optfitcol <- col[length(col)]
#loesscol <-  col[length(col)-1]
spinnercol <- "#800000"
loesscol <- "#000075"

source("data/data.R")
source("data/exercises.R")
#load modules:
tab_files <- list.files(path = "modules", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))


#load ressources:
#name future files according to hierarchy: Navbarpage prefix b; Methods prefix a for order
tab_files <- list.files(path = "tabs", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))


guide <- Cicerone$
  new(mathjax = TRUE, opacity = 0.75)$ 
  step(
    "collapseData",
    "Data options",
    "Specify your variable pair to analyse. Click on header to collapse."
  )$
  step(
    "tabsetmethods",
    "Methods",
    "Here you can select a nonlinear modelling technique."
  )$
  step(
    "inputs_fp",
    "Input Panel",
    "Specify inputs needed here.",
    position = "right"
  )$
  step(
    "powers_fp",
    "Powers",
    "Use the sliders to change the exponents of the fractional polynomials. For FP1 and FP2 a set with 8 values was proposed: <br>
     $$S = {-2, -1, -0.5, \\log(x), 0.5, 1, 2, 3}$$. <br>
     For $$p1 = p2 = p$$ ('repeated powers') it is defined as $$FP2 = \\beta_1 x^p + \\beta_2 x^p \\cdot \\log(x)$$. 
     This defines 8 FP1 and 36 FP2 models.(https://mfp.imbi.uni-freiburg.de/fp)",
    position = "right"
  )$
  step(
    "coefficients_fp",
    "Coefficients",
    "Use the sliders to change the coefficients $$\\beta_1$$ and $$\\beta_2$$ of the fractional polynomials. Set the first coefficient to zero
    to obtain FP1.",
    position = "right"
  )$
  step(
    "val_coef1_fp-minus",
    "Finesse", 
    description  = "For a better control of the coefficients use these buttons.",
    position = "right"
  )$
  step(
    "intercept_fp_all",
    "Intercept", 
    description  = "You can either manually adjust the intercept, or let it adjust automatically in which case the sum of the squared residuals is minimized.",
    position = "right"
  )$
  step(
    "response_fp",
    "Response function", 
    description  = "This plot shows the response function computed based on your settings.",
    position = "bottom"
  )$
  step(
    "basis_fp",
    "Components", 
    description  = "This plot shows the individual components of the response function above.",
    position = "top"
  )$
  step(
    "exercises_fp",
    "Exercises", 
    description  = "Click start to begin an interactive exercise.",
    position = "left"
  )$
  step(
    "transformation_fp",
    "Transformation", 
    description  = "Here the preliminary transformation used is given.",
    position = "left"
  )$
  step(
    "goodness_fit_fp",
    "Goodness of fit", 
    description  = "See here for information about the goodness of your current fit.",
    position = "left"
  )$
  step(
    "formula_fp",
    "Formula", 
    description  = " ",
    position = "left"
  )


