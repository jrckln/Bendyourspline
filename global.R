rm(list = ls(), envir = globalenv()) ## to prevent cross over from old runs

library(shiny)
library(plotly)
library(shinyBS)
library(ggplot2)
library(shinyWidgets)
library(shinyjs)
library(splines)
library(mfp)
library(magrittr)
library(stringr)
library(reshape2)
library(tidyverse)
library(shinyhelper)
library(shinycssloaders)
library(shinyjqui)
library(cicerone)

source("data/data.R")
source("data/exercises.R")
#load modules:
tab_files <- list.files(path = "modules", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))

#colors for plotly
colors3 <- c("#1d3554","#DFE07C", "#7F8E39", "#42858C","#E48F1B","#570D32","#E5C616","#D33B44")
colors4 <- c("#4E84C4", "#E7B800","#F4EDCA", "#FC4E07","#D16103", "#C4961A","#FFDB6D", "#C3D7A4", "#52854C", "#293352", "#00AFBB")
col <- c(colors3[c(1, 2, 4, 5)], colors4[c(11,2, 9)])
optfitcol <- col[length(col)]
loesscol <-  col[length(col)-1]

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


