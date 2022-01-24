rm(list = ls(), envir = globalenv()) ## to prevent cross over from old runs

library(shiny)
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
library(ggplot2)
library(magrittr)
library(bsplus)

col <- c("#2F4B7C", "#CE2029", "#FFA600","#2FA39B", "#97D8C4", "#F0E825", "#91B5D6", "#6BCFE8", "#F0CF44", "#136F63")
optfitcol <- "#F0CF44"
spinnercol <- "#800000"
loesscol <- "#136F63"

source("data/data.R")
source("data/exercises.R")
#load modules:
tab_files <- list.files(path = "modules", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))

modal_help_input_FP <-
  bs_modal(
    id = "modal_help_input_FP",
    title = "Input Parameters - Fractional Polynomials",
    body = includeMarkdown("www/help_input_FP.md"),
    size = "medium"
  )

modal_help_basis_BS_NSP <-
  bs_modal(
    id = "modal_help_basis_BS_NSP",
    title = "Spline basis functions",
    body = includeMarkdown("www/help_basis_BS_NSP.md"),
    size = "medium"
  )

modal_help_basis_FP <-
  bs_modal(
    id = "modal_help_basis_FP",
    title = "Fractional polynomials",
    body = includeMarkdown("www/help_basis_FP.md"),
    size = "medium"
  )

modal_help_exercises <-
  bs_modal(
    id = "modal_help_exercises",
    title = "Exercises",
    body = includeMarkdown("www/help_exercises.md"),
    size = "medium"
  )

modal_help_response_function <- 
  bs_modal(
    id = "modal_help_response_function",
    title = "Response function",
    body = includeMarkdown("www/help_response_function.md"),
    size = "medium"
  )

modal_help_goodnessfit <- 
  bs_modal(
    id = "modal_help_goodnessfit",
    title = "Goodness of fit",
    body = includeMarkdown("www/help_goodnessfit.md"),
    size = "medium"
  )

modal_help_formula_FP <- 
  bs_modal(
    id = "modal_help_formula_FP",
    title = "Formula",
    body = includeMarkdown("www/help_formula_FP.md"),
    size = "medium"
  )

modal_help_input_BS <-
  bs_modal(
    id = "modal_help_input_BS",
    title = "Input Parameters - B-Splines",
    body = includeMarkdown("www/help_input_BS.md"),
    size = "medium"
  )

modal_help_input_NSP <-
  bs_modal(
    id = "modal_help_input_NSP",
    title = "Input Parameters - Natural Splines",
    body = includeMarkdown("www/help_input_NSP.md"),
    size = "medium"
  )

#load ressources:
#name future files according to hierarchy: Navbarpage prefix b; Methods prefix a for order
tab_files <- list.files(path = "tabs", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))

