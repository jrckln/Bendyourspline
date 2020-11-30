library(shiny)
library(plotly)
library(shinyBS)
library(ggplot2)
library(splines)

source("function_transformation_FP.R")
source("data/data.R")

#colors for plotly
col <- c("#4E84C4", "#E7B800", "#FC4E07","#C4961A","#D16103",  "#F4EDCA","#FFDB6D", "#C3D7A4", "#52854C", "#293352", "#00AFBB")

#load ressources:
#name future files according to hierarchy: Navbarpage prefix b; Methods prefix a for order
tab_files <- list.files(path = "tabs", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))