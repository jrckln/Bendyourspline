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

source("data/data.R")
#load modules:
tab_files <- list.files(path = "modules", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))

#colors for plotly
colors3 <- c("#1d3554","#DFE07C", "#7F8E39", "#42858C","#E48F1B","#570D32","#E5C616","#D33B44")
colors4 <- c("#4E84C4", "#E7B800","#F4EDCA", "#FC4E07","#D16103", "#C4961A","#FFDB6D", "#C3D7A4", "#52854C", "#293352", "#00AFBB")
col <- colors3

#load ressources:
#name future files according to hierarchy: Navbarpage prefix b; Methods prefix a for order
tab_files <- list.files(path = "tabs", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))
