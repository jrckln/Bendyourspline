library(shinythemes)

#load ressources:
tab_files <- list.files(path = "tabs", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))
server_files <- list.files(path = "server_files", full.names = T, recursive = T)
suppressMessages(lapply(server_files, source))
