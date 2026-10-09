# Instala os pacotes usados no projeto (rodar uma vez).
pacotes <- c("DBI", "duckdb", "dplyr", "tidyr", "stringr", "stringi", "ggplot2",
             "scales", "nortest", "jsonlite", "readr", "testthat")
faltam <- setdiff(pacotes, rownames(installed.packages()))
if (length(faltam)) install.packages(faltam, repos = "https://cloud.r-project.org")
