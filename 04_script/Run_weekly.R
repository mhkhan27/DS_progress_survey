rm(list = ls())

library(cleaningtools)
library(tidyverse)
library(analyticaid)
library(openxlsx)
library(sf)
library(plotly)
library(leaflet)
write_file <- c(T,F)[1]
source("04_script/write_function.R")

audit_path <- "01_input/01_audit/aB6h5dgnuwEVzP3Kgch2i4_2024_06_13_07_26_08.zip"
date_log_previous<-read.csv("05_date_log/2024_06_12_date_log.csv")

rmarkdown::render("weekly_monitoring_report.Rmd")

if(write_file ==T){
  file.copy(from = "weekly_monitoring_report.html",
            to = paste0("03_output/01_monitoring_reports/",
                        str_replace_all(Sys.Date(),"-","_"),
                        "_daily_monitoring_report.html"),
            overwrite = T)
}




