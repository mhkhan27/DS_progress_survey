rm(list = ls())
library(tidyverse)
library(openxlsx)



# v1 ----------------------------------------------------------------------

hh_v1<- read.csv("01_input/02_raw_data/01_version_1/hh_v1.csv")
indv_v1<- read.csv("01_input/02_raw_data/01_version_1/indv_v1.csv")


# v2 ----------------------------------------------------------------------

hh_v2<- read.csv("01_input/02_raw_data/02_version_2/hh_data.csv")
indv_v2<- read.csv("01_input/02_raw_data/02_version_2/indv.csv")


# check hh col ------------------------------------------------------------
names(hh_v1) %in% names(hh_v2) |> table()
names(indv_v1) %in% names(indv_v2) |> table()ne

names(hh_v2) %in% names(hh_v1) |> table()
names(indv_v2) %in% names(indv_v1) |> table()


# merging <-  -------------------------------------------------------------

hh_data <- hh_v1 |> bind_rows(hh_v2)
indv_data <- indv_v1 |> bind_rows(indv_v2)





