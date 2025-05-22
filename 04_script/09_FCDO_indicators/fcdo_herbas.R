rm(list = ls())

library(openxlsx)
library(tidyverse)
library(srvyr)
library(illuminate)
library(analysistools)


# read data ---------------------------------------------------------------


hh_data <- openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",1)
hh_data <- hh_data |> mutate(
  strata = case_when(pop_group == "idp" & A_6_neighbourhood == "Horseed_CA11" ~ "idp-Horseed_CA11",
                     T~strata)
) |> filter(pop_group == "idp")

hh_data$pop_group |> unique()

all <- hh_data |> select(starts_with("F_5_cope_income."))  |>
  select(-contains(c("99","88")))|> names() |> dput()

low <- c("F_5_cope_income.rely_on_support_from_family",
         "F_5_cope_income.spend_savings",
         "F_5_cope_income.borrow_money" ,
         "F_5_cope_income.purchase_food_on_credit_borrow")

other <- all[!all %in% low]


hh_recoding <-hh_data |> mutate(
  rowsum_low = rowSums(hh_data[low],na.rm = T),
  rowsum_all_other = rowSums(hh_data[other],na.rm = T),
  lower_range_coping = case_when( rowsum_low == 0 & rowsum_all_other == 0 ~ NA_real_,
                                  rowsum_low >1 & rowsum_all_other == 0 ~ T,
                                  T~F)
)
hh_recoding$lower_range_coping |> table()

illuminate::survey_analysis(hh_recoding,
                            weights =T ,
                            weight_column ="survey_weights",
                            strata = "strata",
                            vars_to_analyze =  "lower_range_coping" )

