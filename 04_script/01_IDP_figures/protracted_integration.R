rm(list = ls())
library(tidyverse)

# read data ---------------------------------------------------------------


hh <-openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",1)

hh_s <- hh |>
  filter(!is.na(D_1_4_year_left_origin))  |>
  filter(pop_group == "idp") |> mutate(
    month = case_when(D_1_4_1_season_left == "jilaal_dec_mar" ~ "01",
                      D_1_4_1_season_left == "gu_mar_jun" ~ "05",
                      D_1_4_1_season_left == "hagaa_jul_sep" ~ "08",
                      D_1_4_1_season_left == "deyr_sep_nov" ~"10"),
    start = as.Date(start),
    D_1_4_year_left_origin = as.Date(D_1_4_year_left_origin)
  ) |> mutate(
    date_left_orgin = as.Date(paste0(year(D_1_4_year_left_origin),"-",month,"-01"))
  ) |> mutate(
    month_duration = interval(date_left_orgin,start) %/% months(1)

  )  |> mutate(
    pct_protracted_idp_12_mnth = case_when(month_duration >12 ~
                                             paste0(City_Name,"::12_months_protracted"),
                                           T~ paste0(City_Name,"::No")),
    pct_protracted_idp_36_mnth =  case_when(month_duration >36 ~
                                              paste0(City_Name,"::36_months_protracted"),
                                            T~ paste0(City_Name,"::No")),
    pct_protracted_idp_60_mnth =  case_when(month_duration >60 ~
                                              paste0(City_Name,"::60_months_protracted"),
                                            T~ paste0(City_Name,"::No")),,
    i_integration = case_when(is.na(W_2_IntegrationLevel) ~NA_real_,
                              W_2_IntegrationLevel %in% c("4_integrated","5_very_integrated") ~ 1,
                              T~0)
) |> select(City_Name,strata,survey_weights,pct_protracted_idp_12_mnth,
            pct_protracted_idp_36_mnth,
            pct_protracted_idp_60_mnth,
            i_integration)


hh_idp_svy <- as_survey(hh_s,strata = strata ,weights = survey_weights)

HH_IDP_analysis_a <- create_analysis(design = hh_idp_svy,
                                     group_var = c("City_Name","pct_protracted_idp_12_mnth",
                                                   "pct_protracted_idp_36_mnth",
                                                   "pct_protracted_idp_60_mnth"))$results_table


HH_IDP_analysis <- HH_IDP_analysis_a |>
  filter(analysis_var == "i_integration") |>
  filter(analysis_type == "mean") |>
  filter(!grepl("::No",group_var_value)) |>
  filter(!is.na(group_var)) |>
  select(group_var,group_var_value,stat) |>
  mutate(
    category = case_when(group_var == "City_Name" ~"Overall",
                         T~gsub(".*:", "", group_var_value)),
    city = case_when(group_var == "City_Name" ~group_var_value,
                         T~gsub(":.*", "", group_var_value))
  ) |> select(category,city,stat) |> mutate(
    stat = round(stat*100,0)
  )


HH_IDP_analysis_pi_long <- HH_IDP_analysis |>
  pivot_wider(id_cols = city,names_from = category,values_from = stat)

HH_IDP_analysis_pi_long |>  clipr::write_clip()
