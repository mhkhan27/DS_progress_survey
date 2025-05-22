library(tidyverse)
library(srvyr)
library(dplyr)
library(illuminate)

dsp_service_mapping <- list()
df <- openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",2)
df_hh <-  openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",1)

df_hh$A_5_city |> unique()
hh_df_b_x <- df_hh |>
  dplyr::filter(City_Name %in% c("Xudur","Berdaale"))

indv_df <- df |>dplyr::filter(X_uuid %in% hh_df_b_x$X_uuid)
indv_df <- indv_df |>
  dplyr::left_join((hh_df_b_x |>
                      dplyr::select(X_uuid,neighbourhood_final,City_Name)))

data <- indv_df |>
  dplyr::select(pop_group,City_Name,
                nei_name = neighbourhood_final,
                age_years,sex_member)

data_classification <- data |>
  mutate(age_years = as.numeric(age_years)) |>
  mutate(
  age_group = case_when(age_years <6 ~ "0-6",
                        age_years %in% 6:17 ~ "6-17",
                        age_years %in% 18:60 ~ "18-60",
                        age_years > 60 ~ "60+"
                        ),
  city = case_when(City_Name == "Berdaale" ~ "Berdale" , T~ "Xudur")
) |> dplyr::select(-age_years,-City_Name)


# IDP ---------------------------------------------------------------------

data_classification_idp <- data_classification |>
  filter(pop_group == "idp")

ipd_ana <- data_classification_idp |>
  survey_analysis(disag = c("city","nei_name"),
                  vars_to_analyze = c("sex_member","age_group"))


ipd_ana <- ipd_ana |> select(c("main_variable", "choice", "subset_1_val", "subset_2_val",
                             "stat"))
ipd_ana_piw <- ipd_ana |>
  select(-main_variable) |>
  mutate(stat = round(stat*100,0)) |>
  tidyr::pivot_wider(
                     names_from = "choice",
                     values_from = "stat" ) |>
  select(city = subset_1_val,nei_name = subset_2_val,everything()) |>
  mutate(nei_name = case_when(nei_name =="Horseed_xudur" ~ "Horseed",
                              T~nei_name))
colnames(ipd_ana_piw)[3:8] <- paste0("idp_", colnames(ipd_ana_piw)[3:8])

dsp_service_mapping[["idp"]] <-ipd_ana_piw |> janitor::clean_names()


# HC ----------------------------------------------------------------------

data_classification_hc <- data_classification |>
  filter(pop_group == "hc")


hc_ana <- data_classification_hc |>
  survey_analysis(disag = c("city"),
                  vars_to_analyze = c("sex_member","age_group"))

hc_ana <- hc_ana |> select(c("main_variable", "choice", "subset_1_val",
                               "stat"))
hc_ana_piw <- hc_ana |>
  select(-main_variable) |>
  mutate(stat = round(stat*100,0)) |>
  tidyr::pivot_wider(
    names_from = "choice",
    values_from = "stat" ) |>
  select(city = subset_1_val,everything())

colnames(hc_ana_piw)[2:7] <- paste0("hc_", colnames(hc_ana_piw)[2:7])


dsp_service_mapping[["hc"]] <-hc_ana_piw |> janitor::clean_names()


# dsp_index ---------------------------------------------------------------


dsp_value <- openxlsx::read.xlsx("00_output/2024_09_27_dsp_index_v2.xlsx",1) |>
  dplyr::select(c("City", "admin_name", "population_group", "analysis_level",
                "group_var_value", "dsp_value"))
dsp_value <- dsp_value |> filter(City %in% c("Xudur","Berdaale")) |>
  filter(!(population_group == "IDP" & analysis_level =="City")) |>
  select(-any_of(c("analysis_level","group_var_value")))

dsp_value_idp<- dsp_value |> filter(population_group == "IDP") |>
  rename(dsp_value_idp = dsp_value)
dsp_value_hc<- dsp_value |> filter(population_group == "HC")  |>
  rename(dsp_value_hc = dsp_value) |>
  select(City,dsp_value_hc)

dsp_service_mapping[["dsp_value_f"]] <- dsp_value_idp |> left_join(dsp_value_hc,by = "City") |>
  select(-population_group) |> mutate(
    city_name = case_when(City == "Berdaale" ~ "Bardale" , T~ "Xudur"),
    nei_name = case_when(admin_name == "Horseed_xudur" ~ "Horseed",T~admin_name)
  ) |> select(-City,-admin_name)


# disability --------------------------------------------------------------
dis_cols <- c("vision",
              "hearing",
              "mobility",
              "comunicat",
              "cognition",
              "selfcare")

hh_df <- df_hh |> mutate(
  city = case_when(City_Name == "Berdaale" ~ "Bardale" , T~ City_Name),
  i_hh_with_disability =case_when(rowSums(df_hh[dis_cols] ==  "a_lot_of_difficulties" |
                                            df_hh[dis_cols]  == "cannot_do_at_all",na.rm =T) >0 ~ "at_least_one_disability",
                                  T~ "no_disability"),
)|>
  rename(nei_name = neighbourhood_final) |>
  dplyr::filter(city %in% c("Xudur","Bardale"))

hh_df |> select(nei_name,city) |> unique()

## idp
hh_df_idp <- hh_df |>
  filter(pop_group == "idp")


ipd_ana_dis <- hh_df_idp |>
  survey_analysis(disag = c("city","nei_name"),weights = T,
                  weight_column = "survey_weights",strata = "strata",
                  vars_to_analyze = c("i_hh_with_disability")) |>
  filter(choice == "at_least_one_disability") |>
  select(city=subset_1_val,nei_name=subset_2_val,stat) |>
  mutate(stat = round(stat*100,0)) |>
  mutate(nei_name = case_when(nei_name =="Horseed_xudur" ~ "Horseed",
                              T~nei_name)) |>
  rename(atleast_one_dis_idp =stat)

## HC
hh_df_hc <- hh_df |>
  filter(pop_group == "hc")

hc_ana_dis <- hh_df_hc |>
  survey_analysis(disag = c("city"),weights = F,
                   vars_to_analyze = c("i_hh_with_disability")) |>
  filter(choice == "at_least_one_disability") |>
  select(city=subset_1_val,stat) |>
  mutate(stat = round(stat*100,0)) |>
  # mutate(nei_name = case_when(nei_name =="Horseed_xudur" ~ "Horseed",
  #                             T~nei_name)) |>
  rename(atleast_one_dis_hc =stat)

dsp_service_mapping[["ana_dis"]] <- ipd_ana_dis |> left_join(hc_ana_dis) |>
  rename(city_name = city)

# save --------------------------------------------------------------------


save(dsp_service_mapping,file = "../../04_service_analysis/serviceanalysis/data-raw/dsp_service_mapping.rda")

