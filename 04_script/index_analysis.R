rm(list = ls())

library(openxlsx)
library(tidyverse)
library(srvyr)
library(illuminate)
library(analysistools)



dis_cols <- c("vision",
              "hearing",
              "mobility",
              "comunicat",
              "cognition",
              "selfcare")

# read data ---------------------------------------------------------------


hh <-openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",1)

indv_data <- openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",2) |>
  mutate(age_group = case_when(age_years %in% 0:4 ~ "0_4",
                               age_years %in% 5:11 ~ "5_11",
                               age_years %in% 12:17 ~ "12_17",
                               age_years > 17 ~ "18+"),
         age_gender = paste0(sex_member, "_",age_group))

# recoding ----------------------------------------------------------------



indv_data_hoh <- indv_data |> mutate(
  gender_hoh = case_when(resp_hhh.hhh == 1 ~ sex_member,T~NA_character_)
) |> filter(!is.na(gender_hoh)) |> group_by(X_uuid) |> reframe(
  gender_hoh = paste0(gender_hoh)
)


hh <- hh |> left_join(indv_data_hoh) |>
  mutate(gender_hoh = case_when(is.na(gender_hoh)~"male",T~gender_hoh)) |>
  mutate(
    gender_hoh = paste0(City_Name,"::",gender_hoh),
    strata = case_when(pop_group == "idp" & A_6_neighbourhood == "Horseed_CA11" ~ "idp-Horseed_CA11",
                       T~strata),
    i_hh_with_disability =case_when(rowSums(hh[dis_cols] ==  "a_lot_of_difficulties" |
                                              hh[dis_cols]  == "cannot_do_at_all",na.rm =T) >0 ~ "at_least_one_disability",
                                    T~ "no_disability"),
    disabily_group = paste0(City_Name,"::",i_hh_with_disability )
  )


indv_data <- indv_data |> select(-any_of("strata"))  |>
  left_join(hh |> select(X_uuid,strata))


source("04_script/index_recoding.R")

write.csv(hh_data,"04_script/04_IDSF_policy_brief/hh_data_with_dsp_index.csv")


col <- hh_data |> select(starts_with("i"),
                         dsp_index,dsp_value,
                         dsp_index_6,dsp_value_6) |>
  select(contains(c("cri","sub","dsp"))) |> names() |> sort()


hh_data <- hh_data |> select(c(col,"pop_group","strata","gender_hoh","disabily_group","City_Name","survey_weights"))

hh_data <- hh_data |> mutate_if(is.logical, function(x){x*1})

# -------------------------------------------------------------------------

# HC  ---------------------------------------------------------------------

hh_data_hc <- hh_data  |> filter(pop_group == "hc")
hh_hc_svy <- as_survey(hh_data_hc )
HH_HC_analysis <- create_analysis(design = hh_hc_svy,group_var = c("strata","gender_hoh","disabily_group"))$results_table
HH_HC_analysis <- HH_HC_analysis |> filter(analysis_type != "median")


# IDP ---------------------------------------------------------------------

hh_data_idp <- hh_data  |> filter(pop_group != "hc")
hh_idp_svy <- as_survey(hh_data_idp,strata = strata ,weights = survey_weights)
HH_IDP_analysis <- create_analysis(design = hh_idp_svy,group_var = c("strata","City_Name","gender_hoh","disabily_group"))$results_table
HH_IDP_analysis <- HH_IDP_analysis |> filter(analysis_type != "median")



# bind_analysis -----------------------------------------------------------

HH_analysis <- HH_IDP_analysis |> mutate(pop_group =  "IDP") |> bind_rows(
  HH_HC_analysis |> mutate(pop_group =  "HC")
)  |>
  select(pop_group,group_var,group_var_value,analysis_var,
         analysis_var_value,stat,n_w,n_w_total,n,
         n_total,analysis_type)


HH_analysis <- HH_analysis |> mutate(
  group_var = case_when(is.na(group_var) ~ "No grouping",T~group_var),
  group_var_value = case_when( group_var == "No grouping" ~ "All",T~group_var_value),
)


HH_analysis <- HH_analysis |> mutate(
  stat = case_when (analysis_var %in% c("dsp_value", "dsp_value_6")~ round(stat,2),
                    T~round(stat*100,2))
) |> filter(analysis_var %in% col)

write.csv(HH_analysis,"03_output/05_analysis/dsp_index_analysis.csv")
