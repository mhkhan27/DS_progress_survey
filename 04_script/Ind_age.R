rm(list=ls())
library(tidyverse)
library(srvyr)
library(analysistools)

hh_data <- openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",1)
indv_data <- openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",2)


indv_data <- indv_data |> left_join(hh_data |> select(X_uuid,City_Name,A_7_sampletype))

indv_data$age_years <-  indv_data$age_years |> as.numeric()
indv_data <- indv_data |> mutate(
  age_years =case_when(age_years>94 ~ 95,T~age_years)
) |> filter(!is.na(age_years))


# HC ----------------------------------------------------------------------

indv_data_hc <- indv_data |> filter(A_7_sampletype == "hc") |>
  select(age_years,City_Name,sex_member,
         #strata,survey_weights
         ) |> mutate(
           age_gender = paste0(sex_member,"::",age_years)
         ) |> select(-age_years,-sex_member)


indv_data_hc <- indv_data_hc |> mutate_if(is.logical, function(x){x*1})

indv_hc_svy <- as_survey(indv_data_hc )

IDV_HC_analysis <- create_analysis(design = indv_hc_svy,group_var = "City_Name")$results_table


IDV_HC_analysis2 <- IDV_HC_analysis |> filter(group_var == "City_Name",
                                              analysis_var == "age_gender" ) |> select(
  city = group_var_value,
  variable_name = analysis_var_value,
  pct_hc = stat,
  count_hc = n,
  city_level_total = n_total
) |> separate(variable_name,into = c("gender","age"))




### table for pct

IDV_HC_analysis3 <- IDV_HC_analysis2 |>
  select(city,age,gender,pct_hc) |>
  pivot_wider(id_cols = c(city,age),names_from = "gender",values_from = pct_hc)


IDV_HC_analysis3[is.na(IDV_HC_analysis3)] <- 0

IDV_HC_analysis3 <- IDV_HC_analysis3 |>
  rename(pct_female_hc = female,
         pct_male_hc = male)


### table for count


IDV_HC_analysis_count <- IDV_HC_analysis2 |>
  select(city,age,gender,count_hc) |>
  pivot_wider(id_cols = c(city,age),names_from = "gender",values_from = count_hc)

IDV_HC_analysis_count2 <- IDV_HC_analysis_count |>
  rename(count_female_hc = female,
         count_male_hc = male)

IDV_HC_analysis_count2[is.na(IDV_HC_analysis_count2)] <- 0


###### total table

total <- IDV_HC_analysis2 |> select(city,city_level_hc_total = city_level_total) |> unique()


### HC table


hc_table <- IDV_HC_analysis3 |> left_join(IDV_HC_analysis_count2) |>
  left_join(total) |> mutate(
    age_total_hc_by_city = count_male_hc + count_female_hc,
    pct_male_hc_by_city = count_male_hc/(count_male_hc+count_female_hc),
    pct_female_hc_by_city = count_female_hc/(count_male_hc+count_female_hc)

  ) |> select(city,age,count_male_hc,count_female_hc,age_total_hc_by_city,city_level_hc_total,pct_male_hc_by_city,pct_female_hc_by_city,pct_male_hc,pct_female_hc)



# IDP ---------------------------------------------------------------------


indv_data_idp <- indv_data |> filter(A_7_sampletype != "hc") |>
  select(age_years,City_Name,sex_member,
         strata,survey_weights
  ) |> mutate(
    age_gender = paste0(sex_member,"::",age_years)
  ) |> select(-age_years,-sex_member)


indv_data_idp <- indv_data_idp |> mutate_if(is.logical, function(x){x*1})

indv_idp_svy <- as_survey(indv_data_idp,strata = strata ,weights = survey_weights )

IDV_idp_analysis <- create_analysis(design = indv_idp_svy,group_var = "City_Name")$results_table


IDV_idp_analysis2 <- IDV_idp_analysis |> filter(group_var == "City_Name",
                                              analysis_var == "age_gender" ) |> select(
                                                city = group_var_value,
                                                variable_name = analysis_var_value,
                                                pct_idp = stat,
                                                count_idp = n_w,
                                                city_level_total = n_w_total
                                              ) |> separate(variable_name,into = c("gender","age"))


# IDV_idp_analysis2 |> clipr::write_clip()


### table for pct

IDV_idp_analysis3 <- IDV_idp_analysis2 |>
  select(city,age,gender,pct_idp) |>
  pivot_wider(id_cols = c(city,age),names_from = "gender",values_from = pct_idp)


IDV_idp_analysis3[is.na(IDV_idp_analysis3)] <- 0

IDV_idp_analysis3 <- IDV_idp_analysis3 |>
  rename(pct_female_idp = female,
         pct_male_idp = male)


### table for count


IDV_idp_analysis_count <- IDV_idp_analysis2 |>
  select(city,age,gender,count_idp) |>
  pivot_wider(id_cols = c(city,age),names_from = "gender",values_from = count_idp)

IDV_idp_analysis_count2 <- IDV_idp_analysis_count |>
  rename(count_female_idp = female,
         count_male_idp = male)

IDV_idp_analysis_count2[is.na(IDV_idp_analysis_count2)] <- 0


###### total table

total <- IDV_idp_analysis2 |> select(city,city_level_idp_total= city_level_total) |> unique()


### idp table


idp_table <- IDV_idp_analysis3 |> left_join(IDV_idp_analysis_count2) |>
  left_join(total) |> mutate(
    age_total_by_city = count_male_idp + count_female_idp,
    pct_male_idp_by_city = count_male_idp/(count_male_idp+count_female_idp),
    pct_female_idp_by_city = count_female_idp/(count_male_idp+count_female_idp)

  ) |> select(city,age,count_male_idp,count_female_idp,age_total_by_city,city_level_idp_total,pct_male_idp_by_city,pct_female_idp_by_city,pct_male_idp,pct_female_idp)



# table -------------------------------------------------------------------

age_gender_break <- hc_table |> full_join(idp_table)

age_gender_break[is.na(age_gender_break)] <- 0

age_gender_break$age <- age_gender_break$age |> as.numeric()

age_gender_break <- age_gender_break |> arrange(city,age)


openxlsx::write.xlsx(age_gender_break,"03_output/age_gender_break.xlsx")



