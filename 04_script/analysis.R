rm(list = ls())

library(openxlsx)
library(tidyverse)
library(srvyr)
library(illuminate)
library(analysistools)

yn_f <- function(x){
  case_when(x == "1" ~"Yes",
            x=="0" ~"No",
            x== "99" ~ "Dont_know",
            T~x)}

# read data ---------------------------------------------------------------


hh_data <- openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",1)

indv_data <- openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",2) |>
  mutate(age_group = case_when(age_years %in% 0:4 ~ "0_4",
                               age_years %in% 5:11 ~ "5_11",
                               age_years %in% 12:17 ~ "12_17",
                               age_years > 17 ~ "18+"),
         age_gender = paste0(sex_member, "_",age_group))

hh_data <- hh_data |> mutate(
  strata = case_when(pop_group == "idp" & A_6_neighbourhood == "Horseed_CA11" ~ "idp-Horseed_CA11",
                     T~strata)
)

indv_data <- indv_data |> select(-any_of("strata"))  |> left_join(hh_data |> select(X_uuid,strata))

survey <- openxlsx::read.xlsx("01_input/tool/kobo_tool.xlsx",1)
extra_cols <- c("date","start", "end", "deviceid", "devicephonenum", "username", "audit",
                "audit_URL", "subscriberid", "simid", "version", "loc_gps", "X_loc_gps_latitude",
                "X_loc_gps_longitude", "X_loc_gps_altitude", "X_loc_gps_precision",
                "current_lat", "current_long", "first_id_node", "n_nodes", "first_lat",
                "first_long", "first_gps", "geotrace", "distance", "geofence_result",
                "A_3_region", "A_4_district", "A_5_city", "A_1_team_leader","neighbourhood_final",
                "A_2_enumerator", "A_2_1_enum_sex", "A_2_2_org_name", "A_6_neighbourhood",

                "B_1_consent", "respondent_name", "repondent_phone",
                "C_1_HH_size", "count_respondent", "count_hhh",
                "male_0_4_count", "female_0_4_count", "male_5_11_count", "female_5_11_count",
                "male_12_17_count", "female_12_17_count", "male_5_17_count",
                "female_5_17_count", "male_adult_count", "female_adult_count",
                "total_male_count", "total_female_count",

                "A_6_neighbourhood_001", "selection_type", "building_join", "campname",
                "A_9_Pr_se", "A_12_pindrop", "target_lat", "target_long", "target_gps",
                "geotrace_target_location", "distance_target_location", "distance_error_explain",
                "other_sample_error", "B_1_consent", "respondent_name", "repondent_phone",
                "X_1_comment", "X_2_comeback_consent_yn",
                "X_3_phonumber_confirm", "X_4_enum_comment", "X_id", "X_uuid",
                "X_submission_time", "X_status", "X_submitted_by", "X__version__",
                "X_index", "G_2_1_other", "H_2_1_other", "J_4_1_other", "K_7_1_other", "A_2_enumerator","A_2_1_enum_sex","A_4_district","A_3_region",
                "K_10_1_other", "L_4_1_other", "L_7_1_other", "L_8_1_other", "strata1","A_11_enum_area",
                "X_index", "X_parent_table_name",
                "X_parent_index", "X_submission__id", "X_uuid", "X_submission__submission_time",
                "X_submission__validation_status", "X_submission__notes", "X_submission__status",
                "X_submission__submitted_by", "X_submission___version__", "X_submission__tags","date")

hh_text_col <- analyticaid::find_qualitative_cols_from_kobo(hh_data,kobo_survey_sheet =survey,additional_cols_to_remove =extra_cols )
indv_text_col <- analyticaid::find_qualitative_cols_from_kobo(indv_data,kobo_survey_sheet =survey,additional_cols_to_remove =extra_cols )





# recoding ----------------------------------------------------------------

indv_data_hoh <- indv_data |> mutate(
  gender_hoh = case_when(resp_hhh.hhh == 1 ~ sex_member,T~NA_character_)
) |> filter(!is.na(gender_hoh)) |> group_by(X_uuid) |> reframe(
  gender_hoh = paste0(gender_hoh)
)

indv_data_hoh |> filter(is.na(gender_hoh)) |> pull(X_uuid)


# indv_data_hoh |> filter(X_uuid %in% pb) |> filter(!(X_uuid %in% pb & gender_hoh == "female"))|> view()

# pb <- indv_data_hoh$X_uuid[indv_data_hoh$X_uuid |> duplicated()] ## to be added in the cleaning
#
# indv_data_hoh <- indv_data_hoh |> filter(!(X_uuid %in% pb & gender_hoh == "female"))
#

dis_cols <- c("vision",
              "hearing",
              "mobility",
              "comunicat",
              "cognition",
              "selfcare")

hh_data <- hh_data |> mutate(
  i_hh_with_disability =case_when(rowSums(hh_data[dis_cols] ==  "a_lot_of_difficulties" |
                                            hh_data[dis_cols]  == "cannot_do_at_all",na.rm =T) >0 ~ "at_least_one_disability",
                                  T~ "no_disability"),
  i_year_left_origin = case_when(pop_group == "hc" ~ NA_character_,T~D_1_4_year_left_origin) ,
  i_season_left_idp = case_when(pop_group == "hc" ~ NA_character_,T~D_1_4_1_season_left),
  i_year_arrive_current_idp = case_when(pop_group == "hc" ~ NA_character_,T~D_1_6_year_arrive) ,
  i_season_arrive_current_idp = case_when(pop_group == "hc" ~ NA_character_,T~D_1_6_1_season_left),


  i_reason_displacement_idp = case_when(pop_group == "hc" ~ NA_character_,T~D_1_5_reasondisp),
  i_arrival_period = case_when(as.Date(D_1_6_year_arrive) < as.Date("2014-01-01") ~ "more_than_10_years_ago",
                               as.Date(D_1_6_year_arrive) < as.Date("2019-01-01") ~ "6_10_years_ago",
                               as.Date(D_1_6_year_arrive) < as.Date("2021-01-01") ~ "4-5_years",
                               as.Date(D_1_6_year_arrive) < as.Date("2024-01-01") ~ "1_3_years",
                               as.Date(D_1_6_year_arrive) > as.Date("2023-12-30") ~ "last_year"
  )) |> left_join(indv_data_hoh) |>
  mutate(disabily_group = paste0(City_Name,"::",i_hh_with_disability )) |>
  mutate(gender_hoh = case_when(is.na(gender_hoh)~"male",T~gender_hoh)) |>
  mutate(gender_hoh = paste0(City_Name,"::",gender_hoh),
         month_left = case_when(i_season_left_idp == "jilaal_dec_mar" ~ "01",
                                i_season_left_idp == "gu_mar_jun" ~ "05",
                                i_season_left_idp == "hagaa_jul_sep" ~ "08",
                                i_season_left_idp == "deyr_sep_nov" ~"10"
         ),
         month_arrive = case_when(i_season_arrive_current_idp == "jilaal_dec_mar" ~ "01",
                                  i_season_arrive_current_idp == "gu_mar_jun" ~ "05",
                                  i_season_arrive_current_idp == "hagaa_jul_sep" ~ "08",
                                  i_season_arrive_current_idp == "deyr_sep_nov" ~"10"
         ),

         date_left_origin = case_when(!is.na(i_year_left_origin) ~ paste0(year(as.Date(i_year_left_origin)),"-",month_left ,"-01"),
                                      T~NA_character_)|> as.Date(),
         date_arrival_current = case_when(!is.na(i_year_arrive_current_idp) ~ paste0(year(as.Date(i_year_arrive_current_idp)),"-",month_arrive ,"-01"),
                                          T~NA_character_) |> as.Date(),
         survey_date= start |> as.Date(),
         i1_month_diff_origin_to_current_location = as.numeric(date_arrival_current -date_left_origin)/30.44,
         i2_current_location_month = as.numeric(survey_date-date_arrival_current)/30.44,
  ) |> mutate(
    i1_month_diff_origin_to_current_location = case_when(is.na(i1_month_diff_origin_to_current_location) ~NA_real_,
                                                      i1_month_diff_origin_to_current_location < 0 ~NA_real_,
                                                      T~i1_month_diff_origin_to_current_location),
    i2_current_location_month = case_when(is.na(i2_current_location_month) ~NA_real_,
                                       i2_current_location_month < 0 ~NA_real_,
                                                      T~i2_current_location_month),
    i3_total_displacement_month = i1_month_diff_origin_to_current_location+i2_current_location_month,
    i_shelter_doc_type_rent_own = L_1_shelter_ownership %in% c("rent","own")&
      L_2_shelter_doctype %in% c("written_agreement","land_title_deed"),
    i_shelter_doc_type_rent = L_1_shelter_ownership %in% c("rent") &
      L_2_shelter_doctype %in% c("written_agreement","land_title_deed"),
    i_shelter_status_doc_type_own = L_1_shelter_ownership %in% c("own") &
      L_2_shelter_doctype %in% c("written_agreement","land_title_deed"),


    i_land_doc_type_rent_own = L_5_land_ownership %in% c("rent","own")&
      L_7_land_doctype %in% c("written_agreement","land_title_deed"),
    i_land_doc_type_rent = L_5_land_ownership %in% c("rent") &
      L_7_land_doctype %in% c("written_agreement","land_title_deed"),
    i_land_doc_type_own = L_5_land_ownership %in% c("own") &
      L_7_land_doctype %in% c("written_agreement","land_title_deed")
  )  |>
  select(- D_1_4_year_left_origin,-D_1_4_1_season_left,-D_1_5_reasondisp,-date_left_origin,
         -date_arrival_current,-survey_date)



weather_measure_col <- hh_data |> select(starts_with("P_1_1_measures_weather.")) |> names()  |> dput()


for(i in weather_measure_col){
  hh_data[[i]] <- case_when(is.na(hh_data[[i]]) ~F,T~hh_data[[i]])
}


indv_data <- indv_data |> left_join(hh_data |> select(X_uuid,City_Name,gender_hoh,disabily_group))



# gender_hoh --------------------------------------------------------------

hh_data <- hh_data |> mutate(
  i_gender_hoh = case_when(grepl("::female",gender_hoh) ~ "female",
                           T~"male")
)


# fixing format -----------------------------------------------------------



chracter_cols <- c("R_5_idp_host_trust",	"S_1_TrustHealth",	"S_2_TrustEducat",	"S_3_TrustJustice",	"S_4_TrustSecurity",	"S_5_TrustLocAuth",	"S_6_TrustFinancial")

ynd <- c("I_5_no_food_3mth","K_2_safe_access_water","K_4_access_water_rainy","V_1_situation_improv","U_1_legalservices_access",	"K_5_access_water_dry",
         "D_4_absent_members", "E_5_hoh_read", "H_5_emergency_savings",
         "H_6_finane_account", "I_5_no_food_3mth", "K_2_safe_access_water",
         "K_4_access_water_rainy", "K_5_access_water_dry", "K_9_safe_access_latrin",
         "L_1_1_notpaying_rent", "L_11_eviction", "L_16_gov_land_allocate",
         "L_16_1_gov_land_need", "Q_3_MaleFreelyMove", "Q_4_FemaleFreelyMove",
         "Q_6_victim_violence", "R_2_youth_play_hc", "R_3_youth_play_idp",
         "T_2_gov_visit", "U_1_legalservices_access", "V_1_situation_improv",
         "Q_3_MaleFreelyMove",	"Q_4_FemaleFreelyMove","R_2_youth_play_hc",	"R_3_youth_play_idp",
         "T_2_gov_visit","Q_6_victim_violence","E_5_hoh_read","D_4_absent_members") |> unique()# yes no dont know(99)

yn <- c("X_2_comeback_consent_yn","D_1_origin_yesno","D_4_1_absent_assist_reunify","I_2_safe_access_food",
        "D_1_origin_yesno", "D_4_1_absent_assist_reunify", "I_2_safe_access_food",
        "P_1_adapt_weather", "W_1_IntegConcept1", "X_2_comeback_consent_yn",
        "P_1_adapt_weather","W_1_IntegConcept1","D_4_1_absent_assist_reunify") |> unique()
ynd_f <- c(ynd,yn)



# HH level analysis -------------------------------------------------------

# HC  ---------------------------------------------------------------------

hh_data_hc <- hh_data  |> filter(pop_group == "hc")
indv_data_hc <- indv_data |> filter(X_uuid %in% hh_data_hc$X_uuid)


hh_data_hc <- hh_data_hc |> fix_data_type(remove_all_NA_col = T)
indv_data_hc <- indv_data_hc |> fix_data_type(remove_all_NA_col = T)


hh_data_hc <- hh_data_hc |> mutate_if(is.logical, function(x){x*1})
indv_data_hc <- indv_data_hc |> mutate_if(is.logical, function(x){x*1})



hh_data_hc <- hh_data_hc |> dplyr::mutate_at(chracter_cols,as.character)
hh_data_hc <- hh_data_hc |> dplyr::mutate_at(ynd_f,as.character)
hh_data_hc <- hh_data_hc |> dplyr::mutate_at(ynd_f,yn_f)



# IDP  ---------------------------------------------------------------------

hh_data_idp <- hh_data  |> filter(pop_group != "hc")
indv_data_idp <- indv_data |> filter(X_uuid %in% hh_data_idp$X_uuid)

hh_data_idp <- hh_data_idp |> fix_data_type(remove_all_NA_col = T)
indv_data_idp <- indv_data_idp |> fix_data_type(remove_all_NA_col = T)


hh_data_idp <- hh_data_idp |> mutate_if(is.logical, function(x){x*1})
indv_data_idp <- indv_data_idp |> mutate_if(is.logical, function(x){x*1})



hh_data_idp <- hh_data_idp |> dplyr::mutate_at(chracter_cols,as.character)
hh_data_idp <- hh_data_idp |> dplyr::mutate_at(ynd_f,as.character)
hh_data_idp <- hh_data_idp |> dplyr::mutate_at(ynd_f,yn_f)



# HH level analysis -------------------------------------------------------

hh_hc_svy <- as_survey(hh_data_hc |> select(-any_of(hh_text_col)))
hh_idp_svy <- as_survey(hh_data_idp |> select(-any_of(hh_text_col)),strata = strata ,weights = survey_weights)

HH_HC_analysis <- create_analysis(design = hh_hc_svy,group_var = c("strata","gender_hoh","disabily_group"))$results_table
HH_IDP_analysis <- create_analysis(design = hh_idp_svy,group_var = c("strata","City_Name","gender_hoh","disabily_group"))$results_table


# bind HH analysis ------------------------------------------------------------

HH_analysis <- HH_IDP_analysis |> mutate(pop_group =  "IDP") |> bind_rows(
  HH_HC_analysis |> mutate(pop_group =  "HC")
)  |>
  #   bind_rows(
  #   HH_disability_analysis |> mutate(pop_group =  "HC & IDP")
  # )|>
  select(pop_group,group_var,group_var_value,analysis_var,analysis_var_value,stat,n_w,n_w_total,n,n_total,analysis_type)


HH_analysis <- HH_analysis |> mutate(
  group_var = case_when(is.na(group_var) ~ "No grouping",T~group_var),
  group_var_value = case_when( group_var == "No grouping" ~ "All",T~group_var_value),
)



write.csv(HH_analysis,"03_output/05_analysis/hh_analysis.csv")


# individual level --------------------------------------------------------

indv_hc_svy <- as_survey(indv_data_hc |> select(-any_of(hh_text_col)))
indv_idp_svy <- as_survey(indv_data_idp |> select(-any_of(hh_text_col)))
INDV_HC_analysis <- create_analysis(design = indv_hc_svy,group_var = c("strata","gender_hoh","disabily_group"))$results_table
INDV_IDP_analysis <- create_analysis(design = indv_idp_svy,group_var = c("strata","City_Name","gender_hoh","disabily_group"))$results_table


# bind indv ---------------------------------------------------------------

INDV_analysis <- INDV_IDP_analysis |> mutate(pop_group =  "IDP") |> bind_rows(
  INDV_HC_analysis |> mutate(pop_group =  "HC")
)  |>
  select(pop_group,group_var,group_var_value,analysis_var,analysis_var_value,stat,n_w,n_w_total,n,n_total,analysis_type)


INDV_analysis <- INDV_analysis |> mutate(
  group_var = case_when(is.na(group_var) ~ "No grouping",T~group_var),
  group_var_value = case_when( group_var == "No grouping" ~ "All",T~group_var_value),
)



write.csv(INDV_analysis,"03_output/05_analysis/indv_analysis.csv")



