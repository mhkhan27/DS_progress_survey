rm(list = ls())

library(openxlsx)
library(tidyverse)
library(srvyr)
library(illuminate)
library(analysistools)



### Note
# Analysis level: stratitif + overall  +HC overall/genderhoh- city level ()
#



# read data ---------------------------------------------------------------

hh_data <- openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",1)
indv_data <- openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",2)
survey <- openxlsx::read.xlsx("01_input/tool/kobo_tool.xlsx",1)


extra_cols <- c("date","start", "end", "deviceid", "devicephonenum", "username", "audit",
                "audit_URL", "subscriberid", "simid", "version", "loc_gps", "X_loc_gps_latitude",
                "X_loc_gps_longitude", "X_loc_gps_altitude", "X_loc_gps_precision",
                "current_lat", "current_long", "first_id_node", "n_nodes", "first_lat",
                "first_long", "first_gps", "geotrace", "distance", "geofence_result",
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
                "X_submission__submitted_by", "X_submission___version__", "X_submission__tags")

hh_text_col <- analyticaid::find_qualitative_cols_from_kobo(hh_data,kobo_survey_sheet =survey,additional_cols_to_remove =extra_cols )
indv_text_col <- analyticaid::find_qualitative_cols_from_kobo(indv_data,kobo_survey_sheet =survey,additional_cols_to_remove =extra_cols )



# Data Preparation ------------------------------------------------------------

# HC  ---------------------------------------------------------------------

hh_data_hc <- hh_data  |> filter(A_7_sampletype == "hc")
indv_data_hc <- indv_data |> filter(X_uuid %in% hh_data_hc$X_uuid)


hh_data_hc <- hh_data_hc |> fix_data_type(remove_all_NA_col = T)
indv_data_hc <- indv_data_hc |> fix_data_type(remove_all_NA_col = T)

analysis_vars_hh <- names(hh_data_hc)[!names(hh_data_hc) %in% hh_text_col]



# IDP ---------------------------------------------------------------------

hh_data_idp <- hh_data  |> filter(A_7_sampletype != "hc")
indv_data_idp <- indv_data |> filter(X_uuid %in% hh_data_idp$X_uuid)

hh_data_idp <- hh_data_idp |> fix_data_type(remove_all_NA_col = T)
indv_data_idp <- indv_data_idp |> fix_data_type(remove_all_NA_col = T)


analysis_vars_hh_idp <- names(hh_data_idp)[!names(hh_data_idp) %in% hh_text_col]
analysis_vars_indv_idp <- names(indv_data_idp)[!names(indv_data_idp) %in% indv_text_col]











HH_HC_analysis_OVERALL <- illuminate::survey_analysis(df = hh_data_hc,weights = F,
                                                      vars_to_analyze = analysis_vars_hh
) |> select(analysis_type,
            main_variable,choice,
            pct_hc = stat,
            count_hc = n_unweighted,
            response_count_hc = response_count
)


HH_HC_analysis_STRATA <- illuminate::survey_analysis(df = hh_data_hc,weights = F,
                                                     vars_to_analyze = analysis_vars_hh,
                                                     disag = "strata"
) |> select(analysis_type,
            main_variable,choice,
            group_by = subset_1_name,
            group_value= subset_1_val,
            pct_hc = stat,
            count_hc = n_unweighted,
            count_by_group_hc = count_by_subset,
            response_count = response_count
)











analysis_vars_indv <- names(indv_data_hc)[!names(indv_data_hc) %in% indv_text_col]
INDV_HC_analysis_STRATA <- illuminate::survey_analysis(df = indv_data_hc,weights = F,
                                                       vars_to_analyze = analysis_vars_indv,
                                                       disag = "strata"
)


# IDP ---------------------------------------------------------------------

hh_data_idp <- hh_data  |> filter(A_7_sampletype != "hc")
indv_data_idp <- indv_data |> filter(X_uuid %in% hh_data_idp$X_uuid)

hh_data_idp <- hh_data_idp |> fix_data_type(remove_all_NA_col = T)
indv_data_idp <- indv_data_idp |> fix_data_type(remove_all_NA_col = T)


analysis_vars_hh_idp <- names(hh_data_idp)[!names(hh_data_idp) %in% hh_text_col][1:65]
analysis_vars_indv_idp <- names(indv_data_idp)[!names(indv_data_idp) %in% indv_text_col]


HH_IDP_analysis_OVERALL <- illuminate::survey_analysis(df = hh_data_idp,weights = T,
                                                       strata = "strata",weight_column = "survey_weights",
                                                       vars_to_analyze = analysis_vars_hh_idp) |>
  select(analysis_type,
         main_variable,choice,
         pct_idp = stat,
         count_idp = n_unweighted,
         response_count_idp = response_count
  )

HH_IDP_analysis_ove <- illuminate::survey_analysis(df = hh_data_idp,weights = T,
                                                      strata = "strata",
                                                      weight_column = "survey_weights",
                                                      vars_to_analyze = analysis_vars_hh_idp#,
                                                      # disag = "strata"
)


INDV_IDP_analysis_OVERALL <- illuminate::survey_analysis(df = indv_data_idp,weights = T,
                                                         strata = "strata",weight_column = "survey_weights",
                                                         vars_to_analyze = analysis_vars_indv_idp)

INDV_IDP_analysis_STRATA <- illuminate::survey_analysis(df = indv_data_idp,weights = T,
                                                        strata = "strata",weight_column = "survey_weights",
                                                        vars_to_analyze = analysis_vars_indv_idp,
                                                        disag = "strata"
)


# write_list --------------------------------------------------------------

write_list <- list(
  HH_HC_analysis_STRATA = HH_HC_analysis_STRATA,
  INDV_HC_analysis_STRATA = INDV_HC_analysis_STRATA,
  HH_IDP_analysis_OVERALL = HH_IDP_analysis_OVERALL,
  HH_IDP_analysis_STRATA = HH_IDP_analysis_STRATA,
  INDV_IDP_analysis_OVERALL = INDV_IDP_analysis_OVERALL,
  INDV_IDP_analysis_STRATA = INDV_IDP_analysis_STRATA
)

analyticaid::write_formatted_excel(write_list,"03_output/05_analysis/baidoa.xlsx")



# bind IDP_hc -------------------------------------------------------------
HH_IDP_analysis_OVERALL
HH_HC_analysis_OVERALL


HH_analysis_OVERALL <- HH_HC_analysis_OVERALL |> full_join(HH_IDP_analysis_OVERALL) |>
  select(c("analysis_type", "main_variable", "choice",
           "pct_hc","pct_idp","count_hc","count_idp",
           "response_count_hc","response_count_idp"))


analyticaid::write_formatted_excel(list(HH_analysis_OVERALL =HH_analysis_OVERALL),"03_output/05_analysis/test.xlsx")


