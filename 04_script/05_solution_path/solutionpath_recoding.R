rm(list = ls())

library(openxlsx)
library(tidyverse)
library(srvyr)
library(illuminate)
library(analysistools)


# data pre ----------------------------------------------------------------

source("04_script/05_solution_path/01_data_preparation_fun.R")

transitional_shelter <- c("somali_traditional_house_mundul",
                          "timber_and_mud_with_cgi_roof_baraako",
                          "plywood_wall_with_cgi_roof_darbiga_plywood",
                          "cgi_sheet_wall_and_roof",
                          "refugee_housing_unit")

permanaent_sheter <- c("permanent_shelter_mud_block",
                       "permanent_shelter_stone_brick_wall_with_cgi_roof")


# recoding ----------------------------------------------------------------




hh <- hh |> mutate(
  ## pathway 1
  i_tg_gl_gvt_responsive=T_4_gov_resp_needs %in% c("high","medium"),
  i_tg_gl_gvt_inclusivity = T_5_dec_making_inclusive %in%  c("high","medium"),
  i_tg_gl_trust_local_authority = case_when(S_5_TrustLocAuth == 99 ~ NA_real_,
                                            S_5_TrustLocAuth %in% c(3:5)~T,
                                            T~F),
  i_tg_gl_meeting_attendance = case_when(T_3_public_meeting == "99" ~NA_real_,
                                         T_3_public_meeting == "regularly" ~T,
                                         T~F),
  i_sj_gl_access_to_legal_service = case_when(U_1_legalservices_access == 99 ~NA_real_,
                                              U_1_legalservices_access == 1~T,
                                              T~F),
  i_sj_gl_access_to_effective_justic_sys = case_when(U_3_legalservices_effect %in% c("very_effective",
                                                                                     "somewhat_effective") ~T,
                                                     T~F),
  i_sj_gl_trust_on_security = case_when(S_4_TrustSecurity == 99 ~ NA_real_,
                                        S_4_TrustSecurity %in% c(3:5)~T,
                                        T~F),
  i_sj_gl_trust_on_justice =case_when(S_3_TrustJustice == 99 ~ NA_real_,
                                      S_3_TrustJustice %in% c(3:5)~T,
                                      T~F),

  ## soultion pathway 2: Access to basic services
  i_ea_abc_access_school = case_when(is.na(E_9_timeto_school)~NA_real_,
                                     E_9_timeto_school == "99" ~NA_real_,
                                     E_9_timeto_school %in% c("less_than_half_an_hour"#,
                                                              # "between_1_and_2_hours"
                                                              ) ~T,
                                     T~F),
  i_ea_abc_trust_on_education = case_when(S_2_TrustEducat == 99 ~ NA_real_,
                                          S_2_TrustEducat %in% c(3:5)~T,
                                          T~F),
  i_he_abc_access_healthpoints =case_when(J_1_sickwhere.public_free_health_center == T |
                                            J_1_sickwhere.hospital_private_health_center_clinic == T|
                                            J_1_sickwhere.private_doctor == T|
                                            J_1_sickwhere.mobile_clinic ==T ~T,
                                          T~F
  ),
  i_he_abc_access_healh_travel = case_when(J_2_health_distance == "99" ~NA_real_,
                                           J_2_health_distance %in% c("less_than_half_an_hour"#,
                                                                      # "between_1_and_2_hours"
                                                                      )~T,
                                           T~F),
  i_he_abc_receiving_healhcare = case_when(J_3_healthcare_needed %in% c("99","have_not_needed_healthcare") ~NA_real_,
                                           J_3_healthcare_needed %in% c("received_some_needed_healthcare",
                                                                        "received_all_needed_healthcare") ~T,
                                           T~F),
  i_he_abc_trust_healthsystem = case_when(S_1_TrustHealth == 99 ~ NA_real_,
                                          S_1_TrustHealth %in% c(3:5)~T,
                                          T~F),
  i_wash_abc_access_water = case_when(K_2_safe_access_water == 99 ~NA_real_,
                                      K_2_safe_access_water == 1~T,
                                      T~F),
  i_wash_abc_access_water_travel = case_when(K_3_water_distance_dryseason == "99"~NA_real_,
                                             K_3_water_distance_dryseason %in% "less_than_half_an_hour" ~T,
                                             T~F),
  i_wash_abc_suffi_water_rainy = case_when(K_4_access_water_rainy == 99 ~ NA_real_,
                                           K_4_access_water_rainy == 1 ~T,
                                           T~F),
  i_wash_abc_suffi_water_dry = case_when(K_5_access_water_dry == 99 ~ NA_real_,
                                         K_5_access_water_dry == 1 ~T,
                                         T~F),
  i_wash_abc_safe_latrine= case_when(K_7_latrin_type == "open_defection"~F,
                                     K_9_safe_access_latrin == 99 ~NA_real_,
                                     K_9_safe_access_latrin == 1 ~T,
                                     T~F),
  i_bi_abc_access_to_power = case_when(N_1_source_electricity.99 == T ~NA_real_,
                                       N_1_source_electricity.public_electricity == T |
                                       N_1_source_electricity.private_electricity ==T |
                                       N_1_source_electricity.generator == T |
                                       N_1_source_electricity.solar_panel == T~T,
                                       T~F),

  ## Solution pathway 3
  i_ie_el_basic_need = case_when(F_1_hh_basic_needs == "99" ~ NA_real_,
                                 F_1_hh_basic_needs %in% c("yes_able_to_meet_all_basic_needs",
                                                           "somewhat_able_to_meet_some_basic_needs_but_struggling") ~T,
                                 T~F),
  i_fi_el_debt = case_when(H_1_debttype.77 == T|
                           H_1_debttype.investment_business == T~T,
                           T~F),
  i_fi_el_credit_options = case_when(H_3_credit_option.99 ==T ~NA_real_,
                                     H_3_credit_option.banks ==T |
                                       H_3_credit_option.microfinance_institutions == T ~T,
                                     T~F ),
  i_fi_el_account = case_when(H_6_finane_account == 99 ~ NA_real_,
                              H_6_finane_account == 1 ~T,
                              T~F),
  i_fi_el_financial_institutions = case_when(S_6_TrustFinancial == 99 ~ NA_real_,
                                             S_6_TrustFinancial %in% c(3:5)~T,
                                             T~F),
  i_fm_el_food_source = case_when(is.na(I_2_safe_access_food) ~NA_real_,
                                  I_2_safe_access_food == 1 ~T,
                                  T~F),
  i_fm_el_food_market =case_when(I_3_market_distance == "99" ~NA_real_,
                                 I_3_market_distance %in% c("less_than_half_an_hour"#,
                                                            # "between_1_and_2_hours"
                                                            )~T,
                                 T~F),
  i_fm_el_food_security = case_when(I_5_no_food_3mth == 99 ~NA_real_,
                                    I_5_no_food_3mth == 0 ~T,
                                    T~F),
  i_cm_el_external_assistance = case_when(F_2_assistance.77 == T ~T,
                                          T~F),

  # i_cm_el_coping_mecha = case_when(F_5_cope_income.99 == T ~NA_real_)

  ### Soltuion pathway 4:

  # i_hl_hlp_hh_ownership = case_when
  i_hl_hlp_shelter_doc = case_when(L_2_shelter_doctype == "99" ~NA_real_,
                                   L_2_shelter_doctype %in% c("written_agreement",
                                                              "land_title_deed") ~ T,
                                   T~F),
  i_hl_hlp_safe_shelter  = L_3_shelter_type %in% c(permanaent_sheter,transitional_shelter),

  i_hl_hlp_land_doc = case_when(L_7_land_doctype == "99" ~NA_real_,
                                   L_7_land_doctype %in% c("written_agreement",
                                                              "land_title_deed") ~ T,
                                   T~F),

  i_hl_hlp_land_disputes = case_when(L_9_HLP_dispute == "99" ~NA_real_,
                                     L_9_HLP_dispute %in% c("rarely","never") ~T,
                                     T~F),
  i_hl_hlp_eviction_risk = case_when(L_13_risk_eviction == "99"~NA_real_,
                                        L_13_risk_eviction %in% c("low","negligible") ~T,
                                        T~F),

  i_ld_hlp_legal_doc = case_when(
    L_14_doc_possessed.birth_certificate == T ~T,
    L_14_doc_possessed.passport == T ~T,
    L_14_doc_possessed.marriage_certificate == T ~T,
    L_14_doc_possessed.id_card == T ~T,
    T~ F),

  i_cg_cgr_adaptation = P_1_adapt_weather,
  i_cg_cgr_trust = case_when(R_5_idp_host_trust == 99 ~ NA_real_,
                             R_5_idp_host_trust %in% c(3:5)~T,
                             T~F),

  i_cr_cgr_violence = Q_6_victim_violence == 0,
  i_cr_cgr_feel_safe = Q_2_SafeInLocation %in% c("safe","very_safe"),

  i_cr_cgr_male_freedom = case_when(is.na(Q_3_MaleFreelyMove) ~NA_real_,
                                        Q_3_MaleFreelyMove == 99 ~NA_real_,
                                        Q_3_MaleFreelyMove == 1 ~ TRUE,
                                        T~FALSE),
  i_cr_cgr_female_freedom = case_when(is.na(Q_4_FemaleFreelyMove) ~NA_real_,
                                          Q_4_FemaleFreelyMove == 99 ~NA_real_,
                                          Q_4_FemaleFreelyMove == 1 ~ TRUE,
                                          T~FALSE)
)












