
# hh <- openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",sheet = 1)
# indv <- openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",sheet = 2)
source("07_R/index_function.R")

# cols --------------------------------------------------------------------

transitional_shelter <- c("somali_traditional_house_mundul",
                          "timber_and_mud_with_cgi_roof_baraako",
                          "plywood_wall_with_cgi_roof_darbiga_plywood",
                          "cgi_sheet_wall_and_roof",
                          "refugee_housing_unit")

permanaent_sheter <- c("permanent_shelter_mud_block",
                       "permanent_shelter_stone_brick_wall_with_cgi_roof")

#
# education <- c("primary_completed",
#                "madarassa_islamic_school",
#                "secondary_completed",
#                "university_vocational_training"
# )

# recoding ----------------------------------------------------------------

hh_recode <- hh |> mutate(

  ### cri INCIDATOR 1
  i1_cri_q_6_victim_violence = Q_6_victim_violence == 0,
  i1_cri_q_2_safeinlocation = Q_2_SafeInLocation %in% c("safe","very_safe"),
  i1_cri_q_3_malefreelymove = case_when(is.na(Q_3_MaleFreelyMove) ~NA_real_,
                                        Q_3_MaleFreelyMove == 99 ~NA_real_,
                                        Q_3_MaleFreelyMove == 1 ~ TRUE,
                                        T~FALSE),
  i1_cri_q_4_femalefreelymove = case_when(is.na(Q_4_FemaleFreelyMove) ~NA_real_,
                                          Q_4_FemaleFreelyMove == 99 ~NA_real_,
                                          Q_4_FemaleFreelyMove == 1 ~ TRUE,
                                          T~FALSE),
  i1_cri_q_7_assisst_recover_violence = case_when(is.na(Q_7_assisst_recover_violence) ~F,
                                                  Q_7_assisst_recover_violence == 99 ~NA_real_,
                                                  Q_7_assisst_recover_violence %in% c("yes_human_org","yes_gov_org") ~TRUE,
                                                  T~FALSE),
  i1_cri_p_1_adapt_weather = P_1_adapt_weather,

  ### cri INCIDATOR 2
  i2_cri_i_2_safe_access_food = I_2_safe_access_food,
  i2_cri_i_6_barriers_food_no_barrier = case_when(I_6_barriers_food.99 == T ~NA_real_,
                                                  T~I_6_barriers_food.77),
  i2_cri_i_5_no_food_3mth = case_when(I_5_no_food_3mth == 99 ~NA_real_,
                                      I_5_no_food_3mth == 0 ~ TRUE,
                                      T~FALSE),
  i2_cri_l_3_shelter_type = L_3_shelter_type %in% c(permanaent_sheter,transitional_shelter),

  i2_cri_j_3_healthcare_needed = case_when(J_3_healthcare_needed == "99" ~NA_real_,
                                           J_3_healthcare_needed %in% c("received_some_needed_healthcare",
                                                                        "received_all_needed_healthcare") ~TRUE,
                                           T~FALSE),
  i2_cri_j_4_barrier_health_no_barrier = case_when(J_4_barrier_health.99 ==T ~ NA_real_,
                                                   T~ J_4_barrier_health.77),
  i2_cri_k_2_safe_access_water = case_when(K_2_safe_access_water == 99 ~NA_real_,
                                           K_2_safe_access_water == 1 ~ TRUE,
                                           K_2_safe_access_water == 0 ~FALSE),

  i2_cri_k_3_distance_to_water = case_when(K_3_water_distance_dryseason == "99" ~NA_real_,
                                           K_3_water_distance_dryseason == "less_than_half_an_hour" ~T,
                                           T~F),
  i2_cri_k_5_access_water_dry = case_when(K_5_access_water_dry ==99 ~NA_real_,
                                          K_5_access_water_dry == 1 ~ TRUE,
                                          K_5_access_water_dry == 0 ~FALSE),
  # i2_cri_e_1_female_high_edu = case_when(E_1_female_high_edu == "no_female_in_age_to_be_educated_in_the_household" ~ NA_real_,
  #                                        E_1_female_high_edu == "99" ~ NA_real_,
  #                                        is.na(E_1_female_high_edu) ~NA_real_,
  #                                        E_1_female_high_edu  %in% education ~ TRUE,
  #                                        T~FALSE),
  # i2_cri_e_2_male_high_edu = case_when(E_2_male_high_edu == "no_male_in_age_to_be_educated_in_the_household" ~ NA_real_,
  #                                      E_2_male_high_edu == "99" ~ NA_real_,
  #                                      is.na(E_2_male_high_edu) ~NA_real_,
  #                                      E_2_male_high_edu  %in% education ~ TRUE,
  #                                      T~FALSE),
  i2_cri_e_5_hoh_read = case_when(E_5_hoh_read == 99 ~NA_real_,
                                  E_5_hoh_read == 1 ~T,
                                  T~F),
  i2_cri_e_8_barriers_education_no_barrier = case_when(E_8_barriers_education.99 == T ~ NA_real_,
                                                       T~E_8_barriers_education.77),
  i2_cri_n_1_source_electricity = case_when(N_1_source_electricity.99 == T ~ NA_real_,
                                            N_1_source_electricity.77 == T ~ FALSE,
                                            N_1_source_electricity.88 == T ~ FALSE,
                                            T~T),
  i2_cri_n_2_barrier_electricity_no_barrier = case_when(N_2_barrier_electricity.99 == T ~ NA_real_,
                                                        T~N_2_barrier_electricity.77),

  ### cri INCIDATOR 3
  i3_cri_f_3_income_source = case_when(F_3_income_source.99== T ~NA_real_,
                                       F_3_income_source.salaried_work == T ~T,
                                       F_3_income_source.income_from_selling_animals == T ~T,
                                       F_3_income_source.income_from_selling_animal_products == T ~T,
                                       F_3_income_source.income_from_own_business_or_commerce == T ~T,
                                       F_3_income_source.income_from_households_crops == T ~T,
                                       T~F),
  # i3_cri_f_4_occupation = case_when(F_4_occupation == "99" ~ NA_real_,
  #                                   F_4_occupation %in% c("paid_full_time_employment",
  #                                                         "paid_part_time_employment",
  #                                                         "self_employed") ~T,
  #                                   T~F),
  i3_cri_f_2_assistancen_none = F_2_assistance.77,
  i3_cri_g_1_income = case_when(G_1_income %in% c("more_than_50_usd") ~T,
                                T~F),
  i3_cri_h_1_debttype_no_debt =H_1_debttype.77,
  i3_cri_h_6_finane_account =  case_when(H_6_finane_account == 99 ~ NA_real_,
                                         H_6_finane_account == 1 ~ T,
                                         H_6_finane_account == 0 ~F),

  ### cri INCIDATOR 4
  # i4_cri_l_1_shelter_ownership = L_1_shelter_ownership == "own",
  # i4_cri_l_2_shelter_doctype = case_when(L_2_shelter_doctype == "99" ~ NA_real_,
  #                                        L_2_shelter_doctype %in% c("written_agreement",
  #                                                                   "land_title_deed") ~ T,
  #                                        T~F),
  # i4_cri_l_5_land_ownership = L_5_land_ownership == "own",

  i4_cri_l_7_land_doctype = case_when(L_7_land_doctype == "99" ~ NA_real_,
                                      L_7_land_doctype %in% c("written_agreement",
                                                              "land_title_deed") ~ T,
                                      T~F),


  # i4_cri_shelter_doc_type_rent = (L_1_shelter_ownership %in% c("rent") &
  #   L_2_shelter_doctype %in% c("written_agreement","land_title_deed"))|> as.numeric(),
  # i4_cri_shelter_doc_type_own = (L_1_shelter_ownership %in% c("own") &
  #   L_2_shelter_doctype %in% c("written_agreement","land_title_deed"))|> as.numeric(),
  #
  # i4_cri_land_doc_type_rent = (L_5_land_ownership %in% c("rent") &
  #   L_7_land_doctype %in% c("written_agreement","land_title_deed"))|> as.numeric(),
  # i4_cri_land_doc_type_own = (L_5_land_ownership %in% c("own") &
  #   L_7_land_doctype %in% c("written_agreement","land_title_deed")) |> as.numeric(),

  i4_cri_l_11_eviction = case_when(L_11_eviction == 99 ~ NA_real_,
                                   L_11_eviction == 0 ~T,
                                   L_11_eviction == 1 ~F),

  i4_cri_l_13_risk_eviction = case_when(L_13_risk_eviction == "99"~NA_real_,
                                        L_13_risk_eviction %in% c("low","negligible") ~T,
                                        T~F),
  i4_cri_l_9_hlp_dispute = case_when(L_9_HLP_dispute == "99" ~NA_real_,
                                     L_9_HLP_dispute %in% c("rarely","never") ~T,
                                     T~F),

  ### cri INCIDATOR 5
  i5_cri_l_14_doc_possessed = case_when(
    # L_14_doc_possessed.voting_registration == T ~T,
    # L_14_doc_possessed.driving_license == T ~T,
    L_14_doc_possessed.birth_certificate == T ~T,
    L_14_doc_possessed.passport == T ~T,
    L_14_doc_possessed.marriage_certificate == T ~T,
    L_14_doc_possessed.id_card == T ~T,
    # L_14_doc_possessed.humanitarian_service_card_scope_card == T ~T,
    T~ F),

  ### cri INCIDATOR 6

  i6_cri_d_4_absent_members = case_when(D_4_absent_members == 99 ~NA_real_,
                                        D_4_absent_members == 0 ~T,
                                        D_4_absent_members ==1 ~F),
  i6_cri_d_4_1_absent_assist_reunify = case_when(is.na(D_4_1_absent_assist_reunify)~T,
                                                 T ~ D_4_1_absent_assist_reunify),
  ### cri INCIDATOR 7
  i7_cri_t_3_public_meeting = case_when(T_3_public_meeting == "99" ~NA_real_,
                                        T_3_public_meeting == "regularly" ~T,
                                        T~F),

  i7_cri_t_1_participate_groups = case_when(T_1_participate_groups.77 == T ~F,
                                            T_1_participate_groups.77 == F ~T ),

  ### cri INCIDATOR 8
  i8_cri_u_1_legalservices_access = case_when(U_1_legalservices_access == 99 ~NA_real_,
                                              U_1_legalservices_access == 1 ~ T,
                                              T~F),
  i8_cri_u_2_justice_place = case_when(U_2_justice_place.other_community_organization == T ~T,
                                       U_2_justice_place.elder_councils_xeer == T ~T,
                                       U_2_justice_place.traditional_court == T ~T,
                                       U_2_justice_place.statutory_court == T ~T,
                                       U_2_justice_place.religious_court_leader == T ~T,
                                       U_2_justice_place.police_forces == T ~T,
                                       U_2_justice_place.community_leader == T ~T,
                                       T~ F),
  i8_cri_u_3_legalservices_effect = case_when(U_3_legalservices_effect == "not_effective" ~ F,
                                              T~T),

  ### cri INCIDATOR 9
  # i9_cri_h_4_barriers_loan = case_when(is.na(H_4_barriers_loan) ~ NA_real_,
  #                                      H_4_barriers_loan.99 == T ~ NA_real_,
  #                                      H_4_barriers_loan.lack_of_status_in_the_community==T ~F,
  #                                      T~T),

  i9_cri_i_6_barriers_food = case_when(is.na(I_6_barriers_food) ~ NA_real_,
                                       I_6_barriers_food.99 == T ~ NA_real_,
                                       I_6_barriers_food.unequal_access==T ~F,
                                       T~T),

  i9_cri_j_4_barrier_health = case_when(is.na(J_4_barrier_health) ~ NA_real_,
                                        J_4_barrier_health.99 == T ~ NA_real_,
                                        J_4_barrier_health.unequal_access==T ~F,
                                        T~T),
  i9_cri_k_6_barriers_water = case_when(is.na(K_6_barriers_water) ~ NA_real_,
                                        K_6_barriers_water.99 == T ~ NA_real_,
                                        K_6_barriers_water.unequal_access_to_water==T ~F,
                                        T~T),
  i9_cri_k_10_barriers_latrin = case_when(is.na(K_10_barriers_latrin) ~ NA_real_,
                                          K_10_barriers_latrin.99 == T ~ NA_real_,
                                          K_10_barriers_latrin.unequal_access_to_latrines==T ~F,
                                          T~T),
  i9_cri_l_8_barriers_shelter = case_when(is.na(L_8_barriers_shelter) ~ NA_real_,
                                          L_8_barriers_shelter.99 == T ~ NA_real_,
                                          L_8_barriers_shelter.unequal_access_idps_returnees_clan_groups==T ~F,
                                          T~T),
  i9_cri_m_1_barrier_transp = case_when(is.na(M_1_barrier_transp) ~ NA_real_,
                                        M_1_barrier_transp.99 == T ~ NA_real_,
                                        M_1_barrier_transp.unequal_access_idps_returnees_clan_groups==T ~F,
                                        T~T),
  i9_cri_n_2_barrier_electricity = case_when(is.na(N_2_barrier_electricity) ~ NA_real_,
                                             N_2_barrier_electricity.99 == T ~ NA_real_,
                                             N_2_barrier_electricity.unequal_access_electricity==T ~F,
                                             T~T),

  i9_cri_w_2_integrationlevel = case_when(is.na(W_2_IntegrationLevel)~NA_real_,
                                          W_2_IntegrationLevel %in% c(#"3_neutral",
                                                                      "4_integrated",
                                                                      "5_very_integrated") ~T,
                                          T~F)



)


# calculate subcriteria ---------------------------------------------------




hh_subcriteria <- hh_recode |>
  add_criteria_without_mix(new_col_name = "i1_sub_victims_of_violence",
                           cols = "i1_cri_q_6_victim_violence") |>
  add_criteria_with_mix(new_col_name = "i1_sub_freedom_movement",
                        cols = c("i1_cri_q_2_safeinlocation",
                                 "i1_cri_q_3_malefreelymove",
                                 "i1_cri_q_4_femalefreelymove"),
                        critical_in = c("i1_cri_q_2_safeinlocation")) |>

  add_criteria_without_mix(new_col_name = "i1_sub_protection_mecha",
                           cols = "i1_cri_q_7_assisst_recover_violence") |>

  add_criteria_without_mix(new_col_name = "i1_sub_drr",
                           cols = "i1_cri_p_1_adapt_weather" ) |>

  add_criteria_with_mix(new_col_name = "i2_sub_food_security",
                        cols = c("i2_cri_i_2_safe_access_food",
                                 "i2_cri_i_6_barriers_food_no_barrier",
                                 "i2_cri_i_5_no_food_3mth"),
                        critical_in = c("i2_cri_i_2_safe_access_food",
                                        "i2_cri_i_5_no_food_3mth")) |>
  add_criteria_without_mix(new_col_name = "i2_sub_shelter_housing",
                           cols = "i2_cri_l_3_shelter_type") |>
  add_criteria_without_mix(new_col_name = "i2_sub_medical_services",
                           cols = c("i2_cri_j_3_healthcare_needed",
                                    "i2_cri_j_4_barrier_health_no_barrier")) |>
  add_criteria_with_mix(new_col_name = "i2_sub_water",
                        cols = c("i2_cri_k_2_safe_access_water",
                                 "i2_cri_k_3_distance_to_water",
                                 "i2_cri_k_5_access_water_dry"),
                        critical_in = c("i2_cri_k_2_safe_access_water",
                                        "i2_cri_k_5_access_water_dry")) |>
  add_criteria_with_mix(new_col_name = "i2_sub_education",
                        cols = c(
                          # "i2_cri_e_1_female_high_edu",
                          # "i2_cri_e_2_male_high_edu",
                          "i2_cri_e_5_hoh_read",
                          "i2_cri_e_8_barriers_education_no_barrier"),
                        critical_in = c("i2_cri_e_5_hoh_read",
                                        "i2_cri_e_8_barriers_education_no_barrier")
  ) |>
  add_criteria_without_mix(new_col_name = "i2_sub_electricity",
                           cols =c("i2_cri_n_1_source_electricity",
                                   "i2_cri_n_2_barrier_electricity_no_barrier") ) |>
  add_criteria_without_mix(new_col_name = "i3_sub_emp_livelihood",
                           cols = c("i3_cri_f_3_income_source"
                                    # ,"i3_cri_f_4_occupation"
                           )) |>
  add_criteria_with_mix(new_col_name = "i3_sub_economic_security",
                        cols = c("i3_cri_f_2_assistancen_none",
                                 "i3_cri_g_1_income",
                                 "i3_cri_h_1_debttype_no_debt",
                                 "i3_cri_h_6_finane_account"),
                        critical_in ="i3_cri_g_1_income" ) |>

  # add_criteria_without_mix(new_col_name = "i4_sub_landdoc",
  #              cols = c("i4_cri_l_7_land_doctype"
  #              )) |>
  # add_criteria_with_mix(new_col_name = "i4_sub_risk_eviction",
  #                       cols = c("i4_cri_l_11_eviction",
  #                                "i4_cri_l_13_risk_eviction",
  #                                "i4_cri_l_9_hlp_dispute"
  #                       ),critical_in = c("i4_cri_l_11_eviction",
  #                                         "i4_cri_l_13_risk_eviction")) |>

  add_criteria_with_mix(new_col_name = "i4_sub_land_doc_and_risk_eviction",
                        cols = c("i4_cri_l_11_eviction",
                                 "i4_cri_l_13_risk_eviction",
                                 "i4_cri_l_9_hlp_dispute",
                                 "i4_cri_l_7_land_doctype"
                        ),critical_in = c("i4_cri_l_7_land_doctype")) |>




  add_criteria_without_mix(new_col_name = "i5_sub_documentation",
                           cols ="i5_cri_l_14_doc_possessed" ) |>

  add_criteria_without_mix(new_col_name = "i6_sub_voluntary_reunification",
                           cols = "i6_cri_d_4_absent_members") |>

  add_criteria_without_mix(new_col_name = "i6_sub_renunification_tracing",
                           cols = "i6_cri_d_4_1_absent_assist_reunify" ) |>

  add_criteria_without_mix(new_col_name = "i7_sub_public_affairs",
                           cols = "i7_cri_t_3_public_meeting") |>

  add_criteria_without_mix(new_col_name = "i7_sub_right_to_engage",
                           cols = "i7_cri_t_1_participate_groups") |>

  add_criteria_with_mix(new_col_name = "i8_sub_remedies_justice",
                        cols = c("i8_cri_u_1_legalservices_access",
                                 "i8_cri_u_2_justice_place",
                                 "i8_cri_u_3_legalservices_effect"),
                        critical_in = "i8_cri_u_1_legalservices_access" ) |>

  add_criteria_with_mix(new_col_name = "i9_sub_access_to_service" ,
                        cols = c("i9_cri_i_6_barriers_food", "i9_cri_j_4_barrier_health",
                                 "i9_cri_k_6_barriers_water", "i9_cri_k_10_barriers_latrin",
                                 "i9_cri_l_8_barriers_shelter",
                                 "i9_cri_m_1_barrier_transp", "i9_cri_n_2_barrier_electricity"),
                        critical_in = c("i9_cri_i_6_barriers_food",
                                        "i9_cri_j_4_barrier_health",
                                        "i9_cri_k_6_barriers_water")) |>
  add_criteria_without_mix(new_col_name = "i9_sub_intregation_level",
                        cols = c("i9_cri_w_2_integrationlevel")) |>
  select(X_uuid,
         contains("_sub_"))



# calculate sub -----------------------------------------------------------


hh_ds_criteria <- hh_subcriteria |>
  add_criteria_without_mix(new_col_name = "i_ds_cri1_ss",
                           cols = c("i1_sub_victims_of_violence",
                                    "i1_sub_freedom_movement",
                                    "i1_sub_protection_mecha",
                                    "i1_sub_drr")) |>
  add_criteria_without_mix(new_col_name = "i_ds_cri2_adequare_standard_of_living",
                           cols = c("i2_sub_food_security",
                                    "i2_sub_shelter_housing",
                                    "i2_sub_medical_services",
                                    "i2_sub_water",
                                    "i2_sub_education",
                                    "i2_sub_electricity")) |>
  add_criteria_without_mix(new_col_name = "i_ds_cri3_access_to_livelihood",
                           cols =c("i3_sub_emp_livelihood","i3_sub_economic_security")) |>
  # add_criteria_without_mix(new_col_name = "i_ds_cri4_hlp" ,
  #                          cols =c("i4_sub_landdoc","i4_sub_risk_eviction")) |>
    add_criteria_without_mix(new_col_name = "i_ds_cri4_hlp" ,
                             cols =c("i4_sub_land_doc_and_risk_eviction")) |>


  add_criteria_without_mix(new_col_name = "i_ds_cri5_access_to_document",
                           cols ="i5_sub_documentation") |>
  add_criteria_without_mix(new_col_name = "i_ds_cri6_family_reunification",
                           cols =c("i6_sub_voluntary_reunification",
                                   "i6_sub_renunification_tracing")) |>
  add_criteria_without_mix(new_col_name = "i_ds_cri7_pp_affairs" ,
                           cols =c("i7_sub_public_affairs","i7_sub_right_to_engage")) |>
  add_criteria_without_mix(new_col_name = "i_ds_cri8_access_to_remedies" ,
                           cols = "i8_sub_remedies_justice") |>
  add_criteria_without_mix(new_col_name = "i_ds_cri9_integration",
                           cols = c("i9_sub_access_to_service",
                                    "i9_sub_intregation_level")
                        # ,critical_in = "i9_sub_intregation_level"
                        ) |>
  select(X_uuid,
         starts_with("i_ds_cri"))


names(hh_ds_criteria) [names(hh_ds_criteria) %in% names(hh_recode)]

hh_all_indicator <- hh_recode |>
  left_join(hh_subcriteria) |>
  left_join(hh_ds_criteria)


# calculate ---------------------------------------------------------------

hh_data <- hh_all_indicator |> mutate(
  dsp_value = rowSums(hh_ds_criteria[c("i_ds_cri1_ss",
                                       "i_ds_cri2_adequare_standard_of_living",
                                       "i_ds_cri3_access_to_livelihood",
                                       "i_ds_cri4_hlp",
                                       "i_ds_cri5_access_to_document")]),
  dsp_index =  dsp_value|>as.character(),

  dsp_value_6 = (i_ds_cri1_ss*(5/6) +
                i_ds_cri2_adequare_standard_of_living*(5/6) +
                i_ds_cri3_access_to_livelihood*(5/6) +
                i_ds_cri4_hlp*(5/6) +
                i_ds_cri5_access_to_document*(5/6) +
                i_ds_cri9_integration*(5/6)),

  dsp_index_6 =  dsp_value_6 |> as.character()
) |> select(dsp_index,dsp_value,dsp_index_6,dsp_value_6,everything())


rm(list=setdiff(ls(), "hh_data"))
#
# # hc_progress ------------------------------------------------------------
# #
# hh_svy_hc <- hh_dsp_index |> filter(pop_group  == "hc")  |>
#   select(strata,dsp_index)|> as_survey()
#
# hc_progress <- analysistools::create_analysis(design = hh_svy_hc,group_var = "strata")$results_table |>
#   filter(analysis_var == "dsp_index") |> select(analysis_var_value,group_var_value, stat) |>
#   mutate(stat = paste0(round(stat*100),"%"),
#          group_var_value = case_when(is.na(group_var_value)~ "Overall",
#                                      T~group_var_value)) |>
#   pivot_wider(id_cols = "group_var_value",names_from = "analysis_var_value",values_from = stat)
#
#
#
# # idp_progress ------------------------------------------------------------
#
# hh_svy_idp <- hh_dsp_index |> filter(pop_group  == "idp")  |>
#   select(strata,survey_weights,dsp_index,City_Name)|> as_survey(strata = strata ,weights = survey_weights)
#
# idp_progress <- analysistools::create_analysis(design = hh_svy_idp,group_var = "City_Name")$results_table |>
#   filter(analysis_var == "dsp_index") |> select(analysis_var_value,group_var_value, stat) |>
#   mutate(stat = paste0(round(stat*100),"%"),
#          group_var_value = case_when(is.na(group_var_value)~ "Overall",
#                                      T~group_var_value)) |>
#   pivot_wider(id_cols = "group_var_value",names_from = "analysis_var_value",values_from = stat)
#
#
#
# col <- hh_dsp_index |> select(starts_with("i")) |>
#   select(contains(c("cri","sub"))) |> names() |> sort()
#
#
# df <- hh_dsp_index |> select(col)




