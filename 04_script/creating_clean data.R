rm(list = ls())
library(tidyverse)
library(cleaningtools)
library(openxlsx)
library(illuminate)
library(readxl)
library(analysistools)


audit_path <- "01_input/01_audit/aB6h5dgnuwEVzP3Kgch2i4_2024_06_13_07_26_08.zip"

# read data ---------------------------------------------------------------

# v1 ----------------------------------------------------------------------
hh_v1<- read.csv("01_input/02_raw_data/01_version_1/hh_v1.csv",na.strings = "") |> fix_data_type(character_cols = "Q_5_1_other")
indv_v1<- read.csv("01_input/02_raw_data/01_version_1/indv_v1.csv",na.strings = "")

# v2 ----------------------------------------------------------------------

hh_v2<- read.csv("01_input/02_raw_data/02_version_2/hh_data.csv",na.strings = "") |> fix_data_type(character_cols = "Q_5_1_other")
indv_v2<- read.csv("01_input/02_raw_data/02_version_2/indv.csv",na.strings = "")


# merging <-  -------------------------------------------------------------

hh_data <- hh_v1 |> bind_rows(hh_v2) |> mutate(
  date = start |>  as.Date()
)
indv_data <- indv_v1 |> bind_rows(indv_v2) |> rename(
  X_uuid = X_submission__uuid
) |> select(-ends_with("_calc"))



# remove duplicate --------------------------------------------------------

hh_data <- hh_data |>
  distinct(deviceid,X_uuid,A_6_neighbourhood,current_lat,current_long,.keep_all = T)

indv_data <- indv_data|>
  distinct(roster_count,age_years, sex_member,
           resp_hhh,
           C_5_1_relation_to_HoH,
           C_5_1_1_other, C_7_HoH_marital,X_uuid,.keep_all = T)


indv_data <- indv_data |> mutate(
  resp_hhh.hhh = case_when(X_uuid == "2b9f1930-4fc7-4f1e-a0f6-7b972fdb4f3d" & roster_count == 1 ~ 0,
                           X_uuid == "7d9c23d1-0444-4661-80aa-20e6967b837e" & roster_count == 1 ~ 0,
                           X_uuid == "d284ae6c-c88d-4028-8baa-a72fb01127ba" & roster_count == 1 ~ 0,
                           T~resp_hhh.hhh),
  resp_hhh.respondent  = case_when( X_uuid == "7d9c23d1-0444-4661-80aa-20e6967b837e" & roster_count == 1 ~ 1,
                                    X_uuid == "d284ae6c-c88d-4028-8baa-a72fb01127ba" & roster_count == 1 ~ 1,
                                    T~resp_hhh.respondent ),

  resp_hhh = case_when(X_uuid == "2b9f1930-4fc7-4f1e-a0f6-7b972fdb4f3d" & roster_count == 1 ~ "respondent",
                       X_uuid == "7d9c23d1-0444-4661-80aa-20e6967b837e" & roster_count == 1 ~ "respondent" ,
                       X_uuid == "d284ae6c-c88d-4028-8baa-a72fb01127ba" & roster_count == 1 ~ "respondent" ,
                       T~resp_hhh)
)

hh_data$D_2_disp_status |> unique()

# data cleaning -----------------------------------------------------------

file_list <- list.files(path = "03_output/03_filled_cleaning_log/",all.files = T,include.dirs = T,recursive = T,full.names = T,pattern = "xlsx")

# file_list_1 <- file_list_all[!grepl("cleaning_log_full_",file_list_all)]
# file_list <- file_list_1[!grepl("/~",file_list_1)]


cl_list <- list()
for(i in file_list ) {
  cl_list[[i]] <- openxlsx::read.xlsx(i) |> mutate_all(as.character)
}

all_cl <- bind_rows(cl_list) |> select(X_uuid,action,question,old_value,new_value,A_5_city)


#

# v1_duration -------------------------------------------------------------
audits_list_v1 <- create_audit_list(audit_zip_path = "01_input/01_audit/V1_audit/a8wouhpyJNgiLxLS2Rv5tH_2024_05_14_11_25_51.zip")

duration_v1 <- hh_v1 |> filter(B_1_consent==1) |>
  add_duration_from_audit(audit_list = audits_list_v1,
                          start_question = "B_informed_consent/B_1_consent",
                          end_question = "X_Closing/note_X_closing",
                          uuid_column = "X_uuid" ,
  ) |> select(X_uuid,duration_audit_start_end_minutes)


# v2_duration -------------------------------------------------------------
audits_list <- create_audit_list(audit_path)

duration_v2 <- hh_v2 |> filter(B_1_consent==1) |>
  add_duration_from_audit(audit_list = audits_list,
                          start_question = "B_informed_consent/B_1_consent",
                          end_question = "X_Closing/note_X_closing",
                          uuid_column = "X_uuid" ,
  )|> select(X_uuid,duration_audit_start_end_minutes)

rm(hh_v1,hh_v2,indv_v1,indv_v2)


# duration_join -----------------------------------------------------------

all_dl <- duration_v1 |> bind_rows(duration_v2) |> filter(
  duration_audit_start_end_minutes < 10
)  |>
  filter(is.na(duration_audit_start_end_minutes) | duration_audit_start_end_minutes >0)



all_cl_hh <- all_cl |> filter(question != "age_years") |> bind_rows(
  all_dl |> mutate(
    action = "remove_survey"
  )
) |> filter(!is.na(X_uuid))



# dataclean ---------------------------------------------------------------

clean_data_hh <- cleaningtools::create_clean_data(raw_dataset = hh_data,
                                                  raw_data_uuid_column = "X_uuid",
                                                  cleaning_log = all_cl_hh,
                                                  cleaning_log_uuid_column = "X_uuid",
                                                  cleaning_log_question_column = "question",
                                                  cleaning_log_new_value_column =  "new_value",
                                                  cleaning_log_change_type_column = "action",
                                                  change_response_value = "change_value",
                                                  NA_response_value = "blank_response",
                                                  no_change_value =  "no_action",
                                                  remove_survey_value = "remove_survey" ) |>
  filter(B_1_consent) |>
  mutate(
    City_Name = case_when(A_5_city == "Baardheere" ~ "Baardheere",
                          A_5_city == "Baidoa" ~ "Baydhaba",
                          A_5_city == "Berdaale" ~ "Berdaale",
                          A_5_city == "Doolow" ~ "Doolow",
                          A_5_city == "Kismaayo" ~ "Kismaayo",
                          A_5_city == "MogDaynile" ~ "MogDaynile",
                          A_5_city == "MogKahda" ~ "MogKahda",
                          A_5_city == "Xudur" ~ "Xudur"),
    pop_group = case_when(D_2_disp_status %in% c("internally_displaced_person",
                                                 "returnee_from_a_place_within_somalia_internal_displacement") ~"idp",
                          T~"hc")
  ) |> mutate(
    City_Name = case_when(pop_group == "hc" & grepl("Mog",City_Name) ~ "Mogadishu",
                          T~City_Name)
  ) |> filter(!(pop_group == "idp" & A_6_neighbourhood %in% c("Adaado_", "Daynile (SE)", "Daynile (SW)", "Durasalam_", "Horseed_",
                                                              "Isha_", "Kahda (SE)", "Kahda (SW)", "Madina", "Salamey_", "Wadajiir_",
                                                              "Wadajir"))) |>
  select(A_5_city,City_Name,everything())


clean_data_hh <- clean_data_hh |> mutate(
  neighbourhood_final = case_when(pop_group == "idp" & City_Name == "Baydhaba" &
                                    grepl("Adaado_CA",A_6_neighbourhood) ~ "Adaado_b",

                                  pop_group == "idp" & City_Name == "Baydhaba" &
                                    grepl("Durasalam_CA",A_6_neighbourhood) ~ "Durasalam_b",

                                  pop_group == "idp" & City_Name == "Baydhaba" &
                                    grepl("Horseed_CA",A_6_neighbourhood) ~ "Horseed_b",

                                  pop_group == "idp" & City_Name == "Baydhaba" &
                                    grepl("Howl Wadaag_CA",A_6_neighbourhood) ~ "Howl Wadaag_b",

                                  pop_group == "idp" & City_Name == "Baydhaba" &
                                    grepl("Isha_CA",A_6_neighbourhood) ~ "Isha_b",

                                  pop_group == "idp" & City_Name == "Baydhaba" &
                                    grepl("Salamey_CA",A_6_neighbourhood) ~ "Salamey_b",


                                  pop_group == "idp" & City_Name == "Baydhaba" &
                                    grepl("Towfiq_CA",A_6_neighbourhood) ~ "Towfiq_b",

                                  pop_group == "idp" & City_Name == "Baydhaba" &
                                    grepl("Wadajiir_CA",A_6_neighbourhood) ~ "Wadajiir_b",

                                  pop_group == "idp" & City_Name == "Baydhaba" &
                                    grepl("Weberi_CA",A_6_neighbourhood) ~ "Weberi_b",

                                  T~A_6_neighbourhood



                                  )
)

clean_data_hh |> filter(pop_group == "idp") |> pull(neighbourhood_final) |> unique() |> sort()


# indiv -------------------------------------------------------------------

all_cl_indv <- all_cl |> filter(question %in% names(indv_data)) |> bind_rows(
  all_dl |> mutate(
    action = "remove_survey"
  )
) |> filter(!is.na(X_uuid))  |> filter(action %in% c("change_value","blank_response"))


######## for loop

clean_data_indv <- indv_data |>
  mutate(age_years = as.character(age_years))

for(i in 1:nrow(all_cl_indv)){
  uuid <- all_cl_indv[i,]["X_uuid"] |> pull()
  old = all_cl_indv[i,]["old_value"] |> pull()
  new = all_cl_indv[i,]["new_value"]|> pull()

  clean_data_indv <- clean_data_indv |> mutate(
    age_years = case_when(X_uuid == uuid &
                            age_years == old ~ new,
                          T~age_years)
  )

}



clean_data_indv <- clean_data_indv |> filter(X_uuid %in% clean_data_hh$X_uuid )



# INDV Population group -------------------------------------------------------------

clean_data_indv <- clean_data_indv |> left_join(clean_data_hh |> select(X_uuid,pop_group))



# create weights (HH) ----------------------------------------------------------


hc_sample <- openxlsx::read.xlsx("02_dap/LORA_Sample_frame_v2.xlsx",1) |>
  select(City_Name,Strata,LORA_urban_ID,
         population = frame_HC_HH,
         target= Sample_target)



clean_data_hc <- clean_data_hh |> filter(B_1_consent) |>
  filter(pop_group == "hc")  |> group_by(City_Name) |> summarise(
    completed_survey = n()
  )


hc_sample_weights <- hc_sample |> left_join(clean_data_hc) |>
  mutate(survey_weights = (population/sum(population))/(completed_survey/sum(completed_survey)) ) |> mutate(
    strata = paste0("hc","-",City_Name)
  )



# create weights (IDP) ----------------------------------------------------------

new_idp_pop <- openxlsx::read.xlsx("02_dap/Updated_pop_figures.xlsx",sheet = 3) |>
  filter(Type_Label == "IDPs")

new_idp_pop <- new_idp_pop |>
  select(ADM2_EN,NE_UID,Neig_Vil_C,IDP_HHs_su) |>
  mutate(
    strata = case_when(ADM2_EN == "Xudur" & Neig_Vil_C == "Horseed" &
                         NE_UID == "SO2501_UR_001_NE_005" ~ "Horseed_xudur",
                       T~Neig_Vil_C)
  ) |> select(strata,new_pop = IDP_HHs_su)

# idp_sample$strata %in%new_idp_pop$strata |> table()
# new_idp_pop$strata %in%idp_sample$strata |> table()


idp_sample <- openxlsx::read.xlsx("02_dap/LORA_Sample_frame_v2.xlsx",2)  |> select(
  City_Name,
  strata,
  join_id,
  Lora_urban_id,
  neighbourhood_ID,
  neighbourhood_name,
  EA,
  Neighbourhood_CA_concat,
  population = IDP_HH_frame,
  target= Sample_target

) |> filter(target >0 ) |> left_join(new_idp_pop) |> rename(
  old_pop = population
) |> mutate(
  population = case_when(is.na(new_pop) ~ old_pop,T~new_pop)
) |> relocate(target,.after = population)



# fixing idp -baidoa ------------------------------------------------------

idp_sample <- idp_sample |> mutate(
  strata= case_when(City_Name == "Baydhaba" ~paste0(neighbourhood_name,"_b"),
                    T~strata)
) |> group_by(City_Name,strata) |> summarise_if(is.numeric,sum)




clean_data_idp <- clean_data_hh |> filter(B_1_consent) |>
  filter(pop_group != "hc") |>
  group_by(neighbourhood_final) |> summarise(
    completed_survey = n()
  )

####### temp end

# actaul_targets ----------------------------------------------------------


if(sum(unique(clean_data_idp$neighbourhood_final) %in% idp_sample$strata == F) >0){stop("check the list")}


unique(clean_data_idp$neighbourhood_final)[!unique(clean_data_idp$neighbourhood_final) %in% idp_sample$strata]


idp_sample_wights <- idp_sample |>
  left_join(clean_data_idp,by = c("strata" = "neighbourhood_final")) |>
  mutate(survey_weights = (population/sum(population))/(completed_survey/sum(completed_survey)),
         strata = paste0("idp","-",strata)
  ) |> select(c("City_Name", "strata", "old_pop",
                "new_pop", "population", "target", "completed_survey",
                "survey_weights",
  ))



# weighting frame ---------------------------------------------------------

wf <- hc_sample_weights |> select(strata,survey_weights) |>
  bind_rows(idp_sample_wights |> select(strata,survey_weights))

wf$strata |> duplicated() |> table()

# hh_data add weights -----------------------------------------------------

clean_data_hh_with_weights <- clean_data_hh |> mutate(
  strata =  case_when(pop_group == "hc" ~ paste0(pop_group,"-",City_Name),
                      T ~ paste0(pop_group,"-",neighbourhood_final))
)  |> left_join(wf |> select(-City_Name))|> mutate(
  survey_weights = case_when(pop_group == "hc"~ NA_real_,
                             T~survey_weights)
)


uuid_weights <- clean_data_hh_with_weights |> select(X_uuid,strata,survey_weights)


clean_data_indv_with_weights <- clean_data_indv |> left_join(uuid_weights)



# write_clean_data --------------------------------------------------------



write_list <- list(clean_data_hh_with_weights = clean_data_hh_with_weights,
                   clean_data_indv_with_weights = clean_data_indv_with_weights,
                   idp_sample_wights = idp_sample_wights
)
analyticaid::write_formatted_excel(write_list,"03_output/04_clean_data/clean_data.xlsx",header_fill_color = "lightblue")






