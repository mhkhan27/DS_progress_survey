rm(list = ls())
library(tidyverse)
library(cleaningtools)
library(openxlsx)
library(illuminate)
library(readxl)
library(analysistools)

# install.packages("devtools")
# devtools::install_github("peterdalle/moe")
library(moe)



df <- openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",1) |>
  filter(B_1_consent)
df$D_2_disp_status |> unique()

df_2 <- df |> mutate(
  sample_type_up = case_when(D_2_disp_status %in% c("internally_displaced_person",
                                                    "returnee_from_a_place_within_somalia_internal_displacement") ~ "idp",
                             T~"hc")
)






# create weights (IDP) ----------------------------------------------------------

new_idp_pop <- openxlsx::read.xlsx("02_dap/Updated_pop_figures.xlsx",sheet = 3) |> filter(Type_Label == "IDPs")

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




# idp ---------------------------------------------------------------------


clean_data_idp <- df_2 |> filter(B_1_consent) |>
  filter(sample_type_up != "hc") |>
  group_by(A_6_neighbourhood) |> summarise(
    completed_survey = n()
  )

unique(clean_data_idp$A_6_neighbourhood)[!unique(clean_data_idp$A_6_neighbourhood) %in% idp_sample$strata]


if(sum(unique(clean_data_idp$A_6_neighbourhood) %in% idp_sample$strata == F) >0){stop("check the list")}


unique(clean_data_idp$A_6_neighbourhood)[!unique(clean_data_idp$A_6_neighbourhood) %in% idp_sample$strata]


idp_sample_wights <- idp_sample |>
  left_join(clean_data_idp,by = c("strata" = "A_6_neighbourhood")) |>
  mutate(survey_weights = (population/sum(population))/(completed_survey/sum(completed_survey)),
         # survey_weights_criteria = (population/sum(population))/(completed_survey_criteria/sum(completed_survey_criteria)) ) |> mutate(
         strata = paste0("idp","-",strata)
  ) |>
  # mutate(
  # minimum_target = target - round((target*.1)),
  # criteria_based_representativeness = case_when(minimum_target>completed_survey_criteria ~ "Not_rep",
  #                                               T~"Rep")) |>
  select(c("City_Name", "strata", "join_id", "Lora_urban_id", "neighbourhood_ID",
           "neighbourhood_name", "EA", "Neighbourhood_CA_concat", "old_pop",
           "new_pop", "population", "target", "completed_survey",
           # "minimum_target", "completed_survey_criteria","host_community_resident", "internally_displaced_person", "refugee",
           # "returnee_from_a_place_within_somalia_internal_displacement",
           # "returnee_from_abroad", "criteria_based_representativeness","survey_weights_criteria",
           "survey_weights",
  ))


moe_df <- list()
for(i in unique(idp_sample_wights$strata)) {

  df <- idp_sample_wights |> filter(strata ==i)

   mo_e  <- moe(proportion = .5,
      n = df$completed_survey[1],
      conf.level =.9,
      digits = 2,
      population.correction = T,
      population.size = df$population[1])

   moe_df[[i]] <- data.frame(
     strata = i,
     moe = mo_e$margin.of.error
   )
}


moe_df_f <- moe_df |> bind_rows()



idp_sample_wights <- idp_sample_wights |> left_join(moe_df_f)

idp_sample_with_new_moe <- idp_sample_wights |> select(City_Name,strata,target,completed_survey,
                                                       mergin_of_error = moe) |> mutate(
                                                         mergin_of_error   = mergin_of_error/100,
                                                       CI = .9)

analyticaid::write_formatted_excel(list(idp_sample_with_new_moe=idp_sample_with_new_moe),
                                   "IDP_MoE.xlsx",header_fill_color = "lightblue")



# hc ----------------------------------------------------------------------

hc_sample <- openxlsx::read.xlsx("02_dap/LORA_Sample_frame_v2.xlsx",1) |>
  select(City_Name,Strata,LORA_urban_ID,
         population = frame_HC_HH,
         target= Sample_target)

clean_data_hc <- df_2 |> filter(B_1_consent) |>
  filter(sample_type_up == "hc") |>
  mutate(City_Name = case_when(grepl("Mog",City_Name) ~"Mogadishu" ,T~City_Name)) |>
  group_by(City_Name) |> summarise(
    completed_survey = n()
  )


moe_hc <- list()
for(i in unique(clean_data_hc$strata)) {

  df <- idp_sample_wights |> filter(strata ==i)

  mo_e  <- moe(proportion = .5,
               n = df$completed_survey[1],
               conf.level =.9,
               digits = 2,
               population.correction = T,
               population.size = df$population[1])

  moe_hc[[i]] <- data.frame(
    strata = i,
    moe = mo_e$margin.of.error
  )
}


