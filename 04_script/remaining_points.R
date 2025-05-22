rm(list = ls())
library(tidyverse)
library(cleaningtools)
library(openxlsx)
library(illuminate)



# read data ---------------------------------------------------------------

# v1 ----------------------------------------------------------------------
hh_v1<- read.csv("01_input/02_raw_data/01_version_1/hh_v1.csv",na.strings = "") |> fix_data_type(character_cols = "Q_5_1_other")
# v2 ----------------------------------------------------------------------

hh_v2<- read.csv("01_input/02_raw_data/02_version_2/hh_data.csv",na.strings = "") |> fix_data_type(character_cols = "Q_5_1_other")



# merging <-  -------------------------------------------------------------

hh_data <- hh_v1 |> bind_rows(hh_v2) |> mutate(
  date = start |>  as.Date()
)
rm(hh_v1,hh_v2)



#######

file_list <- list.files(path = "03_output/03_filled_cleaning_log/",all.files = T,include.dirs = T,recursive = T,full.names = T,pattern = "xlsx")

# file_list_1 <- file_list_all[!grepl("cleaning_log_full_",file_list_all)]
# file_list <- file_list_1[!grepl("/~",file_list_1)]
#

read_list <- list()
for(i in file_list ) {
  read_list[[i]] <- openxlsx::read.xlsx(i) |> mutate_all(as.character)
}


all_cl <- bind_rows(read_list) |> select(X_uuid,action,question,old_value,new_value,A_5_city)

all_change_type <- all_cl |> filter(action == "change_value")


all_change_type <-all_change_type |> filter(question %in% names(hh_data))



# dataclean ---------------------------------------------------------------

clean_data <- cleaningtools::create_clean_data(raw_dataset = hh_data,
                                 raw_data_uuid_column = "X_uuid",
                                 cleaning_log = all_change_type,
                                 cleaning_log_uuid_column = "X_uuid",
                                 cleaning_log_question_column = "question",
                                 cleaning_log_new_value_column =  "new_value",
                                 cleaning_log_change_type_column = "action",
                                 change_response_value = "change_value",
                                 NA_response_value = "blank_response",
                                 no_change_value =  "no_action",
                                 remove_survey_value = "remove_survey" )



clean_data <- clean_data |> filter(!is.na(A_12_pindrop))


rm(list=setdiff(ls(), "clean_data"))

all_points <- clean_data$A_12_pindrop |> unique()

"SO2501_UR_001_NE_009_P_181" %in% all_points


remaining_sample_from_from_new_df <-openxlsx::read.xlsx("03_output/01_remaining_surveys/2024_06_13_remaining_surveys.xlsx")
deleted_survey <-openxlsx::read.xlsx("03_output/01_remaining_surveys/2024_06_13_remaining_surveys.xlsx",2,detectDates = T)

# point -------------------------------------------------------------------
write_list <- list()
sample_point <- read.csv("01_input/03_target_points/pindrops.csv")
remaining_summary_df <- remaining_sample_from_from_new_df |>
  # filter(City_Name %in% c(  "Baydhaba","Mog.Daynile","Mog.Kahda"))  |>
  filter(Remaining.survey>0) |>
  filter(location_type == "non_camp")



# finding out remaining primary point -------------------------------------

remaining_points_primary <- sample_point |> mutate(
  survyed = name %in% all_points
) |>
   filter(!survyed) |>
  #filter(ADM2_EN %in% c(  "Baydhaba","Daynile","Kahda")) |>
  filter(join_id %in% remaining_summary_df$A_11_enum_area) |>
   filter(grepl("P_",label))  |>
  select(c("name","label","City_Name","join_id" ))



# remaining_points_primary > to joinlist

# remaining primary point at neighbourhood level --------------------------

remaining_primany_point_by_neighbourhood <- remaining_points_primary |>
  group_by(join_id) |> summarise(
  remaining_primary_point=n()
)  |> mutate(location_type = "non_camp") |> rename(
  A_11_enum_area = join_id
)

# joining with reaminig sample frame to find our p vs s
remaining_point_by_ps <- remaining_summary_df |>
  left_join(remaining_primany_point_by_neighbourhood)
remaining_point_by_ps[is.na(remaining_point_by_ps)]  <- 0

remaining_point_by_ps <- remaining_point_by_ps|>
  mutate(remaining_primary_point =
           case_when(remaining_primary_point > Remaining.survey ~ Remaining.survey,
                     T~ remaining_primary_point))

remaining_point_by_ps <- remaining_point_by_ps|> mutate(
    new_secondary_points = Remaining.survey - remaining_primary_point
  )


# primary -point final ----------------------------------------------------------
# only primary points/primary sample frame ---------------------------------------------------
remaining_primary_frame <- remaining_point_by_ps |>
  filter(remaining_primary_point >0) #|>
# filter((new_secondary_points+remaining_primary_point+Consent.Yes-deleted_survey) !=target)


##  secondary sample

primary_sample <- sample_point |> mutate(
  survyed = name %in% all_points
) |>
  filter(!survyed) |>
  #filter(ADM2_EN %in% c(  "Baydhaba","Daynile","Kahda")) |>
  filter(join_id %in% remaining_primary_frame$A_11_enum_area) |>
  filter(grepl("P_",label))  |>
  select(c("name","label","ADM2_EN","join_id" ))




remaing_primary_to_collect<- list()

for(i in unique(remaining_primary_frame$A_11_enum_area)) {
  sample_point_needed <-  remaining_primary_frame |> filter(A_11_enum_area==i) |> pull(remaining_primary_point)
  Neigbourhood.Name <-  remaining_primary_frame |> filter(A_11_enum_area==i) |> pull(neighbourhood)
  A_11_enum_area <-  remaining_primary_frame |> filter(A_11_enum_area==i) |> pull(A_11_enum_area)
  City_Name <-  remaining_primary_frame |> filter(A_11_enum_area==i) |> pull(city)

  sample_frame <- primary_sample |> filter(join_id == i)
  remaing_primary_to_collect[[i]] <- sample_frame[sample(nrow(sample_frame), sample_point_needed), ] |> mutate(
    Neigbourhood.Name = Neigbourhood.Name,
    A_11_enum_area = A_11_enum_area,
    City_Name =City_Name
  )


}

remaing_primary_to_collect_df <- bind_rows(remaing_primary_to_collect)

# only secondary points/secondary sample frame ---------------------------------------------------
remaining_secondary_frame <- remaining_point_by_ps |>
  filter(new_secondary_points >0) #|>
  # filter((new_secondary_points+remaining_primary_point+Consent.Yes-deleted_survey) !=target)


##  secondary sample

secondary_sample <- sample_point |> mutate(
  survyed = name %in% all_points
) |>
  filter(!survyed) |>
  #filter(ADM2_EN %in% c(  "Baydhaba","Daynile","Kahda")) |>
  filter(join_id %in% remaining_secondary_frame$A_11_enum_area) |>
  filter(grepl("S_",label))  |>
  select(c("name","label","ADM2_EN","join_id" )) |>
  filter(!name %in% c("SO2401_UR_001_NE_020_S_708",
                     "SO2401_UR_001_NE_020_S_699",
                     "SO2401_UR_001_NE_020_S_513"))



# point_to collec ---------------------------------------------------------


remaing_secondary_to_collect<- list()

for(i in unique(remaining_secondary_frame$A_11_enum_area)) {
  sample_point_needed <-  remaining_secondary_frame |> filter(A_11_enum_area==i) |> pull(new_secondary_points)
  Neigbourhood.Name <-  remaining_secondary_frame |> filter(A_11_enum_area==i) |> pull(neighbourhood)
  A_11_enum_area <-  remaining_secondary_frame |> filter(A_11_enum_area==i) |> pull(A_11_enum_area)
  City_Name <-  remaining_secondary_frame |> filter(A_11_enum_area==i) |> pull(city)

  sample_frame <- secondary_sample |> filter(join_id == i)
  remaing_secondary_to_collect[[i]] <- sample_frame[sample(nrow(sample_frame), sample_point_needed), ] |> mutate(
    Neigbourhood.Name = Neigbourhood.Name,
    A_11_enum_area = A_11_enum_area,
    City_Name =City_Name
  )


}

remaing_secondary_to_collect_df <- bind_rows(remaing_secondary_to_collect)


# joining primary and secondary needed_point -------------------------------------------

remaining_point <-
  remaing_primary_to_collect_df |> bind_rows(remaing_secondary_to_collect_df)



# overall sample table ----------------------------------------------------

remaining_point_by_ps

camp_point <- remaining_sample_from_from_new_df |> filter(location_type == "camp")


remaining_sample_number <- remaining_point_by_ps |>
  bind_rows(camp_point) |> arrange(city) |> mutate(
    camp_id = case_when(camp_id == "0" ~NA_character_,T~camp_id),
    camp_name = case_when(camp_name == "0" ~NA_character_,T~camp_name)
  )


write_list[["remaining_sample_number"]] <- remaining_sample_number
write_list[["remaining_point"]] <- remaining_point
write_list[["deleted_survey"]] <- deleted_survey


illuminate::write_formatted_excel(write_list,
                                  paste0("03_output/01_remaining_surveys/01_consolidate_file/",
                                         str_replace_all(today(),"-",""),"_remaining_statistics_v2.xlsx"))

