rm(list = ls())

library(openxlsx)
library(tidyverse)
library(srvyr)
library(illuminate)
library(analysistools)
library(MESS)

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

hh_data <- hh_data |> mutate_if(is.logical, function(x){x*1})



# IDP ---------------------------------------------------------------------

hh_data_idp <- hh_data  |> filter(pop_group != "hc")


# analysis ----------------------------------------------------------------
hh_data_idp$D_1_5_reasondisp |> unique()
hh_data_idp$D_5_preferred_loc |> unique()

df <- hh_data_idp |>
  filter(!(is.na(D_1_5_reasondisp)|D_1_5_reasondisp == "88" ),
         D_5_preferred_loc != "99") |> mutate(
           D_1_5_reasondisp = case_when(D_1_5_reasondisp == "conflict_security_situation" ~ "Conflict",
                                     D_1_5_reasondisp %in% c("flooding_excessive_rain",
                                                          "drought_lack_of_rain")~ "Climate related events",
                                     D_1_5_reasondisp %in% c("eviction_from_land",
                                                          "eviction_from_house",
                                                          "fear_of_persecution",
                                                          "discrimination_by_the_community") ~ "Fear, evition and discrimination",
                                     D_1_5_reasondisp %in% c("lack_of_basic_services","lack_of_shelter") ~ "Lack of shelter and basic services",
                                     D_1_5_reasondisp %in% c("poor_economic_conditions","not_able_to_pay_rent") ~ "Lack of livelihood Oppourtunity",
                                     T~D_1_5_reasondisp

           ),
           D_5_preferred_loc = case_when(D_5_preferred_loc == "this_location" ~ "Current location",
                                         D_5_preferred_loc == "place_of_origin" ~ "Place of origin",
                                         D_5_preferred_loc == "another_location_within_somalia" ~ "Another location in Somalia",
                                         D_5_preferred_loc == "another_location_outside_somalia" ~ "Abroad",
                                         T~D_5_preferred_loc)
         ) |>
 filter(D_1_5_reasondisp %in% c("Conflict","Climate related events") )

df$D_1_5_reasondisp |> unique()
df$D_5_preferred_loc |> unique()


# df_svy <- as_survey(df |>
#                       select(City_Name,strata,survey_weights,D_5_preferred_loc,D_1_5_reasondisp),
#                     strata = strata ,weights = survey_weights)
#
df <- df |> illuminate::fix_data_type()

output <- list()
output[["df_analysis_all_dis"]] <- illuminate::survey_analysis(df = df,
                                        weights = T,weight_column = "survey_weights",
                                        strata = strata,
                                        disag = c("City_Name","D_1_5_reasondisp"),
                                        vars_to_analyze  = "D_5_preferred_loc"
) |>  select(location_preference = choice,
             reason_of_dis = subset_2_val,
         city_name = subset_1_val,
         stat)



# OVERALL -----------------------------------------------------------------
output[["df_analysis_OVERALL"]] <- illuminate::survey_analysis(df = df,
                                           weights = T,
                                           weight_column = "survey_weights",
                                           strata = strata,
                                           vars_to_analyze  = "D_5_preferred_loc"
) |>  select(location_preference = choice,
             stat) |>
  mutate(city_name = "Across",
         reason_of_dis = "All",
  )

# BY CITY -----------------------------------------------------------------
output[["df_analysis_City"]] <- illuminate::survey_analysis(df = df,
                                                   weights = T,
                                                   weight_column = "survey_weights",
                                                   strata = strata,
                                                   disag = "City_Name",
                                                   vars_to_analyze  = "D_5_preferred_loc"
) |>  select(location_preference = choice,
             city_name = subset_1_val,
             stat) |>
  mutate(reason_of_dis = "All")


# REASON OF displacement --------------------------------------------------


output[["df_analysis_reason"]] <- illuminate::survey_analysis(df = df,
                                                weights = T,
                                                weight_column = "survey_weights",
                                                strata = strata,
                                                disag = "D_1_5_reasondisp",
                                                vars_to_analyze  = "D_5_preferred_loc"
) |>  select(location_preference = choice,
             reason_of_dis = subset_1_val,
             stat) |>
  mutate(city_name = "Across")





# bind --------------------------------------------------------------------

df_analysis <- bind_rows(output)



data <- df_analysis |> group_by(city_name,reason_of_dis) |>
  mutate(
  stat_round = MESS::round_percent(stat*100)
  ) |> ungroup() |>
  mutate(
    stat_round_lab = case_when(stat_round <2 ~ NA_character_,
                               T~paste0(stat_round,"%"))
  )

data_e <- data |> select(-stat_round_lab,-stat)

openxlsx::write.xlsx(data_e,"04_script/06_reason_of_dis_and_settlem/reason_of_displacement_df.xlsx")


# Apply the function per group
# data <- df_analysis

ggplot(data, aes(x = stat_round,
                 y = reason_of_dis,
                 fill = location_preference)) +
  geom_bar(stat = "identity") +
  facet_wrap(~city_name,ncol=2) +   # This separates each reason into its own panel (optional)
  labs(
    title = "",
    x = "",
    y = "Reason of displacement",
    fill = "Location Preference"
  ) +
  geom_text(aes(label = stat_round_lab),
            position = position_stack(vjust = 0.5),
            size = 2, color = "white")+
  scale_fill_manual(values = c(
    "Current location" = "#002D72",       # IOM Blue
    "Place of origin" = "#0072BC",        # Light Blue
    "Another location in Somalia" = "#5B6770", # Dark Gray
    "Abroad" = "darkred"                  # Gray (if "Abroad" is used)
  ))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom",       # Moves legend below the plot
    legend.direction = "horizontal",  # Arranges items horizontally
    legend.title = element_text(face = "bold")  # Optional: makes the title bold
  )



# who prefered to live abraod ---------------------------------------------

df_analysis_2 <- illuminate::survey_analysis(df = df,
                                           weights = T,
                                           weight_column = "survey_weights",
                                           strata = strata,
                                           disag = c("D_1_5_reasondisp"),
                                           vars_to_analyze  = "D_5_preferred_loc"
) |>  select(location_preference = choice,
             reason_of_dis = subset_1_val,
             stat)

data2 <- df_analysis_2 |> group_by(reason_of_dis) |>
  mutate(
    stat_round = MESS::round_percent(stat*100)
  ) |> ungroup() |>
  mutate(
    stat_round_lab = case_when(stat_round <2 ~ NA_character_,
                               T~paste0(stat_round,"%"))
  )

ggplot(data2, aes(x = stat_round,
                 y = reason_of_dis,
                 fill = location_preference
                 )) +
  geom_bar(stat = "identity") +
  # facet_wrap(~city_name,ncol=2) +   # This separates each reason into its own panel (optional)
  labs(
    title = "",
    x = "",
    y = "Reason of displacement",
    fill = "Location Preference"
  ) +
  geom_text(aes(label = stat_round_lab),
            position = position_stack(vjust = 0.5),
            size = 2, color = "white")+
  scale_fill_manual(values = c(
    "Current location" = "#002D72",       # IOM Blue
    "Place of origin" = "#0072BC",        # Light Blue
    "Another location in Somalia" = "#5B6770", # Dark Gray
    "Abroad" = "darkred"                  # Gray (if "Abroad" is used)
  ))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "bottom",       # Moves legend below the plot
        legend.direction = "horizontal",  # Arranges items horizontally
        legend.title = element_text(face = "bold")  # Optional: makes the title bold
  )




