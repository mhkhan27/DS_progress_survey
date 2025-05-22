
# # read data ---------------------------------------------------------------
#
# # v1 ----------------------------------------------------------------------
#
# hh_v1<- read.csv("01_input/02_raw_data/01_version_1/hh_v1.csv",na.strings = "")
# indv_v1<- read.csv("01_input/02_raw_data/01_version_1/indv_v1.csv",na.strings = "")
#
#
# # v2 ----------------------------------------------------------------------
#
# hh_v2<- read.csv("01_input/02_raw_data/02_version_2/hh_data.csv",na.strings = "")
# indv_v2<- read.csv("01_input/02_raw_data/02_version_2/indv.csv",na.strings = "")
#
#
# # check hh col ------------------------------------------------------------
# names(hh_v1) %in% names(hh_v2) |> table()
# names(indv_v1) %in% names(indv_v2) |> table()
#
# names(hh_v2) %in% names(hh_v1) |> table()
# names(indv_v2) %in% names(indv_v1) |> table()
#
#
# # merging <-  -------------------------------------------------------------
#
# hh_data <- hh_v1 |> bind_rows(hh_v2) |> mutate(
#   date = start |>  as.Date()
# ) |> fix_data_type(character_cols = "Q_5_1_other")
# indv_data <- indv_v1 |> bind_rows(indv_v2) |> rename(
#   X_uuid = X_submission__uuid
# )
#


# date_log ----------------------------------------------------------------

date_log_full <- hh_data |> select(date,X_uuid)

write.csv(date_log_full,paste0("05_date_log/",str_replace_all(Sys.Date(),"-","_"),"_date_log.csv"))


# hh_data_new <- hh_data |> filter(!X_uuid %in% date_log_previous$X_uuid)


# read tool ---------------------------------------------------------------

kobo_survey <- read.xlsx("01_input/tool/kobo_tool.xlsx","survey")
kobo_choices <- read.xlsx("01_input/tool/kobo_tool.xlsx","choices")


# basic statistics --------------------------------------------------------

number_completed_survey<- hh_data |> filter(B_1_consent) |> nrow()

number_of_completed_survey_by_locaation <- hh_data |> filter(B_1_consent) |>
  group_by(A_7_sampletype,A_6_neighbourhood) |>
  summarise(number_of_completed_survey = n())

### Average number of survey by enumerator [[last two days]]
number_of_completed_survey_by_enum <- hh_data |> filter(B_1_consent) |>
  group_by(A_2_enumerator,date) |>
  summarise(number_of_completed_survey = n())

###  Number of survey by date [[last two days]]

number_of_completed_survey_by_date <- hh_data |> filter(B_1_consent) |>
  group_by(date) |>
  summarise(number_of_completed_survey = n()) |> ungroup() |>
  mutate(cum_sum = cumsum(number_of_completed_survey))

completed_survey_plot_date <- plot_ly(number_of_completed_survey_by_date,
                                      x = ~date, y = ~number_of_completed_survey,
                                      type = 'scatter', mode = 'lines+markers') |>
  layout(title = "Number of competed survey (by date)",
         xaxis = list(title = "Date"),
         yaxis = list (title = "Number of completed survey"))

# completed_survey_plot_date

completed_survey_plot_date_cumsum <- plot_ly(number_of_completed_survey_by_date,
                                      x = ~date, y = ~cum_sum, textposition = 'outside',
                                      text = ~number_of_completed_survey,
                                      hovertemplate =  paste('Number of survey: %{text}' ,
                                                             '<br>Cumulative number of survey: %{y}</br>',
                                                             '<extra></extra>'),

                                      type = 'scatter', mode = 'lines+markers') |>
  add_text(text = ~cum_sum, textposition = "top", showlegend = F) |>
  layout(title = "Number of competed survey (Cumulative) ",
         showlegend = FALSE,
         xaxis = list(title = "Date"),
         yaxis = list (title = "Number of completed survey"),
         legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.3))


# completed_survey_plot_date_cumsum



# duration statistics -----------------------------------------------------

# audits_list <- create_audit_list(audit_zip_path = audit_path)
# duration <- hh_data |> filter(B_1_consent) |>
#   add_duration_from_audit(audit_list = audits_list,
#                           start_question = "B_informed_consent/B_1_consent",
#                           end_question = "X_Closing/note_X_closing",
#                           uuid_column = "X_uuid" ,
#   )

duration <- duration_v2 |>left_join(hh_data)

average_survey_duration<- duration$duration_audit_start_end_minutes |> mean(na.rm= T)
median_survey_duration<- duration$duration_audit_start_end_minutes |> median(na.rm= T)


###### average survey duration by enumerator

enu_dura <- duration |> group_by(A_2_enumerator) |> summarise(
  average_duration = mean(duration_audit_start_end_minutes,na.rm = T),
  median_duration = median(duration_audit_start_end_minutes,na.rm = T),
  average_hh_size = round(mean(C_1_HH_size,na.rm = T),0),
  number_of_completed_survey = n()
) |> arrange(median_duration)

###### average survey duration by city

duration_by_city <- duration |> group_by(A_5_city) |> summarise(
  average_duration = mean(duration_audit_start_end_minutes,na.rm = T),
  median_duration = median(duration_audit_start_end_minutes,na.rm = T),
  average_hh_size = round(mean(C_1_HH_size,na.rm = T),0),
  number_of_completed_survey = n()
) |> arrange(median_duration)





## cleaning:: duration:: survey completed less than median - 10 min og greater than 25 min
duration_flaged <- duration |> filter(duration_audit_start_end_minutes < 15 |
                                        duration_audit_start_end_minutes > 60) |>
  select(X_uuid,old_value=duration_audit_start_end_minutes) |> mutate(
    issue= case_when(old_value <15 ~ "Survey completed in less than 15 min",
                     T~"Survey completed in grater than 60 min")
  )



# location check  ---------------------------------------------------------

hh_data_yes <- hh_data |> filter(B_1_consent)

duplicate_gps <- hh_data_yes$A_12_pindrop[duplicated(hh_data_yes$A_12_pindrop)]

## duplicated
dis <- hh_data |> filter(A_12_pindrop %in% duplicate_gps) |>
  select(X_uuid,A_12_pindrop,A_2_enumerator) |> filter(
    !is.na(A_12_pindrop)
  ) |> arrange(A_12_pindrop) |> mutate(
    issue = "Duplicated GPS point"
  ) |> rename(old_value= A_12_pindrop) |> mutate(
    question = "A_12_pindrop"
  ) |> left_join((hh_data |> select(X_uuid,respondent_name))) #|>
  # mutate(old_value = paste0(old_value, " || ", respondent_name )) |>
  # select(-respondent_name)


duplicate_survey_by_enu <- dis |> group_by(A_2_enumerator) |> summarise(
  number_of_duplicated_survey=n()
) |> arrange(-number_of_duplicated_survey)


#
###### map


hh_sf <- hh_data |> filter(B_1_consent) |>
  select(A_12_pindrop,current_lat,distance_error_explain,current_long,X_uuid,distance_target_location)

hh_pt <- hh_sf |> st_as_sf(coords = c("current_long","current_lat"),crs = 4326)

hh_pt_distance_grater_20 <- hh_pt |>
  filter(distance_target_location >20)



hh_pt_all_good <- hh_pt |>
  filter(!X_uuid %in% dis$X_uuid) |>
  filter(!X_uuid %in%  hh_pt_distance_grater_20$X_uuid)

duplicated_pt <- hh_pt |>
  filter(X_uuid %in% dis$X_uuid)

map_location_issue <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addCircles(data = duplicated_pt, weight = 8,color = "red",
             label = duplicated_pt$A_12_pindrop,
             popup = duplicated_pt$A_12_pindrop,
             group = "Duplicated point") |>
  addCircles(data = hh_pt_distance_grater_20, weight = 8,color = "blue",
             label = hh_pt_distance_grater_20$A_12_pindrop,
             popup = hh_pt_distance_grater_20$A_12_pindrop,
             group = "Distance is grater than 20m") |>
  addCircles(data = hh_pt_all_good, weight = 5,color = "lightgreen",
             label = hh_pt_all_good$A_12_pindrop,
             popup = hh_pt_all_good$A_12_pindrop,
             group = "HH location") |>
  addLayersControl(
    overlayGroups =c("Duplicated point","Distance is grater than 20m","HH location"),
    options = leaflet::layersControlOptions(collapsed = FALSE,position = "topright")
    )




# distance_map ------------------------------------------------------------

iconSet <- awesomeIconList(

  Glyphicon = makeAwesomeIcon(icon = "plus-sign", library = "glyphicon",
                              iconColor = 'rgb(192, 255, 0)',
                              markerColor = 'darkpurple',
                              spin = TRUE,
                              squareMarker = FALSE)
)



targets <- read.csv("01_input/03_target_points/pindrops.csv")


hh_pt_distance_grater_20  <- hh_pt_distance_grater_20 |>
  filter(!X_uuid %in% date_log_previous$X_uuid) |>
  mutate(
  color = case_when(distance_error_explain == "device_error" ~ "brown",
                    distance_error_explain =="outside_interview" ~ "blue",
                    T ~ "red"),
  distance_nor = ((distance_target_location - min(distance_target_location,na.rm = T))/ (max(distance_target_location,na.rm = T)-min(distance_target_location,na.rm = T))*10)+1,
  popup = paste(A_12_pindrop, "<br>Distance_error: ",
                distance_error_explain, "<br> Distance: ", distance_target_location ))



targets_20_m<- targets |> filter(name %in% hh_pt_distance_grater_20$A_12_pindrop)
targets_20_m_sf <- targets_20_m |> st_as_sf(coords = c("longitude","latitude"),crs = 4326)



if(nrow(targets_20_m_sf) >0) {
distance_map <- leaflet(height = 800) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addAwesomeMarkers(data = targets_20_m_sf,
                    icon = iconSet,
                    group = "Target point",
                    label = targets_20_m_sf$name,
                    popup = targets_20_m_sf$name) |>
  addCircleMarkers(data = hh_pt_distance_grater_20,
                   weight = 5,radius = hh_pt_distance_grater_20$distance_nor,
                   fill = hh_pt_distance_grater_20$color,
                   group = "Surveyed point",
                   color = hh_pt_distance_grater_20$color,
                   label = ~A_12_pindrop,
                   popup = paste0(hh_pt_distance_grater_20$A_12_pindrop, "<br>Distance_error: ",
                                  hh_pt_distance_grater_20$distance_error_explain, "<br> Distance: ", hh_pt_distance_grater_20$distance_target_location )) |>
  addLayersControl(
    overlayGroups =c("Target point","Surveyed point"),
    options = leaflet::layersControlOptions(collapsed = FALSE,position = "topright"),
  ) %>% hideGroup("Target point")

}

if(nrow(targets_20_m_sf) ==0) {
  distance_map <- leaflet(height = 800) %>%
    addProviderTiles(providers$OpenStreetMap)
}


# distance  ---------------------------------------------------------------

hh_distance_cleaning <- hh_pt |>
  filter(distance_target_location >20 ) |> mutate(
    issue = paste0("Distance between target point and surveyed point is ", round(distance_target_location,0), " meter. Distance error explaination: ", distance_error_explain  )
  ) |> rename(old_value =distance_target_location) |>    select(X_uuid,old_value,issue) |> as.data.frame() |>
  select(-geometry)




#  outliers --------------------------------------------

outliers_hh_list <- hh_data |>
  check_duplicate(uuid_column = "X_uuid") |>
  check_outliers(uuid_column = "X_uuid",
                 kobo_survey = survey,
                 kobo_choices = choices,
                 columns_not_to_check = c("n_nodes", "distance", "distance_target_location", "repondent_phone",
                                          "C_1_HH_size", "male_0_4_count", "female_0_4_count", "male_5_11_count",
                                          "female_5_11_count", "male_12_17_count", "female_12_17_count",
                                          "male_5_17_count", "female_5_17_count", "male_adult_count", "female_adult_count",
                                          "total_male_count", "total_female_count", "D_3_times_disp", "E_3_male_5_17_attend_school",
                                          "E_4_female_5_17_attend_school", "E_5_hoh_read", "E_6_male_read",
                                          "E_7_female_read","first_lat","target_lat","current_lat","count_hhh"),
                 remove_choice_multiple = T,strongness_factor = 3
  )


outliers_indv_list<- indv_data |>
  check_outliers(uuid_column = "X_uuid",
                 kobo_survey = survey,
                 kobo_choices = choices,
                 columns_not_to_check = c("resp_hhh", "resp_hhh.hhh", "resp_hhh.respondent", "resp_hhh.member", "roster_count",
                                          "C_5_1_relation_to_HoH", "C_5_1_1_other", "C_7_HoH_marital",
                                          "respondent_calc", "hhh_calc", "male_0_4_calc", "female_0_4_calc",
                                          "male_5_11_calc", "female_5_11_calc", "male_12_17_calc", "female_12_17_calc",
                                          "male_5_17_calc", "female_5_17_calc", "male_adult_calc", "female_adult_calc",
                                          "total_male_calc", "total_female_calc", "X_index"),
                 remove_choice_multiple = T,strongness_factor = 3
  )


outliers_indv <- outliers_indv_list$potential_outliers |>  filter(old_value > 90 ) |> rename(X_uuid = uuid) |> filter(
  !(old_value == 99 | old_value == 88 |  old_value == 77 | old_value ==0)
)

outliers_hh <- outliers_hh_list$potential_outliers |> rename(X_uuid = uuid)|> filter(
  !(old_value == 99 | old_value == 88 |  old_value == 77 | old_value ==0)
)
duplicated_log <- outliers_hh_list$duplicate_log |> rename(X_uuid =uuid)



# other checks ------------------------------------------------------------

text_oth_hh <- kobo_survey |>
  dplyr::filter(type == "text", name %in% names(hh_data)) |>
  filter(!name %in% c("respondent_name","A_1_team_leader",
                      "X_1_comment","X_4_enum_comment")) |>
  dplyr::pull(name)

other_hh <- hh_data |>
  check_others(uuid_column = "X_uuid", columns_to_check = text_oth_hh)


text_oth_indv <- kobo_survey |>
  dplyr::filter(type == "text", name %in% names(indv_data)) |>
  filter(!name %in% c("respondent_name","A_1_team_leader",
                      "X_1_comment","X_4_enum_comment")) |>
  dplyr::pull(name)

other_indv <- indv_data |>
  check_others(uuid_column = "X_uuid", columns_to_check = text_oth_indv)









# shoraage path ----------------------------------------------------------
# Missing percentage ------------------------------------------------------


kobo_survey_filter <- kobo_survey |> filter(
  !is.na(relevant)
) |> filter(type !="note") |>
  filter(type !="begin_group")


question_name <- kobo_survey_filter$name

hh_data_rm <- hh_data |> select(-contains("."))
question_name[!question_name %in% names(hh_data_rm) ]


df <- hh_data_rm |> filter(B_1_consent)|> mutate(
  na_sum = rowSums(dplyr::across(.cols = dplyr::all_of(names(hh_data_rm)),
                                 .fns = is.na)),
  number_of_relevant = 121
) |>
  mutate(
    pct_na = na_sum/number_of_relevant
  )


missing_pct <- add_percentage_missing(df,kobo_survey = kobo_survey)  |>
  select(X_uuid,A_2_enumerator,na_sum,number_of_relevant,pct_na,percentage_missing)




##### dont know


kobo_only_type <- kobo_survey |> select(type,name) |> mutate(
  type_only = word(type,1),
  list_name = word(type,2)
)



## select_one ## total number of column 54

## select multipl


multiple_cols <- hh_data |> select(ends_with(".99")) |> names()

kobo_select_one <- kobo_only_type |> filter(type_only %in% c("select_one"))

choice_select_one <- kobo_choices |> filter(list_name %in% kobo_select_one$list_name) |>
  filter(name == "99")

kobo_select_one_final_col <-kobo_select_one |> filter(list_name %in% choice_select_one$list_name) |> pull(name)
kobo_select_one_final_col_hh <- kobo_select_one_final_col[kobo_select_one_final_col %in% names(hh_data)]


### missing and

hh_data_consent_yes <- hh_data |> filter(B_1_consent)

missing_dont_know_rowsum_all <- hh_data_consent_yes |> dplyr::mutate_if(is.character, factor) |> mutate(
  select_one_dont_rosum = rowSums(hh_data_consent_yes[kobo_select_one_final_col_hh] == "99",na.rm = T),
  select_multiple_dont_rosum = rowSums(hh_data_consent_yes[multiple_cols],na.rm = T)
) |> left_join(missing_pct) |>
  select(X_uuid,A_2_enumerator,na_sum,number_of_relevant,pct_na,percentage_missing,select_one_dont_rosum,
         select_multiple_dont_rosum) |> mutate(
    tota_dont_know = select_multiple_dont_rosum +select_one_dont_rosum
  )


  missing_dont_know_rowsum <-  missing_dont_know_rowsum_all |> filter(percentage_missing >.28 | tota_dont_know>6) |>
  select(-select_one_dont_rosum,-select_multiple_dont_rosum)


enumerator_issue_soft_dublicate <- missing_dont_know_rowsum |> group_by(A_2_enumerator) |> summarise(
  number_of_flagged_surveys =n(),
  count_more_than_6_dont_know= sum(tota_dont_know>6,na.rm = T),
  count_more_than_28pct_missing = sum(percentage_missing>.28,na.rm = T)
) |> arrange(-number_of_flagged_surveys)


number_of_survey_by_enu <-hh_data |> filter(B_1_consent) |> group_by(A_2_enumerator) |> summarise(
  number_of_total_survey = n()
  # average_hh_size = round(mean(C_1_HH_size,na.rm=T),0)
)

enumerator_issue_soft_dublicate <- enumerator_issue_soft_dublicate |> left_join(number_of_survey_by_enu)

########## average Na #################

average_na_by_enu <- missing_dont_know_rowsum_all |> group_by(A_2_enumerator) |>
  summarise(
  avg_number_of_na_response = mean(na_sum,na.rm = T),
  number_of_relevancy = mean(number_of_relevant,na.rm = T),
  percentage =paste0(round(avg_number_of_na_response/number_of_relevancy*100), "%")
  ) |> arrange(-avg_number_of_na_response)






# logical check 1: displacement status ------------------------------------

logical_check_displacement <- hh_data |> filter(B_1_consent)  |>
  filter(D_1_origin_yesno & D_2_disp_status %in% c("internally_displaced_person","refugee") |
           !D_1_origin_yesno & D_2_disp_status == "host_community_resident") |>
  mutate(
    issue = case_when(D_1_origin_yesno &D_2_disp_status %in% c("internally_displaced_person","refugee") ~ "Original place is reported YES but IDP/refugee is reported in displacement status.",
                      !D_1_origin_yesno & D_2_disp_status == "host_community_resident" ~"Original place is reported No but Host community resident is reported in displacement status.")
  ) |> mutate(
    D_1_origin_yesno = case_when(D_1_origin_yesno ~ "yes",T~"no"),
    question_to_update = "D_1_origin_yesno, D_2_disp_status",
    available_options = paste0("yes, no, ||| host_community_resident, returnee_from_a_place_within_somalia_internal_displacement,returnee_from_abroad, internally_displaced_person,refugee"),
    old_value = paste0(D_1_origin_yesno , " ||| ", D_2_disp_status)
  )  |> select(X_uuid,question_to_update,old_value,available_options,issue)



####### HH latrine


child_number <- indv_data |> group_by(X_uuid) |> summarise(
  number_of_child = sum(age_years <5,na.rm = T)
)

latrine_check <- hh_data |> filter(B_1_consent)  |>
  filter(K_7_latrin_type == "hh_latrins" & (K_8_ind_share_latrin < C_1_HH_size)) |>
  left_join(child_number) |>
  filter(!((K_8_ind_share_latrin +number_of_child) == C_1_HH_size )) |>
  mutate(issue = paste0("hh_latrins was selected as latrins type however number of pleople sharing the latrine ["
                        ,K_8_ind_share_latrin ,"] is less number of HH size [",C_1_HH_size,"]"),
         question_to_update = "K_7_latrin_type, K_8_ind_share_latrin",
         available_options = "hh_latrins, communal, open_defection, 88",
         old_value= paste0("HH_size :",C_1_HH_size,
                           "||| K_8_ind_share_latrin: ",K_8_ind_share_latrin,
                           "||| Number of child: ",number_of_child),
         question = "K_8_ind_share_latrin"

  ) |>
  select(X_uuid,question,question_to_update,old_value,available_options,issue)






# Enumerator multiple organization ----------------------------------------

hh_data |> filter(B_1_consent) |>
  group_by(A_2_enumerator) |> summarise(
    unique_organization = length(unique(A_2_2_org_name))
  )



differetn_ngo <- hh_data |> filter(B_1_consent) |>
  select(A_2_2_org_name,A_2_enumerator) |>
  unique() |>
  pivot_wider(id_cols = A_2_enumerator,values_from = "A_2_2_org_name",
              names_from = "A_2_2_org_name",values_fn = ~1, values_fill = 0)

enu_reporting_multiple_ngo <- differetn_ngo|>
  mutate(number_of_different_ngo_reported = rowSums(
    differetn_ngo[names(differetn_ngo)[!names(differetn_ngo) %in% "A_2_enumerator"]],na.rm =T)
    ) |> filter(number_of_different_ngo_reported >1) |>
  arrange(-number_of_different_ngo_reported)




# Preparing cleaning log --------------------------------------------------

duration_flaged_all_chara <- duration_flaged |> mutate_all(as.character)
dis_all_chara <- dis |> mutate_all(as.character)
outliers_indv_all_chara <- outliers_indv |> mutate_all(as.character)
outliers_hh_all_chara <- outliers_hh |> mutate_all(as.character)
duplicated_log_all_chara <- duplicated_log |> mutate_all(as.character)
hh_distance_cleaning_all_chara <- hh_distance_cleaning |> mutate_all(as.character)



cleaning_log_full <- dis_all_chara |>
  # bind_rows(duration_flaged_all_chara) |>
  bind_rows(outliers_indv_all_chara) |>
  bind_rows(outliers_hh_all_chara) |>
  bind_rows(duplicated_log_all_chara) |>
  bind_rows(hh_distance_cleaning_all_chara) |>
  bind_rows(latrine_check) #|>
  # bind_rows(logical_check_displacement)


cleaning_log_full <- cleaning_log_full |> select(-A_2_enumerator) |>
  left_join(hh_data |> select(date,X_uuid,A_5_city,A_2_enumerator ),multiple = "first") |>
  mutate(new_value = NA_character_,
         action = NA_character_,
         TL_comments=NA_character_ ) |>
  filter(
    !(old_value == "99" | old_value == "88" |  old_value == "77")
  ) |> filter(!X_uuid %in% date_log_previous$X_uuid) |>
  filter(!grepl("_cal",question)) |>
  select(date,X_uuid,A_5_city,A_2_enumerator,question,old_value,new_value,action,issue,question_to_update,available_options,TL_comments,respondent_name)



#######################. added previous duplication ##########

cleaning_log_full_duplicated <- cleaning_log_full |> filter(question == "A_12_pindrop")  |>
  filter(!X_uuid %in% rm_survey_df$X_uuid)|> pull(old_value)


added_previous_duplication <- dis_all_chara |>
  filter(old_value %in% cleaning_log_full_duplicated) |>
  filter(!X_uuid %in% rm_survey_df$X_uuid) |>
  left_join(hh_data |> filter(B_1_consent)|> select(date,A_5_city,X_uuid)) |>
  mutate(old_value = paste0(old_value, " || ", respondent_name))


# hh_data  |> mutate(
#   old_value = paste0(A_12_pindrop, " || " ,respondent_name)
# ) |>
#  filter(old_value %in% added_previous_duplication$old_value) |> select(X_uuid,old_value)




cleaning_log_full <- cleaning_log_full |> filter(question != "A_12_pindrop")  |>
  bind_rows(added_previous_duplication) |> select(-respondent_name)

############

file_folder <- paste0("03_output/cl_",today())
if(dir.exists(file_folder)){unlink(file_folder,recursive = T)}
if(!dir.exists(file_folder)){dir.create(file_folder)}

write_cleaning_log(cleaning_log =cleaning_log_full,file_path =paste0(file_folder,"/cleaning_log_full_",str_replace_all(Sys.Date(),"-","_"),".xlsx") )


for(i in unique(cleaning_log_full$A_5_city)) {
  cleaning_log_filter <- cleaning_log_full |> filter(A_5_city ==i)
  write_cleaning_log(cleaning_log =cleaning_log_filter,file_path = paste0(file_folder,"/",i,"_",str_replace_all(Sys.Date(),"-",""),".xlsx") )

}


# OVERALL Enumerator performance ------------------------------------------



### duration

duration_issue <- enu_dura |> filter(median_duration < 15 | median_duration> 70) |> pull(A_2_enumerator)
### duplucate
duplicate_point_name <- duplicate_survey_by_enu |> filter(number_of_duplicated_survey>2) |> pull(A_2_enumerator)
### distance

distance_name <- hh_data |>  filter(distance_target_location >20) |>
  select(A_2_enumerator) |>
  group_by(A_2_enumerator) |>
  summarise(
    number_of_survey= n()) |>
  filter(number_of_survey >10) |> pull(A_2_enumerator)


shortage_path_name <- enumerator_issue_soft_dublicate  |>
  filter(number_of_flagged_surveys > 10) |>
  pull(A_2_enumerator)



#### data set
enem_performance <- data.frame(A_2_enumerator = hh_data$A_2_enumerator |> unique())

enem_performance <- enem_performance |> mutate(
  city = gsub('[0-9]+', '', A_2_enumerator),
  duration= A_2_enumerator %in% duration_issue,
  duplicate = A_2_enumerator%in% duplicate_point_name,
  distance = A_2_enumerator%in% distance_name,
  shortage_path = A_2_enumerator%in% shortage_path_name
) |> mutate(
  summary = duration+duplicate+distance+shortage_path
) |> select(city,A_2_enumerator,summary,everything()) |>
  arrange(-summary)
