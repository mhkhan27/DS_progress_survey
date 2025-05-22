library(tidyr)
library(tidyverse)





file_list_all <- list.files(path = "03_output/03_filled_cleaning_log/",all.files = T,include.dirs = T,recursive = T,full.names = T,pattern = "xlsx")

# file_list_1 <- file_list_all[!grepl("cleaning_log_full_",file_list_all)]
# file_list <- file_list_1[!grepl("/~",file_list_1)]
file_list <- file_list_all

read_list <- list()
for(i in file_list ) {
  read_list[[i]] <- openxlsx::read.xlsx(i) |> mutate_all(as.character)
}


all_cl <- bind_rows(read_list) |> select(X_uuid,action,new_value,question)






# deletion ----------------------------------------------------------------

# v1_duration -------------------------------------------------------------
audits_list_v1 <- create_audit_list(audit_zip_path = "01_input/01_audit/V1_audit/a8wouhpyJNgiLxLS2Rv5tH_2024_05_14_11_25_51.zip")

duration_v1 <- hh_v1 |> filter(B_1_consent==1) |>
  add_duration_from_audit(audit_list = audits_list_v1,
                          start_question = "B_informed_consent/B_1_consent",
                          end_question = "X_Closing/note_X_closing",
                          uuid_column = "X_uuid" ,
  ) |> select(X_uuid,duration_audit_start_end_minutes)


# v2_duration -------------------------------------------------------------
audits_list <- create_audit_list("01_input/01_audit/aB6h5dgnuwEVzP3Kgch2i4_2024_05_22_03_30_37.zip")

duration_v2 <- hh_v2 |> filter(B_1_consent==1) |>
  add_duration_from_audit(audit_list = audits_list,
                          start_question = "B_informed_consent/B_1_consent",
                          end_question = "X_Closing/note_X_closing",
                          uuid_column = "X_uuid" ,
  )|> select(X_uuid,duration_audit_start_end_minutes)



# duration_join -----------------------------------------------------------

duration_df <- duration_v1 |> bind_rows(duration_v2) |> filter(
  duration_audit_start_end_minutes < 10
)  |>
  filter(is.na(duration_audit_start_end_minutes) | duration_audit_start_end_minutes >0)

all_dl <- data.frame(
  X_uuid = duration_df$X_uuid,
  action = "remove_survey"
)



total_cl <- all_cl |> bind_rows(all_dl) |> filter(is.na(question) | question != "age_years")

total_cl$action |> table()


clean_data <- cleaningtools::create_clean_data(raw_dataset = hh_data,
                                               raw_data_uuid_column = "X_uuid",
                                               cleaning_log = total_cl,
                                               cleaning_log_uuid_column = "X_uuid",
                                               cleaning_log_question_column = "question",
                                               cleaning_log_new_value_column =  "new_value",
                                               cleaning_log_change_type_column = "action",
                                               change_response_value = "change_value",
                                               NA_response_value = "blank_response",
                                               no_change_value =  "no_action",
                                               remove_survey_value = "remove_survey" )

clean_data <- clean_data |> fix_data_type() |> filter(B_1_consent)








####### clean ######







df <- clean_data |> select(starts_with("G_4_"),A_5_city)






df_box <- df |> pivot_longer(!"A_5_city") |> filter(
  !grepl("_cal",name)
)


box_plot <- list()
for(i in unique(df_box$name )) {
  df_box_i <- df_box |> filter(name ==i)
  box_plot[[i]] <- ggplot(data=df_box_i, mapping=aes(x=name, y=value))+geom_boxplot() +
  facet_wrap(. ~ A_5_city,scales = "free")+theme_bw()+
    ggtitle(i)
  ggsave(filename  = paste0("03_output/expenditure_overview/",i,".jpg"),dpi = 300)

}

box_plot$G_4_2_spend_food

