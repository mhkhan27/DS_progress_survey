rm(list = ls())

library(tidyverse)
library(openxlsx)
options(scipen = 999)


# kobo\ -------------------------------------------------------------------


sample_idp <- openxlsx::read.xlsx("02_dap/LORA_Sample_frame_v2.xlsx",2) |> select(admin_name=strata,City=ADM2_EN) |> unique()



sample_idp <- sample_idp |> mutate(
  admin_name = case_when( City == "Baydhaba" & admin_name== "Horseed_CA11" ~ "Horseed_CA11",

                                   City == "Baydhaba" & grepl("Adaado_CA",admin_name) ~ "Adaado_b",

                                   City == "Baydhaba" & grepl("Durasalam_CA",admin_name) ~ "Durasalam_b",

                                   City == "Baydhaba" &
                                     grepl("Horseed_CA",admin_name) ~ "Horseed_b",

                                   City == "Baydhaba" &
                                     grepl("Howl Wadaag_CA",admin_name) ~ "Howl Wadaag_b",

                                   City == "Baydhaba" &
                                     grepl("Isha_CA",admin_name) ~ "Isha_b",

                                   City == "Baydhaba" &
                                     grepl("Salamey_CA",admin_name) ~ "Salamey_b",


                                   City == "Baydhaba" &
                                     grepl("Towfiq_CA",admin_name) ~ "Towfiq_b",

                                   City == "Baydhaba" &
                                     grepl("Wadajiir_CA",admin_name) ~ "Wadajiir_b",

                                   City == "Baydhaba" &
                                     grepl("Weberi_CA",admin_name) ~ "Weberi_b",

                                   T~admin_name



  )
) |> distinct()


survey <- openxlsx::read.xlsx("01_input/tool/kobo_tool.xlsx",sheet = 1) |>
  select(type,name,label =`label::english`) |>
  filter(grepl("select_|integer",type)) |>
  mutate(
    list_name = str_replace_all(type,"select_multiple |select_one ","")
  )



choice <- openxlsx::read.xlsx("01_input/tool/kobo_tool.xlsx",sheet = 2) |>
  select(list_name,name,label = `label::english`) |>
  left_join(survey |>
              select(list_name,q_name=name) |> distinct()) |>
  filter(!is.na(q_name)) |> mutate(
    choice_join_id = paste0(q_name,".",name)
  )




not_to_be_analyzed <- c("G_4_totalspend_week_cal",  "G_4_totalspend_month_cal",
                        "A_7_sampletype", "idp_selection","City_Name", "strata", "survey_weights",
                        "gender_hoh","disabily_group","roster_count")


n_analysis <- read.csv("03_output/05_analysis/hh_analysis.csv") |>
  mutate( type ="hh")  |> bind_rows(
    read.csv("03_output/05_analysis/indv_analysis.csv") |>
      mutate(type ="indv")) |>
  filter(analysis_type != "median") |>
  filter(!is.na(stat)) |>
  mutate(choice_join_id =  paste0(analysis_var,".",analysis_var_value)) |>
  filter(!analysis_var %in% not_to_be_analyzed)


n_analysis_with_questin <- n_analysis |> left_join(survey |> select(name,question =label),
                                                   by = c("analysis_var"="name")) |>
  left_join(choice |> select(choice_join_id,choice = label))  |> mutate(
    choice = case_when(analysis_type == "mean" ~ question,
                       is.na(choice) ~ analysis_var_value,
                       T~choice ),
    question = case_when(is.na(question) ~ analysis_var , T~ question)

  )



n_analysis <- n_analysis_with_questin |>
  select(type,pop_group, group_var ,group_var_value,analysis_type,
         analysis_var =question,
         analysis_var_value=choice,
         stat, n_w,n_w_total, n, n_total

  )




# pct_table ---------------------------------------------------------------

n_analysis <- n_analysis |> select(
  c("type","pop_group", "group_var", "group_var_value", "analysis_var",
    "analysis_var_value", "stat")
) |> mutate(
  stat = round(stat,4)
)



# hh_indicators -----------------------------------------------------------
n_analysis_hh <- n_analysis |> filter(type == "hh")

analysis_list <- list()

for(i in unique(n_analysis_hh$pop_group)){
  analysis_list[[i]] <- n_analysis_hh |> filter(pop_group == i) |>
    tidyr::pivot_wider(id_cols = c("pop_group", "group_var", "group_var_value"),
                       names_from = c(analysis_var,analysis_var_value),
                       values_from = stat,names_sep =  "::")
}


pi_wider <-  bind_rows(analysis_list)



col_name_first <- data.frame(col_name = names(pi_wider)) |> mutate(
  first_header = gsub( "::.*$", "",col_name)
  # second_header =  sub(".*::", "", col_name)
) |> pivot_wider(names_from = col_name,values_from = first_header)

col_name_second <- data.frame(col_name = names(pi_wider)) |> mutate(
  # first_header = gsub( "::.*$", "",col_name)
  second_header =  sub(".*::", "", col_name)
) |> pivot_wider(names_from = col_name,values_from = second_header)

names(col_name_first) %in% names(pi_wider) |> table()
names(col_name_second) %in% names(pi_wider) |> table()



df_names <- col_name_first  |>
  bind_rows(col_name_second)  |> rbind(pi_wider)



df_names_2 <- df_names |> mutate(
  analysis_level= case_when(group_var == "strata" & pop_group == "IDP" ~ "Neighbourhood",
                            group_var == "strata" & pop_group == "HC" ~ "City",
                            group_var %in% c ("City_Name","disabily_group" )~ "City",
                            T~group_var),
  admin_name = case_when(grepl("idp-",group_var_value) ~ str_replace_all(group_var_value,"idp-",""),
                         grepl("hc-",group_var_value) ~ str_replace_all(group_var_value,"hc-",""),
                         grepl("::",group_var_value) ~ gsub( "::.*$", "",group_var_value),
                         T~group_var_value)) |>
  left_join(sample_idp) |>

  mutate(City = case_when(is.na(City) ~admin_name ,T~City)) |> mutate(
    City = case_when(City == "group_var_value" ~ "City", T~City ),
    admin_name = case_when(admin_name == "group_var_value" ~ "admin_name", T~admin_name ),
    analysis_level = case_when(analysis_level == "group_var" ~ "analysis_level", T~analysis_level ),
    group_var_value = case_when(grepl("::",group_var_value) ~ sub(".*::", "", group_var_value),
                                T~group_var_value)
  )

hh_table <- df_names_2[,order(colnames(df_names_2))] |>
  select(City,pop_group,analysis_level,admin_name,group_var,group_var_value,everything()) |> select(-group_var)


hh_table$`pop_group::hc`  <- hh_table$`pop_group::hc`  |> as.character()
hh_table$`pop_group::idp`  <- hh_table$`pop_group::idp`  |> as.character()


# indv indicators -----------------------------------------------------------
n_analysis_indv <- n_analysis |> filter(type == "indv") |>
  filter(!analysis_var %in% c("C_5_1_1_other","gender_hoh","pop_group",
                              "disabily_group","survey_weights",
                              "City_Name","disabily_group","strata"))

n_analysis_indv$analysis_var |> unique()

analysis_list_indv <- list()

for(i in unique(n_analysis_indv$pop_group)){
  analysis_list_indv[[i]] <- n_analysis_indv |> filter(pop_group == i) |>
    tidyr::pivot_wider(id_cols = c("pop_group", "group_var", "group_var_value"),
                       names_from = c(analysis_var,analysis_var_value),
                       values_from = stat,names_sep =  "::")
}


pi_wider <-  bind_rows(analysis_list_indv)



col_name_first <- data.frame(col_name = names(pi_wider)) |> mutate(
  first_header = gsub( "::.*$", "",col_name)
  # second_header =  sub(".*::", "", col_name)
) |> pivot_wider(names_from = col_name,values_from = first_header)

col_name_second <- data.frame(col_name = names(pi_wider)) |> mutate(
  # first_header = gsub( "::.*$", "",col_name)
  second_header =  sub(".*::", "", col_name)
) |> pivot_wider(names_from = col_name,values_from = second_header)

names(col_name_first) %in% names(pi_wider) |> table()
names(col_name_second) %in% names(pi_wider) |> table()



df_names <- col_name_first  |>
  bind_rows(col_name_second)  |> rbind(pi_wider)



df_names_2 <- df_names |> mutate(
  analysis_level= case_when(group_var == "strata" & pop_group == "IDP" ~ "Neighbourhood",
                            group_var == "strata" & pop_group == "HC" ~ "City",
                            group_var %in% c ("City_Name","disabily_group" )~ "City",
                            T~group_var),
  admin_name = case_when(grepl("idp-",group_var_value) ~ str_replace_all(group_var_value,"idp-",""),
                         grepl("hc-",group_var_value) ~ str_replace_all(group_var_value,"hc-",""),
                         grepl("::",group_var_value) ~ gsub( "::.*$", "",group_var_value),
                         T~group_var_value)) |>
  left_join(sample_idp) |>

  mutate(City = case_when(is.na(City) ~admin_name ,T~City)) |> mutate(
    City = case_when(City == "group_var_value" ~ "City", T~City ),
    admin_name = case_when(admin_name == "group_var_value" ~ "admin_name", T~admin_name ),
    analysis_level = case_when(analysis_level == "group_var" ~ "analysis_level", T~analysis_level ),
    group_var_value = case_when(grepl("::",group_var_value) ~ sub(".*::", "", group_var_value),
                                T~group_var_value)
  )

indv_table <- df_names_2[,order(colnames(df_names_2))] |>
  select(City,pop_group,analysis_level,admin_name,group_var,group_var_value,everything()) |> select(-group_var)


# indv_table$`pop_group::idp`  <- indv_table$`pop_group::idp`  |> as.character()


full_table_df <- hh_table |> left_join(indv_table)

full_table <- full_table_df[-2:-3,]  |> mutate(
  group_var_value = str_replace_all(group_var_value,"hc-","")
) |> select(-admin_name) |> mutate(
  analysis_level = case_when(group_var_value %in% c("at_least_one_disability","no_disability") ~ "Disability status",T~analysis_level)
) |> distinct()


# neigh_analysis ----------------------------------------------------------
full_table$analysis_level |> unique()

full_table <- full_table |> rename(
  population_group =pop_group
) |> mutate(
  population_group =case_when(population_group == "pop_group" ~"population_group",
                              T~population_group)
)

neigh_analysis <- full_table |> filter(analysis_level %in% c("analysis_level","Neighbourhood","City"))

intercity_analysis <- full_table |> filter(analysis_level %in% c("analysis_level","No grouping","gender_hoh",
                                                                 "Disability status", "City"))
neigh_analysis <- neigh_analysis |> mutate(
  group_var_value = group_var_value |> str_replace_all("_b","")
) |> mutate(
  group_var_value = case_when(group_var_value == "idp-Horseed_CA11" ~ "Barwaqo(CA11)",
                              T~group_var_value)
)



nrow(neigh_analysis)+nrow(intercity_analysis)

source("write_formatted analysis.R")
write_analysis(neigh_analysis = neigh_analysis,
               intercity_analysis = intercity_analysis ,
               file_path = paste0("00_output/",str_replace_all(today(),"-","_"),"_DSP_analysis.xlsx"))



#
#
# hh_table |> select(starts_with("pop_group"))
# indv_table |> select(starts_with("pop_group"))
#
#
# hh_table$`pop_group::hc` |>
# indv_table$`pop_group::hc` |> unique()
