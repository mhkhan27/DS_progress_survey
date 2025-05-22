rm(list = ls())
library(tidyverse)
library(openxlsx)
# source("04_script/index_analysis.R")
source("04_script/write_index_function.R")

# city name from analysis -------------------------------------------------
#
# strata_city_name <- openxlsx::read.xlsx("00_output/2024_07_05_DSP_analysis.xlsx",1) |>
#   select(City,group_var_value) |> distinct()

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


# the URL of your sharepoint file
dap2 <- openxlsx::read.xlsx("02_dap/DSP_Index.xlsx",sheet = "Final to use")

dap <- openxlsx::read.xlsx("02_dap/DSP_index/DSP_Index.xlsx","index_dap",
                           rows = 2:100,skipEmptyRows = T,skipEmptyCols = T,
                           fillMergedCells =T ) |>
  select(-passing_criteria) |>
  distinct()




cri_indicator_name <- dap |>
  select(col_name=indicator_name,
         idicator_code,
         col_label =indicator) |>
  filter(!(is.na(col_name) & is.na(col_label)))   |> mutate(
    col_label = paste0(idicator_code,"[CI] ",col_label)
  ) |> select(-idicator_code)

sub_indicator_name <- dap |>
  select(col_name=sub_criteria_indicator_name,
         code = iasc_sub_criteria_code,
        col_label =iasc_sub_criteria) |>
  filter(!(is.na(col_name) & is.na(col_label)))  |> mutate(
    col_label = paste0(code,"[IRIS-SC] ",col_label)
  ) |> select(-code) |> distinct()

iasc_indicator <- dap |>
  select(col_name=DS_criteria,
         code = oasc_criteria_code,
         col_label =iasc_criteria) |>
  filter(!(is.na(col_name) & is.na(col_label)))  |> mutate(
    col_label = paste0(code, "[IASC] ",col_label)
  ) |> select(-code) |> distinct()


all_indicator <- cri_indicator_name |>
  bind_rows(sub_indicator_name) |>
  bind_rows(iasc_indicator) |> mutate(
    col_label = trimws(col_label),
    col_name = trimws(col_name)
  )

col_order <- all_indicator$col_label |> sort()

df <- read.csv("03_output/05_analysis/dsp_index_analysis.csv") |>
  filter(!analysis_var %in% c("dsp_index_6"))




#
all_indicator$col_name[!all_indicator$col_name %in% df$analysis_var ]
df$analysis_var[!df$analysis_var %in% all_indicator$col_name ] |> unique()

df <- df |> left_join(all_indicator,by = c("analysis_var" = "col_name"))

df <- df |> mutate(
  col_label = case_when(is.na(col_label) ~ analysis_var,
                       T~col_label)
) |> mutate(
  col_label = case_when(!is.na(analysis_var_value) ~
                          paste0(col_label,"::",analysis_var_value),
                        T~col_label)
)
df <- df |> select(c("pop_group", "group_var", "group_var_value",
                     # "analysis_var",
                     "col_label",
                      "stat"))
df_pivot_wider <- df |> pivot_wider(
                                    values_from = stat,
                                    names_from = col_label)

dsp_index_file <-df_pivot_wider |>
  select(population_group= pop_group,
         analysis_level = group_var,
         group_var_value,
         col_order,
         everything())



names(dsp_index_file) <- case_when(
  grepl(pattern = "]",x = gsub(".*\\[","",names(dsp_index_file))) ~
    paste0("[",gsub(".*\\[","",names(dsp_index_file))),
  T~gsub(".*\\[","",names(dsp_index_file)))




dsp_index_file <- dsp_index_file |> mutate(
  analysis_level= case_when(analysis_level == "strata" & population_group == "IDP" ~ "Neighbourhood",
                            analysis_level == "strata" & population_group == "HC" ~ "City",
                            analysis_level %in% c ("City_Name","disabily_group" )~ "City",
                            T~analysis_level),
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
  ) |> select(City,,admin_name,everything()) |> mutate(
    analysis_level = case_when(grepl("at_least_one_disability|no_di",group_var_value) ~ "Disability status",
                               T~analysis_level)
  ) |>
  relocate("dsp_value",.after = "dsp_index::5") #|>
  # relocate("dsp_value_6",.after = "dsp_index_6::5")



dsp_index_file[is.na(dsp_index_file)] = 0


dsp_neighbourhood <-dsp_index_file |>
  filter(analysis_level %in% c("Neighbourhood","City"))


dsp_intercity <-dsp_index_file |>
  filter(analysis_level %in% c("gender_hoh","No grouping","Disability status","City") |
  grepl("at_least_one_disability|no_di",group_var_value))

## |> mutate(
#     analysis_level = case_when(population_group == "HC" & analysis_level == "strata" ~ "City",
#                                population_group == "IDP" & analysis_level == "strata" ~ "Neighbourhood",
#                                population_group == "IDP" & analysis_level == "City_Name" ~ "City"),
#     group_var_value = group_var_value |> str_replace_all("hc-","")
#
#   ) |> left_join(strata_city_name,by = "group_var_value") |>
#   select(City,everything())


# openxlsx formating ------------------------------------------------------

write_index_result(
                   dsp_intercity = dsp_intercity,
                   dsp_neighbourhood = dsp_neighbourhood,
                   file_path = paste0("00_output/", str_replace_all(today(), "-","_"),"_dsp_index_v2.xlsx"))

