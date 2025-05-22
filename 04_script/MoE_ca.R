library(tidyverse)
library(moe)
library(openxlsx)


data <- openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",3)


data <- data |> group_by(City_Name) |> summarise(across(is.numeric,sum))


unique(data$City_Name)
df_list = list()

for(i in unique(data$City_Name)){
  data_f <- data |> filter(City_Name==i)
  moe <- moe(proportion = .5,
      n = data_f$completed_survey ,
      conf.level = 0.95,
      digits = 2,
      population.correction = T,
      population.size = data_f$population)$margin.of.error

  df_list[[i]] <- data_f |>  mutate(MoE= paste0(round(moe,0),"%"))
}


MoE_IDP <- df_list |> bind_rows() |> mutate(CI= "95%") |>
  select(City_Name,population,completed_survey,MoE,CI)



analyticaid::write_formatted_excel(list(MoE_IDP=MoE_IDP),"03_output/04_clean_data/MoE_IDP_city.xlsx")


df_list = list()

for(i in unique(data$City_Name)){
  data_f <- data |> filter(City_Name==i)
  moe <- moe(proportion = .5,
             n = data_f$completed_survey ,
             conf.level = 0.90,
             digits = 2,
             population.correction = T,
             population.size = data_f$population)$margin.of.error

  df_list[[i]] <- data_f |> mutate(MoE= paste0(ceiling(moe),"%"))
}


MoE_IDP_90 <- df_list |> bind_rows() |> mutate(CI= "90%")
