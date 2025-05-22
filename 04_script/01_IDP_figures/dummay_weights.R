library(tidyverse)





df <- data.frame(
  uuid = c("x1","x2","x3","x4","x5"),
  i_1 = c(0,1,NA_real_,1,0),
  i_2 = c(0,NA_real_,1,1,0),
  i_3 = c(0,1,0,NA_real_,1),
  i_4 = c(1,NA_real_,1,1,1),
  i_5 = c(1,1,1,1,1)
)

critical_in <- c("i_1","i_2","i_3","i_4")
all_indicator <- c("i_1","i_2","i_3","i_4","i_5")
non_cri <- all_indicator[!all_indicator%in%critical_in]

df_a <- df |> mutate(
  total_in_non_na = rowSums(!is.na(df[all_indicator]),na.rm =T),
  number_of_critical = rowSums(!is.na(df[critical_in]),na.rm =T),
  number_of_noncritical = rowSums(!is.na(df[non_cri]),na.rm =T),
  number_of_critical_2x = rowSums(!is.na(df[critical_in]),na.rm =T)*2,
  equal_weights_total = 1/( number_of_critical_2x+ number_of_noncritical)
  ) |> mutate(across(non_cri,~.x* equal_weights_total)) |>
  mutate(across(critical_in,~.x* equal_weights_total*2)) |>
  rowwise(uuid) %>%
  mutate(i_sub_w=sum(c_across(all_of(all_indicator)),na.rm=T),
         i_sub = case_when(i_sub_w >.49 ~1 ,
                           i_sub_w < .50~0)) |>
  select(i_sub,all_indicator,everything())






