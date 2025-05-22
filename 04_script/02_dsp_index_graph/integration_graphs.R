rm(list=ls())
library(tidyverse)

indicator <- c("D_5_preferred_loc","W_2_IntegrationLevel")[1]


df <- read.csv("03_output/05_analysis/hh_analysis.csv")

df_i <- df |> filter(analysis_var == indicator) |>
  filter(group_var == "City_Name" |
           grepl("hc-",group_var_value)) |>
  filter(!is.na(stat)) |>
  mutate(
    group_var_value = str_replace_all(group_var_value,"hc-","") ,
    analysis_var_value = gsub('[0-9]+', '', analysis_var_value)  |>
      snakecase::to_sentence_case(),
    # stat = round(stat*100,2)
  ) |> select(pop_group,
              city = group_var_value,
              option = analysis_var_value,
              value =stat

  )  |> filter( option != "")

df_i$option |> unique()

mog_df <- df_i |> filter(city == "Mogadishu")

mog <- mog_df |> mutate(
  city = "MogDaynile"
) |> bind_rows(
  mog_df |> mutate(
    city = "MogKahda"
  )

)

df_i <- df_i |> filter(city != "Mogadishu") |>
  bind_rows(mog)

df_i$option |> uniq

if(indicator =="W_2_IntegrationLevel" ) {
col <- c("city",
         "IDP_Very integrated",
         "IDP_Integrated",
         "IDP_Neutral",
         "IDP_Not integrated",
         "IDP_Not integrated at all",

         "HC_Very integrated",
         "HC_Integrated",
         "HC_Neutral",
         "HC_Not integrated",
         "HC_Not integrated at all"
)}


if(indicator == "D_5_preferred_loc") {
  col <- c("city",
  "IDP_This location",
  "IDP_Place of origin",
  "IDP_Another location within somalia",
  "IDP_Another location outside somalia",

  "HC_This location",
  "HC_Place of origin",
  "HC_Another location within somalia",
  "HC_Another location outside somalia")
}



df_pi <-  df_i |>
  pivot_wider(id_cols = city,
              names_from = c("pop_group","option"),
              values_from = value) |>
  select(col)








df_pi[is.na(df_pi)] <-  0


df_pi |> clipr::write_clip()

# df_ii <- df_i |> filter(pop_group == "IDP")
#
#
# # donut  ------------------------------------------------------------------
#
# data <- df_ii |> group_by(city) |>  mutate(
#   fraction = value/sum(value),
#   ymax = cumsum(fraction),
#   ymin = c(0, head(ymax, n=-1)),
#   labelPosition = (ymax + ymin) / 2,
#   label =  paste0(round(value,0), "%"),
#   x = case_when(round(value) > 3 ~ 3.5,T~4.2),
#   color = case_when(option == "Not integrated at all" ~ "darkred",
#                     option == "Not integrated" ~ "#fac04e",
#                     option == "Neutral" ~ "#ffff99",
#                     option == "Integrated" ~ "#009EDB",
#                     option == "Very integrated" ~ "#9bc4a2")
# )
# colr <- c("#9bc4a2","#009EDB","#ffff99","#fac04e","darkred")
#
# data$option <- factor(data$option,
#                       levels = c("Very integrated","Integrated",
#                                  "Neutral", "Not integrated", "Not integrated at all" ),
#                       labels = c("Very integrated","Integrated",
#                                  "Neutral", "Not integrated", "Not integrated at all"))
#
#
# data <- data |> filter(round(value,0) > 0)
#
# ggplot(data, aes(ymax=ymax, ymin=ymin,
#                  xmax=4, xmin=3,
#                  fill= option)) +
#   geom_rect() +
#   geom_label( x=data$x,
#               aes(y=labelPosition,
#                   label=label),
#               size=3,
#               fontface = "bold",
#               label.size = NA,
#               show.legend = F) +
#   coord_polar(theta="y",direction = -1) + # Try to remove that to understand how the chart is built initially
#   xlim(c(2,4))+
#   scale_fill_manual(values = colr) +
#
#   theme_void()+
#
#   theme(legend.position = "bottom",
#         legend.title = element_blank(),
#         legend.box.spacing = unit(.2,"in"),
#         legend.text = element_text(size =9)) +
#   facet_wrap(~city,
#              nrow = 2,
#              ncol = 4,
#              strip.position = "bottom")
