rm(list=ls())
library(tidyverse)


city_pop_df_indv <- data.frame(
  strata = c("Baardheere",
             "Baydhaba",
             "Berdaale",
             "Doolow",
             "Kismaayo",
             "MogDaynile",
             "MogKahda",
             "Xudur",

             "idp-Northeast (NE)",
             "idp-Northwest (NW)",
             "idp-Southeast (SE)",
             "idp-Southwest (SW)"),
  population_indv = c(  65825, #36588  ,# indv Baardheere
                        624748, #731492 ,# indv Baydhaba,
                        89490,# indv Berdaale,
                        112358, #124062  ,#indv Doolow,
                        122677, #169434  ,# indv Kismaayo,
                        541008, #550008 ,# indv MogDaynile,
                        428564,#428564  ,# indv MogKahda,
                        54208, #53748,  #indv Xudur

                        14526, #indv Northeast,
                        2868,#indv Northwest,
                        10134,#indv Southeast,
                        8478#indv Southwest

  ),

  source = c(     "OCHA",#Baardheere
                  "OCHA",#Baydhaba,
                  "R3",#Berdaale,
                  "OCHA",#Doolow,
                  "OCHA",#Kismaayo,
                  "OCHA",#MogDaynile,
                  "R2/OCHA",#MogKahda,
                  "OCHA", #Xudur,

                   "R3" ,# Northeast,
                   "R3" ,# Northwest,
                   "R3" ,# Southeast,
                   "R3" # Southwest

  )
) |> rename(
  City = strata
)

df <- read.csv("00_output/protracted_pct.csv")

df <- df |> select(City, strata,group_var,
                   starts_with("pct_protracted_"),
                   # starts_with("pct_non_protracted_")
                   )


# table  ------------------------------------------------------------------

df_city <- df |> filter(group_var == "Neighbourhood") |>
  filter(City == "Baardheere") |> select(- City,- group_var) |>
  pivot_longer(cols = !c("strata"),names_to = "name",values_to = "value") |>
  left_join(city_pop_df_indv |> rename(strata = City)) |>  mutate(
    name = case_when(grepl("12_",name) ~ "12 months",
                     grepl("36_",name) ~ "36 months",
                     grepl("60_",name) ~ "60 months"),
    protracted_idp = round(value*population_indv,0),
    non_protracted_idp = round((1-value)*population_indv,0),
  ) |> select(
              strata,
              `Total IDPs`=population_indv,
              `Protracted IDPs` = protracted_idp,
              `Non protracted IDPs` = non_protracted_idp,
              name)


all_city <- df_city |> group_by(name) |>
  summarise_if(is.numeric,sum) |>
  mutate(City = "All 8 Cities")

df_city |> bind_rows(all_city)|> clipr::write_clip()

# grap - table ------------------------------------------------------------


df_city <- df |> filter(group_var == "City") |>
  select(-strata,-group_var) |>
  pivot_longer(cols = !City,names_to = "name",values_to = "value") |>
 left_join(city_pop_df_indv) |>  mutate(
    name = case_when(grepl("12_",name) ~ "12 months",
                     grepl("36_",name) ~ "36 months",
                     grepl("60_",name) ~ "60 months"),
    protracted_pop = round(value*population_indv,0),
    value = round(value*100,0)
  )  |>  mutate(
    non_protacted = 100-value,
    total = 100,
    color = case_when(name == "12 months" ~ "#009EDB",
                      name == "36 months" ~ "#A6CEE3",
                      name == "60 months" ~ "#B2DF8A")
  )


# graphs ------------------------------------------------------------------


# ggplot(data=df_city,
#        aes(forcats::fct_reorder(.desc = T,City, value, sum),y=value)) +

ggplot(data=df_city,
       aes(City,y=value)) +
  geom_bar(data = df_city,
           stat="identity",
           show.legend = T,
           # linetype = "dotted",
           linewidth = .05,
           color="grey29",alpha = .3,
           fill="grey70",
           aes(y = total,group = name),
           position=position_dodge())+

  geom_bar(data = df_city,
           stat="identity",
           color="transparent",
           aes(fill= name,
               group = name),
           position=position_dodge(),show.legend = T) +

  scale_fill_manual(values = df_city$color)+
  # scale_fill_manual( = "grey70")+


  geom_text(data = df_city ,
            aes(label=paste0(round(protracted_pop/1000,0), "K (",value, "%)"),
                group = name),
            angle =90 ,
            hjust = case_when(df_city$value > 13 ~1,T~-.1),
            vjust =.5,
            position=position_dodge(width = .9),
            # fontface = "bold",
            show.legend = F,
            size=3)+

  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0,vjust = .8,hjust=.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x= element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.length.y = unit(0,units = "cm"),
        # axis.text.y = element_blank(),
        axis.ticks = element_line(),
        panel.grid.major.y = element_line(colour = "grey95",linetype = "dashed"),
        # legend.box.spacing = unit(0, "pt")
  )+
  # scale_y_continuous(labels = function(x) {paste0(x/1000000," M")}
  #                    ,expand = c(0,0))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  ylab("% of Protracted IDP")



# baardheere --------------------------------------------------------------
Baardheere
df$strata |> unique()
df_b <- df |> filter(City == "Baardheere") |>
  select(-City,-group_var) |>
  rename(City = strata) |>
  pivot_longer(cols = !City,names_to = "name",values_to = "value") |>
  left_join(city_pop_df_indv) |>  mutate(
    name = case_when(grepl("12_",name) ~ "12 months",
                     grepl("36_",name) ~ "36 months",
                     grepl("60_",name) ~ "60 months"),
    protracted_pop = round(value*population_indv,0),
    value = round(value*100,0)
  )  |>  mutate(
    non_protacted = 100-value,
    total = 100,
    color = case_when(name == "12 months" ~ "#009EDB",
                      name == "36 months" ~ "#A6CEE3",
                      name == "60 months" ~ "#B2DF8A")
  ) |>
  mutate(City= str_replace_all(City,"idp-","")) |>
  mutate(City= str_replace_all(City,"Kahda_",""))



# neighbouhood graph ------------------------------------------------------


ggplot(data=df_b,
       aes(City,y=value)) +
  geom_bar(data = df_b,
           stat="identity",
           show.legend = T,
           # linetype = "dotted",
           linewidth = .05,
           color="grey29",alpha = .3,
           fill="grey70",
           aes(y = total,group = name),
           position=position_dodge())+

  geom_bar(data = df_b,
           stat="identity",
           color="transparent",
           aes(fill= name,
               group = name),
           position=position_dodge(),show.legend = T) +
  scale_fill_manual(values = df_b$color)+
  geom_text(data = df_b ,
            # aes(label=paste0(round(protracted_pop/1000,0), "K (",value, "%)"),
                aes(label=paste0(value, "%"),

                group = name),
            angle =90 ,
            hjust = case_when(df_b$value > 30 ~1,T~-.1),
            vjust =.5,
            position=position_dodge(width = .9),
            # fontface = "bold",
            show.legend = F,
            size=3)+

  # geom_bar(data = df_b,stat="identity",
  #          show.legend = T,
  #          # linetype = "dotted",
  #          linewidth = .05,
  #
  #          color="grey29",
  #          fill="transparent",
  #          aes(y = total,group = name,)
  #          ,position=position_dodge())+

  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0,vjust = .8,hjust=.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x= element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.length.y = unit(0,units = "cm"),
        # axis.text.y = element_blank(),
        axis.ticks = element_line(),
        panel.grid.major.y = element_line(colour = "grey95",linetype = "dashed"),
        # legend.box.spacing = unit(0, "pt")
  )+
  # scale_y_continuous(labels = function(x) {paste0(x/1000000," M")}
  #                    ,expand = c(0,0))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  ylab("% of Protracted IDP")

