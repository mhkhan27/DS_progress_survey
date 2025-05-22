rm(list=ls())
library(tidyverse)

perehid_orange="#fac04e"
perehid_blue="#009EDB"
perehid_green="#9bc4a2"

dsp_df |> filter(pop_group == "HC",
                 analysis_var == "dsp_value",
                 group_var_value =="hc-Mogadishu"
) |> pull(stat)


dsp_df <- read.csv("03_output/05_analysis/dsp_index_analysis.csv")


stat_re <- dsp_df |> filter(pop_group == "HC",
                            analysis_var == "dsp_value",
                            group_var_value =="hc-Mogadishu"
)|> pull(stat)


dsp_value_city <- dsp_df |>
  filter(analysis_var == "dsp_value") |>
  filter(group_var %in% c("City_Name","strata")) |>
  filter(!(group_var == "strata" & pop_group =="IDP" )) |>
  select(pop_group,
         city = group_var_value,
         stat
  ) |> mutate(
    city = str_replace_all(city,"hc-",""),
    city = case_when(city == "Mogadishu" ~ "MogDaynile",
                     T~ city)
  ) |> bind_rows(
    data.frame(city  = "MogKahda",
               pop_group = "HC",
               stat =stat_re
    )) |> rename(
      value =stat
    )



# graph -------------------------------------------------------------------

dsp_value_city_gap <- dsp_value_city |>
  pivot_wider(id_cols = city,
              values_from = value,names_from = pop_group) |>
  mutate(value = paste0(round((HC-IDP)/HC*100,0)))


dsp_value_city_hc <- dsp_value_city |> filter(pop_group == "HC") |>
  mutate(id =1 )
dsp_value_city_idp <- dsp_value_city |>
  filter(pop_group == "IDP") |>
  mutate(id =2 )

ggplot(data=dsp_value_city_hc,
       aes(x = city,y=value)) +
  geom_bar(data = dsp_value_city_idp,
           fill ="#009EDB",
           color="transparent",
           stat="identity",
           position=position_dodge(),
           show.legend = T) +
  scale_fill_manual(values = "#009EDB" )+


  geom_bar(data = dsp_value_city_hc,
           stat="identity",
           show.legend = T,
           linewidth = .5,
           color="black",
           alpha = .3,
           fill="transparent",
           aes(y = value,
               group = pop_group),
           position=position_dodge())+


  geom_text(data = dsp_value_city_gap ,
            aes(y =IDP ,
                label=paste0(value, "% gap"),
                group = city),
            angle =0 ,
            hjust =.5,
            vjust = -1.3,
            # vjust =case_when(dsp_value_city_gap$value > 18 ~ -.1,
            #                  T~ 1.2),

            position=position_dodge(width = .9),
            fontface = "bold",
            show.legend = F,
            size=5)+

  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0,
                                   vjust = .8,hjust=.5,size=12),
        axis.text.y = element_text(size=12),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x= element_blank(),
        axis.title.y= element_text(size=12),

        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.length.y = unit(0,units = "cm"),
        axis.ticks = element_line(),
        panel.grid.major.y = element_line(colour = "grey85",linetype = "dashed"),
  )+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  ylab("DSP Index")

ggsave(filename = paste0("00_output/01_graphs/dsp_comparison.png"),
       units = "in",width = 12,height = 7)

# gnrl grph ---------------------------------------------------------------


dsp_value_city <- dsp_value_city |> mutate(
  color = case_when(pop_group == "IDP" ~ "#009EDB",
                    T~ "#9bc4a2")
)



ggplot(dsp_value_city,aes(x = city,
                          y = value,
                          fill=pop_group
                        )) +
  geom_bar(stat="identity",
           color="transparent",
           position = "dodge",
           show.legend = T) +
  scale_fill_manual(values = c("#9bc4a2","#009EDB"))+

  geom_text(data = dsp_value_city ,
            aes(label=value,
                group = pop_group),
            angle =0 ,
            vjust = 1.5,
            hjust =.5,
            position=position_dodge(width = .9),
            fontface = "bold",
            show.legend = F,
            size=5)+

  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0,size = 12,
                                   vjust = .8,hjust=.5),
        axis.text.y = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size=12),
        legend.title = element_blank(),
        axis.title.x= element_blank(),
        axis.title.y= element_text(size = 12),

        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.length.y = unit(0,units = "cm"),
        axis.ticks = element_line(),
        panel.grid.major.y = element_line(colour = "grey30",linetype = "dashed"),
  )+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  ylab("DSP Index")

ggsave(filename = paste0("00_output/01_graphs/dsp_value.png"),
       units = "in",width = 12,height = 7)


# baardheere --------------------------------------------------------------
