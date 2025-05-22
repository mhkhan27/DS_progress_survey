rm(list=ls())
library(tidyverse)

dap <- openxlsx::read.xlsx("02_dap/DSP_index/DSP_Index.xlsx","index_dap",
                           startRow = 2)

ds_cri <- dap |>
  select(analysis_var=DS_criteria,label =iasc_criteria) |>
  filter(!is.na(analysis_var))
sub_cri <- dap |> select(
  analysis_var = sub_criteria_indicator_name,
  label =iasc_sub_criteria
) |> filter(!is.na(analysis_var))

dap_criteria <- ds_cri |> bind_rows(sub_cri)
dap_criteria$analysis_var <- dap_criteria$analysis_var  |> trimws()

dsp_df <- read.csv("03_output/05_analysis/dsp_index_analysis.csv")




dsp_df_v2 <- dsp_df |>
  filter(grepl("dsp_value|i_ds_cri",analysis_var)) |>
  filter(!analysis_var %in% "dsp_value_6" ) |>
  filter(group_var == "City_Name" |
           grepl("hc-",group_var_value)) |> mutate(
             group_var_value = str_replace_all(group_var_value,"hc-","")
           ) |>
  left_join(dap_criteria) |> mutate(
    label = case_when(is.na(label) ~ analysis_var,T~label)
  ) |> select(pop_group,group_var_value,label,stat)

dsp_df_v2_mog <-dsp_df_v2 |>
  filter(group_var_value == "Mogadishu") |> mutate(
    group_var_value = "MogKahda"
  ) |> bind_rows(
    dsp_df_v2 |>
      filter(group_var_value == "Mogadishu") |> mutate(
        group_var_value = "MogDaynile"
      )
  )


df <- dsp_df_v2 |>
  filter(group_var_value !="Mogadishu") |>
  bind_rows(dsp_df_v2_mog)

# for megan ---------------------------------------------------------------
df |> names()
data<- df |> select(
  pop_group,
  City = group_var_value,
  IASC_Criteria=label,
  Value= stat
)

library(openxlsx)
analyticaid::write_formatted_excel(list(data = data),
                                   output_path = "00_output/02_data_for_megan/iasc_criteria.xlsx",
                                   # hader_front_color = "black",
                                   header_fill_color = Sys.getenv("perehid_blue")
                                  )

# grp ---------------------------------------------------------------------

df <- df |> filter(label != "9. Social cohesion and integration")


for( i in unique(df$group_var_value )) {

  df_g <- df |> filter(group_var_value ==i) |>
    filter(label != "dsp_value")

  dsp_hc <- df |> filter(group_var_value ==i,
                         label == "dsp_value",
                         pop_group == "HC") |> pull(stat)
  dsp_idp <- df |> filter(group_var_value ==i,
                         label == "dsp_value",
                         pop_group == "IDP") |> pull(stat)

  ggplot(data=df_g, aes(x = label,
                      y=stat, fill=pop_group)) +

    geom_bar(stat="identity", position=position_dodge())+
    geom_text(aes(label=paste0(round(stat,0),"%"),colour=pop_group),
              angle =90 ,
              vjust=.5,
              hjust = case_when(df_g$stat > 10 ~1,T~-.1),

              fontface = "bold",show.legend = F,
              position = position_dodge(0.9), size=3.5)+
    scale_colour_manual(values  = c("#353535","#353535"))+
    scale_fill_manual(values  = c("#9bc4a2","#009EDB"))+

    theme_minimal()+
    theme(axis.text.x = element_text(angle = 0,vjust = .8),
          legend.position = "bottom",
          axis.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          # axis.text.y = element_blank(),
          legend.title = element_blank(),
          axis.ticks = element_line(),
          legend.box.spacing = unit(0, "pt"),

          plot.title = element_text(size=15,hjust = 0.5,face = "bold"),
          plot.subtitle = element_text(hjust =.5,size=10,colour = "blue")
          )+
    scale_y_continuous(limits=c(0,max(df$stat,na.rm = T)),
                       expand = c(0,0)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
    ggtitle(label = paste0("% of HH passes IASC Criteria-",i),
            subtitle = paste0("DSP value-     HC:",dsp_hc,";     IDP-",dsp_idp))

ggsave(filename = paste0("00_output/01_graphs/",i,"iasc.png"),
       units = "in",width = 12,height = 7)

  }


