rm(list=ls())
library(tidyverse)
library(plotly)

PATH <- ( "../../08_MIGRATION_DATA/estat_migr_imm1ctz.tsv")

data <- read.delim(PATH, header = TRUE, sep = "\t")

data |> names()


so_se <- data |>
  select(name ="freq.citizen.agedef.age.unit.sex.geo.TIME_PERIOD",
         everything()) |>
  filter(grepl(",SO,",name)) |>
  filter(grepl(",SE",name)) |>
  filter(grepl(",T,",name)) |>
  filter(name == "A,SO,COMPLET,TOTAL,NR,T,SE") |>
  mutate(
    name = "SO_to_SE"
  ) |> pivot_longer(cols = !name,names_to = "col_name") |>
  select(-name)


# Clean the col_name column to extract the year
data <- so_se %>%
  mutate(year = as.numeric(gsub("X", "", col_name)))

data$value <- data$value |> as.numeric()


# plot_ly(data,
#         x = ~year,
#         y = ~value,
#         type = 'scatter',
#         mode = 'lines+markers+text', # add text mode
#         text = ~value,               # show value labels
#         textposition = 'top center'  # position of text
# ) %>%
#   layout(
#     xaxis = list(title = "Year"),
#     yaxis = list(title = "")
#   )
#



####### based on previous residency
data_3 <- read.delim("../../08_MIGRATION_DATA/estat_migr_imm5prv.tsv", header = TRUE, sep = "\t")
data_3$freq.partner.agedef.age.unit.sex.geo.TIME_PERIOD

df <- data_3 |>
  select(name ="freq.partner.agedef.age.unit.sex.geo.TIME_PERIOD",
       everything()) |>
  filter(grepl(",SO,",name)) |>
  filter(grepl(",SE",name)) |>
  filter(grepl(",T,",name)) |>
  filter(name == "A,SO,COMPLET,TOTAL,NR,T,SE") |>
  mutate(
    name = "SO_to_SE"
  ) |> pivot_longer(cols = !name,names_to = "col_name") |>
  select(-name)



# Clean the col_name column to extract the year
df <- df |>
  mutate(year = as.numeric(gsub("X", "", col_name))) |>
  rename (from_somalia_directly=value)



data_final <- df |> left_join(data)

data_final <- data_final |> rename(
  from_somalia_direct_indirect = value
) |> select(-col_name)

data_final |> clipr::write_clip(object_type = "table")


plot_ly(data_final, x = ~year) %>%

  # First line: From Somalia Directly
  add_trace(y = ~from_somalia_directly,
            name = 'From Somalia Directly',
            type = 'scatter',
            mode = 'lines+markers+text',
            text = ~from_somalia_directly,
            textposition = 'top center',
            textfont = list(size = 10),
            line = list(color = '#002D72', width = 2),
            marker = list(size = 6)) %>%

  # Second line: From Somalia Direct & Indirect
  add_trace(y = ~from_somalia_direct_indirect,
            name = 'From Somalia Direct & Indirect',
            type = 'scatter',
            mode = 'lines+markers+text',
            text = ~from_somalia_direct_indirect,
            textposition = 'top center',
            textfont = list(size = 10),
            line = list(color = '#0072BC', width = 2),
            marker = list(size = 6)) %>%

  # Layout with centered legend
  layout(
    title = 'Movements from Somalia to Sweden',
    xaxis = list(title = 'Year'),
    yaxis = list(title = 'Number of People'),

    # Centered horizontal legend at bottom
    legend = list(
      orientation = 'h',
      x = 0.5,
      xanchor = 'center',
      y = -0.2  # You can adjust this to control vertical position
    ),

    hovermode = 'x unified'
  )





plot_ly(data,
        x = ~year,
        y = ~value,
        type = 'scatter',
        mode = 'lines+markers+text', # add text mode
        text = ~value,               # show value labels
        textposition = 'top center'  # position of text
) %>%
  layout(
    xaxis = list(title = "Year"),
    yaxis = list(title = "")
  )



