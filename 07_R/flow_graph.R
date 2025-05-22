library(dplyr)
library(tidyr)
library(tidyverse)
library(purrr)
library(tidygraph)
library(networkD3)
library(tidygraph)



################################# SANKEY FUNCTION ######################################
sankey_from_data_frame <- function(data, val_col,height,width=1000, ...) {

  weight <- enquo(val_col)

  df <- 1:(ncol(data)-2) %>%
    # collapse dataframe into 3 columns: from, to, weight
    map_df(~ select(data,
                    from = !! quo(names(data)[.x]),
                    to = !! quo(names(data)[.x + 1]),
                    !! weight)) %>%
    drop_na() %>%
    group_by(from, to) %>%
    summarise(weight = sum(!! weight)) %>%
    mutate(colour = to) # used to colour edges in the sankeyNetwork function (optional)

  # create tidygraph from data frame
  ig <- tidygraph::as_tbl_graph(df)

  # extract node data and reduce ids by 1 (required for D3 plotting)
  nodes <- as_tibble(ig) %>%
    rowid_to_column("id") %>%
    mutate(id = id -1) %>%
    as.data.frame()

  # do the same for the edges
  edges <- ig %>%
    activate(edges) %>%
    as_tibble() %>%
    mutate(from = from - 1, to = to - 1) %>%
    as.data.frame()


  # plot work networkD3::sankeyNetwork
  sankeyNetwork(Links = edges,
                # iterations = 10,
                Nodes = nodes,
                Source = "from",
                Target = "to",
                NodeID = "name", Value = "weight",
                # LinkGroup = "color", #(use if you want to colour edges)
                fontSize = 10,
                fontFamily = "sans-serif",
                width = width,
                height = height

  )
}




# data --------------------------------------------------------------------


dap <- openxlsx::read.xlsx("02_dap/DSP_index/DSP_Index.xlsx","index_dap",
                           rows = 2:100,skipEmptyRows = T,skipEmptyCols = T,
                           fillMergedCells =T )|>
  select(iasc_criteria,iasc_sub_criteria,indicator,Weights) |>
  distinct() |>
  filter(!is.na(Weights)) |>
  mutate(Weights= case_when(Weights == "Lower" ~ 8000,
                            T~20000)) |>
  mutate(dsp = "DSP INDEX")  |>
  mutate(dsp = case_when(iasc_criteria %in% c("6. Family Reunification",
                                              "9. Social cohesion and integration",
                                              "7. Participation in Public Affairs ",
                                              "8. Acess to Effective Remedies and Justice"
  ) ~NA_character_,T~dsp)) |> select(dsp,everything())

graph <- sankey_from_data_frame(data = dap,val_col = Weights,
                                height = 800,width = 1200)


javascript_string <-
  'function(el) {
     d3.select(el).selectAll(".link")
       .style("stroke", d => d.source.color);
  }'



sankey <-
  htmlwidgets::prependContent(graph,
                              htmltools::tags$h2("Methodology::Durable Solution Progress(DSP) Index"),
                              htmltools::tags$em(htmltools::tags$h5("A thicker line signifies a higher weight assigned to the indicator, while a thinner line reflects a lower weight."))) |>
  htmlwidgets::onRender( jsCode = javascript_string)

htmlwidgets::saveWidget(sankey, file="00_output/DSP_methodology_flow_diagram/DSP_index_flow_diagram.html")

library(webshot)
# you convert it as png
webshot(url = "00_output/DSP_methodology_flow_diagram/DSP_index_flow_diagram.html",
        file = "00_output/DSP_methodology_flow_diagram/DSP_index_flow_diagram.png",
        vwidth = 1000, vheight = 900)
# htmlwidgets::saveWidget(sankey, file="DSP_index_flow_diagram.pdf")


