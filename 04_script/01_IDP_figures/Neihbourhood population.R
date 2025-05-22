
library(sf)
# pop data ----------------------------------------------------------------


pop_df <- read.xlsx("01_input/04_round_3_pop/B2F_v3_Dataset_2024.xlsx")

pop_df |> names()

pop_df_sel <- pop_df |> select(
  state= State,
  adm1pcode = "Admin1.Pcode",
  district ="M-0304:.District.name" ,
  adm2pcode ="Admin2.Pcode",
  latitude = "M-0446:.Latitude",
  longitude = "M-0447:.Longitude",
  region = "M-0303:.Region.name" ,
  idp_hh = "M-0309:.Estimated.#.of.IDP.Households" ,
  idp_ind= "M-0310:.Estimated.#.of.IDP.Individuals"
)

pop_df_brdhe <- pop_df_sel|>
  filter(district =="Baardheere") |>
  filter(idp_ind >0 )

pop_df_brdhe_st <- pop_df_brdhe |>
  st_as_sf(coords = c("longitude","latitude"),crs = 4326) |>
  st_transform(crs = 32639)


# shapefile ---------------------------------------------------------------

neighbourhood <- st_read("01_input/01_neighbourhood_boundary/bardheere_neighbourhoods/bardheere_neighbourhoods.shp")

geod <- "01_input/01_neighbourhood_boundary/LORA_7Districts.gdb/"
st_layers(geod)

# 32639

neighbourhood <- st_read(geod,layer = "LORA_7Districts_NeighborhoodsSurvey2") |>
  filter(ADM2_EN=="Baardheere") |>
  select(Neig_Villa) #|> st_transform(4326)


st_write(neighbourhood,"01_input/01_neighbourhood_boundary/Baardheere.shp")

pop_df_brdhe_st_inter <-st_intersection(x = pop_df_brdhe_st,
                                        neighbourhood)



pop_df_brdhe_st_inter |> as.data.frame() |>
  group_by(Neig_Villa) |> summarise(
    idp_hh = sum(idp_hh),
    idp_ind = sum(idp_ind)
  )



#
# pop_df_brdhe_st_inter$idp_ind |> sum()
#
#
# library(tmap)
# ttm()
#
# tm_shape(pop_df_brdhe_st)+
#   tm_sf()+
#   tm_shape(neighbourhood)+
#   tm_sf()
#
