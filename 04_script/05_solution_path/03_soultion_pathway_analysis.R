rm(list = ls())

library(openxlsx)
library(tidyverse)
library(srvyr)
library(illuminate)
library(analysistools)


source("04_script/05_solution_path/solutionpath_recoding.R")


dap <- openxlsx::read.xlsx("04_script/05_solution_path/Solutions_Pathways_M+E_Plan.xlsx",2,startRow = 2)
dap <- dap |> filter(!Included %in% c("NO","No","NO"))

# pct_attendace -----------------------------------------------------------

hh_clean_data <- hh
hh_pct_edu_city <- hh_clean_data |>
  dplyr::group_by(City_Name,pop_group)|>
  dplyr::reframe(
    total_school_age_male = sum(school_aged_male,na.rm = T),
    total_school_age_female = sum(school_aged_female,na.rm = T),

    total_male_going_school = sum(E_3_male_5_17_attend_school,na.rm = T),
    total_female_going_school = sum(E_4_female_5_17_attend_school,na.rm = T)

  ) |> ungroup() |> mutate(
    i_ea_abc_attendance_boy = round((total_male_going_school/total_school_age_male)*100,0),
    i_ea_abc_attendance_girl = round((total_female_going_school/total_school_age_female)*100,0)
  ) |> select(City_Name,pop_group,i_ea_abc_attendance_boy,i_ea_abc_attendance_girl) |>
  mutate(pop_group = toupper(pop_group)) |> mutate(
    strata = City_Name
  ) |> select(City_Name,strata,everything())

### idp - NEIGHBOURHOOD
hh_pct_edu_idp_nei <- hh_clean_data |>
  filter(pop_group != "hc") |>
  dplyr::group_by(strata,pop_group) |>
  dplyr::reframe(
    total_school_age_male = sum(school_aged_male,na.rm = T),
    total_school_age_female = sum(school_aged_female,na.rm = T),

    total_male_going_school = sum(E_3_male_5_17_attend_school,na.rm = T),
    total_female_going_school = sum(E_4_female_5_17_attend_school,na.rm = T)

  ) |> ungroup() |> mutate(
    i_ea_abc_attendance_boy = round((total_male_going_school/total_school_age_male)*100,0),
    i_ea_abc_attendance_girl = round((total_female_going_school/total_school_age_female)*100,0)
  ) |> select(strata,pop_group,i_ea_abc_attendance_boy,i_ea_abc_attendance_girl) |>
  mutate(pop_group = toupper(pop_group))

##### all
hh_pct_edu_all <- hh_clean_data |>
  dplyr::group_by(pop_group) |>
  dplyr::reframe(
    total_school_age_male = sum(school_aged_male,na.rm = T),
    total_school_age_female = sum(school_aged_female,na.rm = T),

    total_male_going_school = sum(E_3_male_5_17_attend_school,na.rm = T),
    total_female_going_school = sum(E_4_female_5_17_attend_school,na.rm = T)

  ) |> ungroup() |> mutate(
    i_ea_abc_attendance_boy = round((total_male_going_school/total_school_age_male)*100,0),
    i_ea_abc_attendance_girl = round((total_female_going_school/total_school_age_female)*100,0)
  ) |> select(pop_group,i_ea_abc_attendance_boy,i_ea_abc_attendance_girl) |>
  mutate(pop_group = toupper(pop_group)) |>
  mutate(strata = "All")





# bind_rows ---------------------------------------------------------------

hh_pct_edu <- hh_pct_edu_city |>
  bind_rows(hh_pct_edu_idp_nei) |>
  bind_rows(hh_pct_edu_all) |>
  select(-City_Name) |>
  rename(City_Name= strata)




# analysis ----------------------------------------------------------------


col <- hh |> select(starts_with("i")) |>
  select(contains(c("_gl_","_abc_","_el_","_hlp_","_cgr_"))) |> names()


hh <- hh |> select(c(col,"pop_group","strata","City_Name","survey_weights"))

hh <- hh |> mutate_if(is.logical, function(x){x*1})


# HC  ---------------------------------------------------------------------

hh_data_hc <- hh  |> filter(pop_group == "hc")
hh_hc_svy <- as_survey(hh_data_hc )
HH_HC_analysis <- create_analysis(design = hh_hc_svy,group_var = c("strata"))$results_table
HH_HC_analysis <- HH_HC_analysis |> filter(analysis_type != "median")


# IDP ---------------------------------------------------------------------

hh_data_idp <- hh  |> filter(pop_group != "hc")
hh_idp_svy <- as_survey(hh_data_idp,strata = strata ,weights = survey_weights)
HH_IDP_analysis <- create_analysis(design = hh_idp_svy,group_var = c("strata","City_Name"))$results_table
HH_IDP_analysis <- HH_IDP_analysis |> filter(analysis_type != "median")



# bind_analysis -----------------------------------------------------------

HH_analysis <- HH_IDP_analysis |> mutate(pop_group =  "IDP") |> bind_rows(
  HH_HC_analysis |> mutate(pop_group =  "HC")
)  |>
  select(pop_group,group_var,group_var_value,analysis_var,
         analysis_var_value,stat,n_w,n_w_total,n,
         n_total,analysis_type)


HH_analysis <- HH_analysis |> mutate(
  group_var = case_when(is.na(group_var) ~ "No grouping",T~group_var),
  group_var_value = case_when( group_var == "No grouping" ~ "All",T~group_var_value),
)


HH_analysis <- HH_analysis |> mutate(
  stat = round(stat*100,0)
) |> filter(analysis_var %in% col) |> mutate(
  analysis_level = case_when(pop_group == "IDP" &
                               group_var == "strata" ~ "Neighbourhood",
                             T~"City"))


HH_analysis2 <- HH_analysis |>
  filter(group_var != "no_grouping") |>
  # filter((pop_group == "HC" & group_var =="strata") |
  #          (pop_group == "IDP" & group_var == "City_Name")) |>
  select(pop_group,City_Name = group_var_value,
         analysis_var,analysis_level,stat) |>
  pivot_wider(id_cols = c("pop_group","City_Name","analysis_level"),
              names_from =analysis_var,
              values_from = stat ) |>  mutate(
                City_Name = str_replace_all(City_Name,"hc-","")
              ) |>
  left_join(hh_pct_edu) |> mutate(
    analysis_level = case_when(City_Name == "All" ~ "All",
                               T~analysis_level)
  )

################### Adding city ###########################

city_df <- structure(list(City_Name = c("Baydhaba", "Berdaale", "Xudur", "Kismaayo",
                                   "Kismaayo", "Daynile", "Daynile", "Daynile", "Daynile", "Daynile",
                                   "Daynile", "Daynile", "Daynile", "Daynile", "Daynile", "Daynile",
                                   "Daynile", "Daynile", "Daynile", "Baydhaba", "Kismaayo", "Kismaayo",
                                   "Berdaale", "Baydhaba", "Baydhaba", "Xudur", "Baydhaba", "Baydhaba",
                                   "Doolow", "Kahda", "Kahda", "Kahda", "Kahda", "Kahda", "Kahda",
                                   "Kahda", "Kahda", "Kahda", "Kahda", "Kahda", "Kahda", "Doolow",
                                   "Doolow", "Xudur", "Baardheere", "Baardheere", "Berdaale", "Doolow",
                                   "Doolow", "Baydhaba", "Xudur", "Xudur", "Baardheere", "Baardheere",
                                   "Baydhaba", "Berdaale", "Baydhaba", "Baydhaba", "Baardheere",
                                   "Baydhaba", "Berdaale", "Doolow", "Kismaayo", "MogDaynile", "MogKahda",
                                   "Xudur", "Baardheere", "Baydhaba", "Berdaale", "Doolow", "Kismaayo",
                                   "Mogadishu", "Xudur","All"),
                          strata = c("Adaado_b", "Ahmed Gurey",
                                         "Buulow", "Central", "Dalxiska", "Daynile_CA01", "Daynile_CA02",
                                         "Daynile_CA09", "Daynile_CA10", "Daynile_CA11", "Daynile_CA18",
                                         "Daynile_CA19", "Daynile_CA20", "Daynile_CA21", "Daynile_CA22",
                                         "Daynile_CA23", "Daynile_CA24", "Daynile_CA25", "Daynile_CA26",
                                         "Durasalam_b", "Fanole", "Galbeet", "Horseed", "Horseed_CA11",
                                         "Horseed_b", "Horseed_xudur", "Howl Wadaag_b", "Isha_b", "Kabasa",
                                         "Kahda_CA03", "Kahda_CA04", "Kahda_CA05", "Kahda_CA06", "Kahda_CA07",
                                         "Kahda_CA08", "Kahda_CA12", "Kahda_CA13", "Kahda_CA14", "Kahda_CA15",
                                         "Kahda_CA16", "Kahda_CA17", "Kaxaarey", "Ladan", "Moorogaabey",
                                         "Northeast (NE)", "Northwest (NW)", "Oktober", "Qansakey", "Qurdubey",
                                         "Salamey_b", "Sheik Awes", "Shidoole", "Southeast (SE)", "Southwest (SW)",
                                         "Towfiq_b", "Wabeeri", "Wadajiir_b", "Weberi_b", "Baardheere",
                                         "Baydhaba", "Berdaale", "Doolow", "Kismaayo", "MogDaynile", "MogKahda",
                                         "Xudur", "Baardheere", "Baydhaba", "Berdaale", "Doolow", "Kismaayo",
                                         "Mogadishu", "Xudur","All")),
                     class = "data.frame", row.names = c(NA,  -74L))



city_df <- city_df |> distinct()


HH_analysis2<- HH_analysis2 |>
  rename(strata = City_Name) |>
  mutate(strata = str_replace_all(strata,"idp-",""))  |>
  left_join(city_df)|>
  select(pop_group,City_Name,strata,analysis_level,everything()) |>
  mutate(City_Name = case_when(City_Name == "Daynile" ~ "MogDaynile",
                               City_Name == "Kahda" ~ "MogKahda",
                               T~City_Name))


HH_analysis2$strata [!HH_analysis2$strata %in% city_df$strata]

################### Adding state ###########################

state_df <- data.frame(
  City_Name = c("Berdaale","Baydhaba","Xudur",
                "Baardheere", "Doolow", "Kismaayo",
                "MogDaynile", "MogKahda", "Mogadishu","All"),
  State = c("South west state","South west state","South west state",
            "Jubaland","Jubaland","Jubaland",
            "Banadir","Banadir","Banadir","All")
)



HH_analysis2 <- HH_analysis2 |> filter(analysis_level %in% c("City"))

list_tbl <- list()

for(i in c("_gl_","_abc_","_el_","_hlp_","_cgr_")){

  tbl <- HH_analysis2 |>
    left_join(state_df) |>
    select(State,City_Name,strata,pop_group,analysis_level,contains(i))

  tbl_df_hc_mog <- tbl |>
    filter(pop_group == "HC" & City_Name == "Mogadishu")

  tbl_df_hc_mog_DAY<- tbl_df_hc_mog |> mutate(
    City_Name = "MogDaynile"
  )
  tbl_df_hc_mog_kah<- tbl_df_hc_mog |> mutate(
    City_Name = "MogKahda"
  )

  tbl_2 <- tbl |>
    filter(!(pop_group == "HC" & City_Name == "Mogadishu")) |>
    bind_rows(tbl_df_hc_mog_DAY) |>
    bind_rows(tbl_df_hc_mog_kah)


  tbl_name <- case_when( i == "_gl_" ~"1. Government Leadership",
                         i == "_abc_" ~ "2. Access to Basic Services",
                         i == "_el_"~ "3. Employment and Livelihood",
                         i == "_hlp_"~ "4. Legal Documentation and HLP",
                         i == "_cgr_"~ "5.Climate Change and Resilience"
  )

  dap_2 <- dap |> filter(indicaor_code %in% names(tbl_2))

  data.table::setnames(tbl_2, old = c(dap_2[["indicaor_code"]]),
                       new = c(dap_2[["indicator_label"]]))


  list_tbl[[tbl_name]]  <- tbl_2 |> arrange(pop_group,State,City_Name)

}

openxlsx::write.xlsx(list_tbl, "04_script/05_solution_path/pathway_matrix.xlsx",asTable = T)







