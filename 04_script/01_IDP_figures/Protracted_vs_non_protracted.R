rm(list=ls())

library(tidyverse)
library(openxlsx)
library(srvyr)
library(analysistools)
tar <- c("hh","indv")


source("04_script/01_IDP_figures/write_output.R")


# population --------------------------------------------------------------


city_pop_df <- data.frame(
  strata = c("Baardheere",
             "Baydhaba",
             "Berdaale",
             "Doolow",
             "Kismaayo",
             "MogDaynile",
             "MogKahda",
             "Xudur"),
  population = c(  6098 ,#Baardheere
                   # 36588  ,# indv Baardheere

                   121915,#Baydhaba,
                   # 731492 ,# indv Baydhaba,

                   14915,#Berdaale,
                   # 89490,# indv Berdaale,

                   20677 ,#Doolow,
                   # 124062  ,#indv Doolow,

                   28239 ,#Kismaayo,
                   # 169434  ,# indv Kismaayo,

                   91720,#MogDaynile,
                   # 550008 ,# indv MogDaynile,

                   71427 ,#MogKahda,
                   # 428564  ,# indv MogKahda,

                   9118 #Xudur
                   # 53748  #indv Xudur

  ),

  source = c(     "R3",#Baardheere
                  "R3",#Baydhaba,
                  "R3",#Berdaale,
                  "R3",#Doolow,
                  "R3",#Kismaayo,
                  "R2",#MogDaynile,
                  "R2",#MogKahda,
                  "R3"#Xudur
  )
)




city_pop_df_indv <- data.frame(
  strata = c("Baardheere",
             "Baydhaba",
             "Berdaale",
             "Doolow",
             "Kismaayo",
             "MogDaynile",
             "MogKahda",
             "Xudur"),
  population_indv = c(  65825, #36588  ,# indv Baardheere
                        624748, #731492 ,# indv Baydhaba,
                        89490,# indv Berdaale,
                        112358, #124062  ,#indv Doolow,
                        122677, #169434  ,# indv Kismaayo,
                        541008, #550008 ,# indv MogDaynile,
                        428564,#428564  ,# indv MogKahda,
                        54208,#53748  #indv Xudur

  ),

  source = c(     "OCHA",#Baardheere
                  "OCHA",#Baydhaba,
                  "R3",#Berdaale,
                  "OCHA",#Doolow,
                  "OCHA",#Kismaayo,
                  "OCHA",#MogDaynile,
                  "R2/OCHA",#MogKahda,
                  "OCHA"#Xudur
  )
)



# data preparation --------------------------------------------------------


dis_cols <- c("vision",
              "hearing",
              "mobility",
              "comunicat",
              "cognition",
              "selfcare")

# read data ---------------------------------------------------------------


hh <-openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",1)

indv_data <- openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",2) |>
  mutate(age_group = case_when(age_years %in% 0:4 ~ "0_4",
                               age_years %in% 5:11 ~ "5_11",
                               age_years %in% 12:17 ~ "12_17",
                               age_years > 17 ~ "18+"),
         age_gender = paste0(sex_member, "_",age_group))

# recoding ----------------------------------------------------------------

indv_data_hoh <- indv_data |> mutate(
  gender_hoh = case_when(resp_hhh.hhh == 1 ~ sex_member,T~NA_character_)
) |> filter(!is.na(gender_hoh)) |> group_by(X_uuid) |> reframe(
  gender_hoh = paste0(gender_hoh)
)


hh <- hh |> left_join(indv_data_hoh) |>
  mutate(gender_hoh = case_when(is.na(gender_hoh)~"male",T~gender_hoh)) |>
  mutate(
    gender_hoh = paste0(City_Name,"::",gender_hoh),
    strata = case_when(pop_group == "idp" & A_6_neighbourhood == "Horseed_CA11" ~ "idp-Horseed_CA11",
                       T~strata),
    i_hh_with_disability =case_when(rowSums(hh[dis_cols] ==  "a_lot_of_difficulties" |
                                              hh[dis_cols]  == "cannot_do_at_all",na.rm =T) >0 ~ "at_least_one_disability",
                                    T~ "no_disability"),
    disabily_group = paste0(City_Name,"::",i_hh_with_disability )
  )


indv_data <- indv_data |> select(-any_of("strata"))  |>
  left_join(hh |> select(X_uuid,strata))



survey <- openxlsx::read.xlsx("01_input/tool/kobo_tool.xlsx",1)



sample_frame <- openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",3) |>
  select(City_Name,strata,population,target,completed_survey) |> bind_rows(
    data.frame(
      City_Name = "Baydhaba" ,
      strata = "idp-Horseed_CA11",
      population = 4285,
      target = 67
    )
  )


sample_frame <- sample_frame |> mutate(
  City_Name = case_when(grepl("Kahda",strata) ~ "MogKahda",
                        grepl("Daynile",strata) ~ "MogDaynile",
                        T~City_Name)
)  |> bind_rows(city_pop_df
                #               |> mutate(
                # City_Name = strata)
)




hh_s <- hh |>
  filter(!is.na(D_1_4_year_left_origin))  |>
  filter(pop_group == "idp") |> mutate(
    month = case_when(D_1_4_1_season_left == "jilaal_dec_mar" ~ "01",
                      D_1_4_1_season_left == "gu_mar_jun" ~ "05",
                      D_1_4_1_season_left == "hagaa_jul_sep" ~ "08",
                      D_1_4_1_season_left == "deyr_sep_nov" ~"10"),
    start = as.Date(start),
    D_1_4_year_left_origin = as.Date(D_1_4_year_left_origin)
  ) |> mutate(
    date_left_orgin = as.Date(paste0(year(D_1_4_year_left_origin),"-",month,"-01"))
  ) |> mutate(
    month_duration = interval(date_left_orgin,start) %/% months(1)

  )  |> mutate(
    pct_protracted_idp_12_mnth = (month_duration >12) |> as.numeric() ,
    pct_protracted_idp_36_mnth = (month_duration >36 ) |> as.numeric(),
    pct_protracted_idp_60_mnth =  (month_duration >60) |> as.numeric(),

    protracted_gender_12_mnth = paste0(gender_hoh,"-",pct_protracted_idp_12_mnth),
    protracted_gender_36_mnth = paste0(gender_hoh,"-",pct_protracted_idp_36_mnth),
    protracted_gender_60_mnth = paste0(gender_hoh,"-",pct_protracted_idp_60_mnth),


    protracted_disabled_12_mnth = paste0(disabily_group,"-",pct_protracted_idp_12_mnth),
    protracted_disabled_36_mnth = paste0(disabily_group,"-",pct_protracted_idp_36_mnth),
    protracted_disabled_60_mnth = paste0(disabily_group,"-",pct_protracted_idp_60_mnth)


  ) |>
  select(strata,
         City_Name,
         survey_weights,
         month_duration,
         gender_hoh,
         disabily_group,
         pct_protracted_idp_12_mnth,
         pct_protracted_idp_36_mnth,
         pct_protracted_idp_60_mnth,
         protracted_gender_12_mnth,
         protracted_gender_36_mnth,
         protracted_gender_60_mnth,
         protracted_disabled_12_mnth,
         protracted_disabled_36_mnth,
         protracted_disabled_60_mnth)



hh_idp_svy <- as_survey(hh_s,strata = strata ,weights = survey_weights)
# hh_idp_svy <- as_survey(hh_s)

HH_IDP_analysis_a <- create_analysis(design = hh_idp_svy,group_var = c("strata","City_Name","gender_hoh","disabily_group"))$results_table

HH_IDP_analysis <- HH_IDP_analysis_a |>
  filter(!analysis_var %in% c("strata","survey_weights","month_duration","City_Name")) |>
  filter(analysis_type != "median") |>
  select(analysis_var,analysis_var_value,group_var,group_var_value,stat) |> mutate(
    group_var = case_when(is.na(group_var) ~ "All",
                          group_var == "strata" ~ "Neighbourhood",
                          group_var == "City_Name" ~ "City",
                          T~group_var),
    group_var_value = case_when(is.na(group_var_value) ~ "All",
                                T~group_var_value)
  ) |> filter(!(group_var == "All" & grepl("::",analysis_var_value)))



# neighbourhood and city --------------------------------------------------


analysis_neighbou_city <-  HH_IDP_analysis |>
  filter(!analysis_var %in% c("gender_hoh",
                              "disabily_group",
                              "protracted_gender_12_mnth",
                              "protracted_gender_36_mnth",
                              "protracted_gender_60_mnth",
                              "protracted_disabled_12_mnth",
                              "protracted_disabled_36_mnth",
                              "protracted_disabled_60_mnth")) |>
  filter(!group_var %in% c("gender_hoh","disabily_group")) |>

  filter(!is.na(group_var)) |>
  pivot_wider(id_cols = c("group_var","group_var_value"),
              names_from = analysis_var,values_from = stat) |>
  rename(strata = group_var_value)


# gender_disablity --------------------------------------------------------
analysis_gender_disab <-  HH_IDP_analysis |>
  filter(analysis_var %in% c("gender_hoh","disabily_group")) |>
  filter(group_var %in% c("Neighbourhood","City")) |> mutate(
    analysis_var_value = gsub(".*:", "", analysis_var_value)
  ) |> pivot_wider(id_cols = group_var_value,values_from = stat,
                   names_from =analysis_var_value ) |>
  rename(strata = group_var_value)


# hoh_gender --------------------------------------------------------------

analysis_gender <-  HH_IDP_analysis |>
  filter(analysis_var %in% c("protracted_gender_12_mnth",
                             "protracted_gender_36_mnth",
                             "protracted_gender_60_mnth")) |>
  filter(group_var %in% c("Neighbourhood","City")) |> mutate(
    analysis_var_value = gsub(".*:", "", analysis_var_value)
  ) |> mutate(
    analysis_var_value = paste0(analysis_var,"::",analysis_var_value)
  )|> pivot_wider(id_cols = group_var_value,values_from = stat,
                  names_from= analysis_var_value ) |>
  rename(strata = group_var_value)


# disabbility --------------------------------------------------------------

analysis_disability <-  HH_IDP_analysis |>
  filter(analysis_var %in% c("protracted_disabled_12_mnth",
                             "protracted_disabled_36_mnth",
                             "protracted_disabled_60_mnth")) |>
  filter(group_var %in% c("Neighbourhood","City")) |> mutate(
    analysis_var_value = gsub(".*:", "", analysis_var_value)
  ) |> mutate(
    analysis_var_value = paste0(analysis_var,"::",analysis_var_value)
  )|> pivot_wider(id_cols = group_var_value,values_from = stat,
                  names_from= analysis_var_value ) |>
  rename(strata = group_var_value)



# overall -----------------------------------------------------------------


all_svy <- as_survey((hh_s |>
                        select(strata,survey_weights,
                               gender_hoh,disabily_group,
                               protracted_disabled_12_mnth,
                               protracted_disabled_36_mnth,
                               protracted_disabled_60_mnth,
                               protracted_gender_12_mnth,
                               protracted_gender_36_mnth,
                               protracted_gender_60_mnth
                        ) |>
                        mutate(gender_hoh =  gsub(".*:", "", gender_hoh),
                               disabily_group = gsub(".*:", "", disabily_group),
                               protracted_disabled_12_mnth =gsub(".*:", "", protracted_disabled_12_mnth),
                               protracted_disabled_36_mnth =gsub(".*:", "", protracted_disabled_36_mnth),
                               protracted_disabled_60_mnth =gsub(".*:", "", protracted_disabled_60_mnth),

                               protracted_gender_12_mnth =gsub(".*:", "", protracted_gender_12_mnth),
                               protracted_gender_36_mnth =gsub(".*:", "", protracted_gender_36_mnth),
                               protracted_gender_60_mnth =gsub(".*:", "", protracted_gender_60_mnth),


                        )),
                     strata = strata ,weights = survey_weights)

HH_IDP_analysis_all <- create_analysis(design = all_svy)$results_table |>
  filter(analysis_var %in% c("gender_hoh","disabily_group",
                             "protracted_gender_12_mnth",
                             "protracted_gender_36_mnth",
                             "protracted_gender_60_mnth",

                             "protracted_disabled_12_mnth",
                             "protracted_disabled_36_mnth",
                             "protracted_disabled_60_mnth"
  )) |>
  mutate(strata = "All",
         analysis_var_value = case_when(!analysis_var %in% c("gender_hoh","disabily_group")
                                        ~ paste0(analysis_var,"::",analysis_var_value),
                                        T~analysis_var_value
         )
  ) |>
  pivot_wider(id_cols = strata,names_from = analysis_var_value,values_from = stat)


# adding all --------------------------------------------------------------

analysis_gender_disab <- analysis_gender_disab |>
  bind_rows(HH_IDP_analysis_all |> select(strata,female, male,
                                          at_least_one_disability, no_disability))

analysis_gender <- analysis_gender |>
  bind_rows(HH_IDP_analysis_all |> select(strata,starts_with("protracted_gender")))

analysis_disability <- analysis_disability |>
  bind_rows(HH_IDP_analysis_all |> select(strata,starts_with("protracted_dis")))

analysis <- analysis_neighbou_city |>
  left_join(analysis_gender_disab) |>
  left_join(analysis_gender) |>
  left_join(analysis_disability) |>
  left_join(sample_frame) |>
  select(City_Name,population,everything()) |>
  select(-target,-completed_survey) |> mutate(
    City_Name = case_when(is.na(City_Name) ~ strata,
                          T~City_Name),
    source =case_when(group_var == "Neighbourhood" ~ "Sample frame",
                      T~source)
  ) |>
  select(City = City_Name,strata,group_var,
         pop_source=source,
         population_hh= population,
         everything())

analysis <- analysis |> mutate(
  population_hh = case_when(City == "All" ~ sum(analysis[analysis$group_var == "City",]$population_hh),
                            T~population_hh),
  pct_non_protracted_idp_12_mnth = 1- pct_protracted_idp_12_mnth,
  pct_non_protracted_idp_36_mnth =1- pct_protracted_idp_36_mnth,
  pct_non_protracted_idp_60_mnth =1- pct_protracted_idp_60_mnth,
) |> relocate(pct_non_protracted_idp_12_mnth,.after = pct_protracted_idp_12_mnth) |>
  relocate(pct_non_protracted_idp_36_mnth,.after = pct_protracted_idp_36_mnth) |>
  relocate(pct_non_protracted_idp_60_mnth,.after = pct_protracted_idp_60_mnth)


write.csv(analysis,"00_output/protracted_pct.csv")

# covert to count ---------------------------------------------------------

col_multiply <- names(analysis)[!names(analysis) %in% c("City","strata","group_var","pop_source","population_hh")]

analysis <- analysis |> left_join(city_pop_df_indv |>
                                    select(-source)) |>
  mutate(population_indv = case_when(is.na(population_indv) ~ population_hh*6,
                                     T~population_indv))


analysis_hh <- analysis |> mutate(
  across(col_multiply, ~round(population_hh*.,0))
)

analysis_indv <- analysis |> mutate(
  across(col_multiply, ~round(population_indv*.,0))
)


analysis_list <- list()

for(i in tar) {

  if(i == "indv"){analysis <- analysis_indv |> rename(
    population = population_indv
  ) |> select(-population_hh)}

  if(i == "hh"){analysis <- analysis_hh |> rename(
    population = population_hh
  ) |> select(-population_indv)}

  # rename ------------------------------------------------------------------

  analysis <- analysis |> select(City,
                                 strata = strata,
                                 group_var,
                                 population,
                                 pop_source,
                                 overall_male = male,
                                 overall_female = female,
                                 overall_at_least_one_disabled = at_least_one_disability,
                                 overall_no_disabled =no_disability,
                                 everything()
  ) |> rename(
    overall_protracted_idp_12_mnth = pct_protracted_idp_12_mnth,
    overall_protracted_idp_36_mnth= pct_protracted_idp_36_mnth,
    overall_protracted_idp_60_mnth=pct_protracted_idp_60_mnth,
    overall_non_protracted_idp_12_mnth = pct_non_protracted_idp_12_mnth,
    overall_non_protracted_idp_36_mnth= pct_non_protracted_idp_36_mnth,
    overall_non_protracted_idp_60_mnth=pct_non_protracted_idp_60_mnth
  )


  col_order <- c("City", "strata", "group_var", "population", "pop_source",
                 "overall_male", "overall_female", "overall_at_least_one_disabled",
                 "overall_no_disabled",
                 "overall_protracted_idp_12_mnth", "overall_non_protracted_idp_12_mnth",
                 "overall_protracted_idp_36_mnth", "overall_non_protracted_idp_36_mnth",
                 "overall_protracted_idp_60_mnth", "overall_non_protracted_idp_60_mnth",

                 "protracted_gender_12_mnth::male-1", "protracted_gender_12_mnth::female-1",
                 "protracted_gender_12_mnth::male-0",  "protracted_gender_12_mnth::female-0",

                 "protracted_gender_36_mnth::male-1", "protracted_gender_36_mnth::female-1",
                 "protracted_gender_36_mnth::male-0",  "protracted_gender_36_mnth::female-0",


                 "protracted_gender_60_mnth::male-1", "protracted_gender_60_mnth::female-1",
                 "protracted_gender_60_mnth::male-0",  "protracted_gender_60_mnth::female-0",



                 "protracted_disabled_12_mnth::at_least_one_disability-1", "protracted_disabled_12_mnth::no_disability-1",
                 "protracted_disabled_12_mnth::at_least_one_disability-0","protracted_disabled_12_mnth::no_disability-0",

                 "protracted_disabled_36_mnth::at_least_one_disability-1", "protracted_disabled_36_mnth::no_disability-1",
                 "protracted_disabled_36_mnth::at_least_one_disability-0","protracted_disabled_36_mnth::no_disability-0",


                 "protracted_disabled_60_mnth::at_least_one_disability-1", "protracted_disabled_60_mnth::no_disability-1",
                 "protracted_disabled_60_mnth::at_least_one_disability-0","protracted_disabled_60_mnth::no_disability-0"


  )


  names(analysis)[!names(analysis) %in% col_order]

  col_order[!col_order %in% names(analysis)]


  analysis <- analysis |> select(col_order)


  col_name <- names(analysis)





  # first row  --------------------------------------------------------------

  col_name_df <- data.frame(
    col_name = col_name
  ) |> mutate(
    Protected_non_protected = case_when(grepl("pct_protracted",col_name) ~ "Protracted",
                                        grepl("pct_non_protracted",col_name) ~ "Non-protracted",
                                        grepl("-1",col_name) ~ "Protracted",
                                        grepl("-0",col_name) ~ "Non-protracted",
                                        T~col_name
    ),
    disagregation = gsub(".*:", "", col_name) |> str_replace_all("-1","") |>
      str_replace_all("-0","") |> trimws() |> str_replace_all("pct","overall")
  ) |> mutate(
    Protected_non_protected = case_when(grepl("12",col_name) ~ paste0(Protected_non_protected , "-12 months"),
                                        grepl("36",col_name) ~ paste0(Protected_non_protected , "-36 months"),
                                        grepl("60",col_name) ~ paste0(Protected_non_protected , "-60 months"),
                                        T~Protected_non_protected)
  ) |> mutate(col_order =  gsub("::.*", "", col_name) ) |>
    mutate(col_order = case_when(grepl("overall",col_order) ~ "Overall",
                                 T~col_order),
           Protected_non_protected = Protected_non_protected|>
             str_replace_all("12_mnth-12 months","12_mnth")  |>
             str_replace_all("36_mnth-36 months","36_mnth") |>
             str_replace_all("60_mnth-60 months","60_mnth")
    ) |> mutate(
      Protected_non_protected = case_when(grepl("gender",col_name)~ paste0(Protected_non_protected," (Gender)"),
                                          grepl("disabled",col_name)~ paste0(Protected_non_protected," (disabled)"),
                                          T~Protected_non_protected),
      col_order = 	col_order |>
        str_replace_all("protracted_gender_","Gender disaggregation ") |>
        str_replace_all("protracted_disabled_","Disability disaggregation ") |>
        str_replace_all("mnth", "months") |> str_replace_all("_", " ") |> trimws()

    ) |> mutate(
      Protected_non_protected = case_when(disagregation == "overall_protracted_idp_12_mnth" ~ "12 months threshold (Overall)",
                                          disagregation == "overall_non_protracted_idp_12_mnth" ~ "12 months threshold (Overall)",

                                          disagregation == "overall_protracted_idp_36_mnth" ~ "36 months threshold (Overall)",
                                          disagregation == "overall_non_protracted_idp_36_mnth" ~ "36 months threshold (Overall)",

                                          disagregation == "overall_protracted_idp_60_mnth" ~ "60 months threshold (Overall)",
                                          disagregation == "overall_non_protracted_idp_60_mnth" ~ "60 months threshold (Overall)",

                                          T~Protected_non_protected

      ),
      disagregation = case_when(disagregation == "overall_protracted_idp_12_mnth" ~ "Protracted",
                                disagregation == "overall_non_protracted_idp_12_mnth" ~ "Non protracted",

                                disagregation == "overall_protracted_idp_36_mnth" ~ "Protracted",
                                disagregation == "overall_non_protracted_idp_36_mnth" ~ "Non protracted",

                                disagregation == "overall_protracted_idp_60_mnth" ~ "Protracted",
                                disagregation == "overall_non_protracted_idp_60_mnth" ~ "Non protracted",

                                T~disagregation

      ),
    )




  col_name_df_fist <- col_name_df  |>
    select(-Protected_non_protected,-disagregation)|>
    pivot_wider(names_from = col_name,values_from = col_order)

  col_name_df_second <- col_name_df  |>
    select(-col_order,-disagregation)|>
    pivot_wider(names_from = col_name,
                values_from = Protected_non_protected )

  col_name_df_third <- col_name_df  |>
    select(-col_order,- Protected_non_protected )|>
    pivot_wider(names_from = col_name,
                values_from =  disagregation)

  # create_name_dataframe ---------------------------------------------------


  analysis_list[[i]] <-  col_name_df_fist |>
    bind_rows(col_name_df_second) |> bind_rows(
      col_name_df_third) |>
    bind_rows(analysis |>
                mutate(pop_source = case_when(City == "All" ~ "Sum of all cities from dsp",
                                              T~pop_source)) |>
                mutate(across(everything(),as.character)) |>
                mutate(across(everything(),~replace(., is.na(.), 0)))
    )


}


write_protracted(analysis_hh = as.data.frame( analysis_list$hh),
                 analysis_indv = as.data.frame( analysis_list$indv),
                 "00_output/(non)protracted_idp_up.xlsx")


