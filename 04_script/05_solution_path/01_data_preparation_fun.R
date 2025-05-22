

  dis_cols <- c("vision",
                "hearing",
                "mobility",
                "comunicat",
                "cognition",
                "selfcare")


  #  data preparation  ---------------------------------------------------------------


  hh <-openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",1)

  indv_data <- openxlsx::read.xlsx("03_output/04_clean_data/clean_data.xlsx",2) |>
    mutate(age_group = case_when(age_years %in% 0:4 ~ "0_4",
                                 age_years %in% 5:11 ~ "5_11",
                                 age_years %in% 12:17 ~ "12_17",
                                 age_years > 17 ~ "18+"),
           age_gender = paste0(sex_member, "_",age_group))

  # recoding ----------------------------------------------------------------



  indv_data_hoh <- indv_data |> mutate(
    age_years = age_years |> as.numeric(),
    gender_hoh = case_when(resp_hhh.hhh == 1 ~ sex_member,T~NA_character_)
  ) |> filter(!is.na(gender_hoh)) |> group_by(X_uuid) |> reframe(
    gender_hoh = paste0(gender_hoh))



  indv_data_sch_age <- indv_data |> mutate(
    age_years = age_years |> as.numeric(),
  ) |>
    group_by(X_uuid) |> reframe(
      school_aged_male = sum(sex_member == "male" & age_years %in% c(5:17)),
      school_aged_female = sum(sex_member == "female" & age_years %in% c(5:17))

    )


  hh <- hh |>
    left_join(indv_data_hoh) |>
    left_join(indv_data_sch_age) |>
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


  indv <- indv_data |> select(-any_of("strata"))  |>
    left_join(hh |> select(X_uuid,strata))

  rm(list = ls()[!ls() %in%  c("hh","indv")])


