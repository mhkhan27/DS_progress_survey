# rm(list = ls())



# QUESTIONS ---------------------------------------------------------------

# In some countries, IDPs who displaced due to conflict are more likely to
# intend to return home if/when the conflict ends. By contrast climate IDPs may
# me more likely to intend to locally integrate – with an understanding the climate
# is not going to get better and therefore integration in urban centers is preferred.
# This latter dynamic goes some way to explain the rapid urbanization that is occurring
# in Somalia.
#
# To support the strategy, could I please ask you to run some DSP analysis to answer
# the following questions:

# # 1. Are climate IDPs more likely to locally integrate in current location
        # compared with conflict IDPs? And vice versa, are conflict IDPs more likely to intend to return?
# # 2. For IDPs who intend to return, what assistance would they require to do so?
        # (or what assistance would encourage them to return if made available)
# # 3. Do preferences for local integration vs. return vary according to length of time
        # in displacement? Do they vary geographically – by city of displacement and/or area
        # of origin?
# # 4.  City level information would be great (don’t worry about neighbourhood level)
#
# When the analysis is done, could you please draft a short narrative explaining the
# findings – highlighting how dynamics/preferences vary according to the above factors
# (length of time in displacement, geographically, etc). 1 page max would be plenty.
# Would this be possible by Friday 25 Oct?#



# read data ---------------------------------------------------------------


# survey <- openxlsx::read.xlsx("01_input/tool/kobo_tool.xlsx",1)
# choice <- openxlsx::read.xlsx("01_input/tool/kobo_tool.xlsx",2)
# idp_data$D_1_5_reasondisp |> table()



# recoding ----------------------------------------------------------------





idp_data$reason_of_leaving |> table()


analysis_col <- c("D_5_preferred_loc",
                  "D_3_times_disp",
                  "reason_of_leaving",
                  "D_6_1_opt_return",
                  "i3_total_displacement_month",
                  "i2_current_location_month"
                  )

idp_data_svy <- as_survey(idp_data |> select(all_of(analysis_col),
                                             strata,survey_weights,City_Name) |>
                            mutate(reason_and_city = paste0(reason_of_leaving,"::",City_Name),
                                   D_5_preferred_loc_city =paste0(D_5_preferred_loc,"::",City_Name) )
                          ,strata = strata ,weights = survey_weights)


idp_climate_analysis <- (create_analysis(design = idp_data_svy,
                                         group_var = c("reason_of_leaving","reason_and_city",
                                                       "D_5_preferred_loc","D_5_preferred_loc_city",
                                                       "City_Name")))$results


# Q1: ---------------------------------------------------------------------
# Are climate IDPs more likely to locally integrate in current location
# compared with conflict IDPs? And vice versa, are conflict IDPs more likely to intend to return?

### intended to return place of origin

# D_5_preferred_loc


### what is the relation between disaplacetime vs reason of leaving


