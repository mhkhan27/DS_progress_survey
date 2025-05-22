# #Meeting title: achievement of solutions
# 1. Very few HHs achieved a solution (passed 5 IASC criteria).
        # a. What were the main drivers of preventing this achievement/progress towards solutions?
# 2. Existing analysis shows the cities with the lowest index scores (Daynile, Kahda, Xudur) also have very high preferences to return.
        # [From previous anlysis] a. We know in many cases conflict IDPs are more likely to want to return home.
        # [Done] b. Is there a relationship/correlation between poor conditions and high return preferences?
        # [Done] c. Is there a relationship between duration in displacement and high return preferences?
        # [Done] d. How do these vary geographically?
# 3. Think of any other key indicators that could stimulate interesting discussion with SRP

rm(list = ls())

library(dplyr)
library(srvyr)
library(illuminate)


# read data ---------------------------------------------------------------

path <-"04_script/04_IDSF_policy_brief/hh_data_with_dsp_index.csv"
hh <-  read.csv(path,1)
# indv <-  openxlsx::read.xlsx(path,2)

# dsp_index <- read.csv("03_output/05_analysis/dsp_index_analysis.csv")
dsp_index_formaterd <- openxlsx::read.xlsx("00_output/2024_09_27_dsp_index_v2.xlsx")


# b. Is there a relationship/correlation between poor conditions and high return preferences?

hh$D_6_12mnth_plan |> unique()

hh <- hh |> mutate(
  return = case_when(D_6_12mnth_plan == "move_back_to_place_of_origin" ~ "Yes",
                      D_6_12mnth_plan == "99" ~ NA_character_,
                      T~ "No"),
  movement = case_when(
    D_6_12mnth_plan == "99" ~ NA_character_,
    D_6_12mnth_plan == "we_do_not_plan_to_move" ~ "No",
    T~"Yes"
  )
)


### correlation between DSP score vs movement

hh_idp <- hh |> filter(pop_group == "idp")

# Load necessary packages
library(dplyr)
library(ggplot2)

# Example: your dataframe is called df
# Remove NA in 'return' to simplify analysis
df_clean <- hh_idp %>% filter(!is.na(return))
df_clean$return <- factor(df_clean$return, levels = c("No", "Yes"))
ggplot(df_clean, aes(x = return, y = dsp_value)) +
  geom_boxplot() +
  labs(title = "DSP Score by Return Preference", x = "Return Preference", y = "DSP Score")

# Statistical test (e.g., Wilcoxon rank-sum test if not normally distributed)
test_result <- wilcox.test(dsp_value~ return, data = df_clean)
print(test_result)
# Statistical test (e.g., t.test as normally distributed)
t_test_result <- t.test(dsp_value ~ return, data = df_clean, var.equal = FALSE)  # Welch's t-test
print(t_test_result)

shapiro.test(df_clean$dsp_value[df_clean$return == "Yes"])
shapiro.test(df_clean$dsp_value[df_clean$return == "No"])


df_clean$dsp_value |> hist()

# RESULT:: NO CORRELATION FOUND

# s -----------------------------------------------------------------------

df_clean2 <- hh |>
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

  )  |> filter(month_duration > 0)

df_clean2$month_duration


shapiro.test(df_clean2$month_duration) ## normality
shapiro.test(df_clean2$dsp_value)  ## to test normality
cor.test(df_clean2$dsp_value, df_clean2$month_duration, method = "pearson",
         conf.level = .90)

#RESULT:::::: The result confirms a statistically significant but weak positive
# relationship between DSP score and duration since displacement.
# The longer people have been displaced, the slightly higher their
# DSP scores tend to be — but the relationship is not strong.


city_test <- list()

for(i in unique(df_clean2$City_Name)) {

  df <- df_clean2 |> filter(City_Name ==i)
  city_test[[i]] <- cor.test(df$dsp_value, df$month_duration, method = "pearson",
                             conf.level = .90)
}
### RESULT:: MogDaynile == week positive corelation ;
#### Doolow = moderate positive corelation
#### MogKahda,Baydhaba,Kismaayo,Berdaale,Xudur,Baardheere = no correlation



# [] a. We know in many cases conflict IDPs are more likely to want to return home.



# a. What were the main drivers of preventing this achievement/progress towards solutions?

Main drivers are
  1. Access to living standard [IDP - 58%; HC - 88% If the access to electricity can be improved  and reduced tthe barries  for IDP then we can improve ]
  2. Access to livelihood [IDP -27%, HC - 48% - FOCUS - access to financial institutaion and avg income must be improved]
  3. Access to documentation [IDP - 9%, HC- 32%- Legal indentity ]






