rm(list = ls())
library(tidyverse)
library(openxlsx)

hh_data <- read_xlsx("01_input/02_raw_data/01_version_1/data_version_1.xlsx",1,guess_max = Inf) |> select_if(function(x) !all(is.na(x)))
indv_data <- read_xlsx("01_input/02_raw_data/01_version_1/data_version_1.xlsx",3,guess_max = Inf) |> select_if(function(x) !all(is.na(x)))


# dealing with column name ------------------------------------------------

survey <-  read_xlsx("01_input/02_raw_data/01_version_1/kobo_version_1.xlsx",1,guess_max = Inf)
choice <-  read_xlsx("01_input/02_raw_data/01_version_1/kobo_version_1.xlsx",2,guess_max = Inf)



lookup_table <- survey |> select(type,name, label = `label::english` ) |> filter(
  !is.na(label)
)

##### select_one chaning
lookup_table_select_one <- lookup_table |> filter(
  grepl("select_one|text|integer",type )
) |> select(col_name_lable = label,
            xml_label = name)



# select_multiple ---------------------------------------------------------


choice_list <- choice |> select(list_name,name_choice=name,choice_label=`label::english`)

lookup_table_select_multiple <- lookup_table |> filter(
  grepl("select_mu",type )
) |> mutate(
  list_name = str_replace_all(type, "select_multiple ","")
) |> left_join(choice_list,multiple = "all" ) |> mutate(
  col_name_lable = paste0(label,"/",choice_label),
  xml_label = paste0(name,".",name_choice)
) |> select(col_name_lable,xml_label)



lookup_table <-lookup_table_select_one |> bind_rows(lookup_table_select_multiple)

######### fixing_dataset version

df_name <- data.frame(col_name = names(hh_data)) |>
  filter(!col_name %in% lookup_table$col_name_lable) |>
  filter(grepl(" ",col_name)) |>
  filter(!grepl("Note to the enumerator",col_name)) |> mutate(
    main_col = gsub("\\.\\.\\..*","",col_name),
    number = gsub(".*\\.\\.\\.","",col_name) |> as.numeric(),
  )  |> group_by(main_col) |> mutate(
    max = max(number,na.rm = T)
  )



# |> mutate(
#   check = main_col %in%lookup_table$col_name_lable
# ) |> filter(!check)





