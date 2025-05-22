# creating sub criteria ---------------------------------------------------

add_criteria_without_mix <- function(df, new_col_name,cols) {

  if(sum(!cols %in% names(df)) > 0 ){stop("col_not_found")}


  if(length(cols)==1){
    df2 <-  df |> mutate(
      !!sym(new_col_name):= !!sym(cols)) |>
      select(new_col_name,cols,everything())
  }

  if(length(cols)>1){
    df2 <-  df |>
      mutate(
        !!sym(new_col_name):= rowSums(df[cols],na.rm=T),
        rs = rowSums(!is.na(df[cols]),na.rm=T),
      ) |> mutate(
        !!sym(new_col_name):= case_when(!!sym(new_col_name)/rs > .49 ~ 1,
                                        !!sym(new_col_name)/rs <.50 ~ 0)
      )  |> select(-rs) |>
      select(new_col_name,cols,everything())
  }
  return(df2)
}


# with mix ----------------------------------------------------------------
add_criteria_with_mix <- function(df, new_col_name,cols,critical_in) {

  if(sum(!cols %in% names(df)) > 0 ){stop("col_not_found")}
  if(sum(!critical_in %in% names(df)) > 0 ){stop("critical_ind_col_not_found")}


  non_cri <- cols[!cols%in%critical_in]



  df |> mutate(
    total_in_non_na = rowSums(!is.na(df[cols]),na.rm =T),
    number_of_critical = rowSums(!is.na(df[critical_in]),na.rm =T),
    number_of_noncritical = rowSums(!is.na(df[non_cri]),na.rm =T),
    number_of_critical_2x = rowSums(!is.na(df[critical_in]),na.rm =T)*2,
    equal_weights_total = 1/( number_of_critical_2x+ number_of_noncritical)
  ) |> mutate(across(non_cri,~.x* equal_weights_total)) |>
    mutate(across(critical_in,~.x* equal_weights_total*2)) |>
    rowwise(X_uuid) |>
    mutate(  !!sym(new_col_name):=sum(c_across(all_of(cols)),na.rm=T),
             !!sym(new_col_name):= case_when(!!sym(new_col_name) >.49 ~1 ,
                                             !!sym(new_col_name) < .50~0)) |>
    ungroup() |>
    select(new_col_name,cols,everything()) |>
    select(-total_in_non_na,
           -number_of_critical,
           -number_of_noncritical,
           -number_of_critical_2x,
           -equal_weights_total)


}


