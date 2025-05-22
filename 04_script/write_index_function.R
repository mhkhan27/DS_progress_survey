

write_index_result <- function(dsp_neighbourhood,
                               dsp_intercity,
                               file_path){



  headerStyle <- createStyle(fontSize = 12,
                             fontColour = "black",
                             halign = "center",
                             valign = "center",
                             fontName ="Aptos",
                             textDecoration = "bold",
                             fgFill = "#428ce2",
                             border = "TopBottomLeftRight ",
                             borderColour = "#fafafa",
                             wrapText = T)


  core_indicator_style <- createStyle(fontSize = 12,
                                      fontColour = "black",
                                      halign = "center",
                                      valign = "center",
                                      fontName ="Aptos",
                                      textDecoration = "bold",
                                      fgFill = "#d7edf8",
                                      border = "TopBottomLeftRight ",
                                      borderColour = "#fafafa",
                                      wrapText = T)

  sub_indicator_style <- createStyle(fontSize = 12,
                                     fontColour = "black",
                                     halign = "center",
                                     valign = "center",
                                     fontName ="Aptos",
                                     textDecoration = "bold",
                                     fgFill = "#3fc0fc",
                                     border = "TopBottomLeftRight ",
                                     borderColour = "#fafafa",
                                     wrapText = T)




  iasc_indicator_style <- createStyle(fontSize = 12,
                                      fontColour = "white",
                                      halign = "center",
                                      valign = "center",
                                      fontName ="Aptos",
                                      textDecoration = "bold",
                                      fgFill = "#074866",
                                      border = "TopBottomLeftRight ",
                                      borderColour = "#fafafa",
                                      wrapText = T)

  dsp_style <- createStyle(fontSize = 12,
                           fontColour = "black",
                           halign = "center",
                           valign = "center",
                           fontName ="Aptos",
                           textDecoration = "bold",
                           fgFill = "#428ce2",
                           border = "TopBottomLeftRight ",
                           borderColour = "#fafafa",
                           wrapText = T)


  bodyStyle <- createStyle(fontSize = 11,
                           fontName = "Aptos",
                           border = "TopBottomLeftRight ",
                           borderColour = "#4F81BD",
                           valign = "center",
                           halign = "left")

  negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")



  wb <- createWorkbook()
  ######### sheet 1 ################

  core_indicator <- which(grepl("CI",names(dsp_neighbourhood)))
  sub_indicator <- which(grepl("IRIS-SC",names(dsp_neighbourhood)))
  iasc_indicator <- which(grepl("IASC",names(dsp_neighbourhood)))
  dsp_indicator <- which(grepl("dsp",names(dsp_neighbourhood)))
  core_col <-  which(c("pop_group","group_var","group_var_value") %in% names(dsp_neighbourhood))


  addWorksheet(wb,"dsp_index_neighbourhood")
  writeData(wb, sheet = 1,dsp_neighbourhood, rowNames = F)
  addFilter(wb,sheet =  1, rows = 1, cols = 1:5)
  freezePane(wb, sheet = 1, firstCol = TRUE, firstRow = T)

  addStyle(wb, sheet = 1,
           style = headerStyle,
           rows = 1,
           cols = 1:5,
           gridExpand = FALSE)

  addStyle(wb, sheet = 1,
           style = core_indicator_style,
           cols = core_indicator,
           rows = 0:nrow(dsp_neighbourhood)+1,
           gridExpand = TRUE)
  addStyle(wb, sheet = 1,
           style = dsp_style,
           cols = dsp_indicator,
           rows = 0:nrow(dsp_neighbourhood)+1,
           gridExpand = TRUE)
  addStyle(wb, sheet = 1,
           style = sub_indicator_style,
           cols = sub_indicator,
           rows = 0:nrow(dsp_neighbourhood)+1,
           gridExpand = TRUE)
  addStyle(wb, sheet = 1,
           style = iasc_indicator_style,
           cols = iasc_indicator,
           rows = 0:nrow(dsp_neighbourhood)+1,
           gridExpand = TRUE)

  setColWidths(wb, sheet = 1, cols = 1, widths = 20)


  ######### sheet 2 ################
  core_indicator_ic <- which(grepl("CI",names(dsp_intercity)))
  sub_indicator_ic <- which(grepl("IRIS-SC",names(dsp_intercity)))
  iasc_indicator_ic <- which(grepl("IASC",names(dsp_intercity)))
  dsp_indicator_ic <- which(grepl("dsp",names(dsp_intercity)))
  core_col_ic <-  which(c("pop_group","group_var","group_var_value") %in% names(dsp_intercity))


  addWorksheet(wb,"dsp_index_intercity")
  writeData(wb, sheet = 2,dsp_intercity, rowNames = F)
  addFilter(wb,sheet =  2, rows = 1, cols = 1:5)
  freezePane(wb, sheet = 2, firstCol = TRUE, firstRow = T)

  addStyle(wb, sheet = 2,
           style = headerStyle,
           rows = 1,
           cols = 1:5,
           gridExpand = FALSE)

  addStyle(wb, sheet = 2,
           style = core_indicator_style,
           cols = core_indicator_ic,
           rows = 0:nrow(dsp_intercity)+1,
           gridExpand = TRUE)
  addStyle(wb, sheet = 2,
           style = dsp_style,
           cols = dsp_indicator_ic,
           rows = 0:nrow(dsp_intercity)+1,
           gridExpand = TRUE)
  addStyle(wb, sheet = 2,
           style = sub_indicator_style,
           cols = sub_indicator_ic,
           rows = 0:nrow(dsp_intercity)+1,
           gridExpand = TRUE)
  addStyle(wb, sheet = 2,
           style = iasc_indicator_style,
           cols = iasc_indicator_ic,
           rows = 0:nrow(dsp_intercity)+1,
           gridExpand = TRUE)

  setColWidths(wb, sheet = 2, cols = 1, widths = 20)



  ######### write ################
  saveWorkbook(wb, file = file_path, overwrite = TRUE)
}
