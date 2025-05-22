
write_cleaning_log <- function(cleaning_log,file_path){

  headerStyle <- createStyle(fontSize = 12,
                             fontColour = "black",
                             halign = "center",
                             valign = "center",
                             fontName ="Arial Narrow",
                             textDecoration = "bold",
                             fgFill = "#428ce2",
                             border = "TopBottomLeftRight ",
                             borderColour = "#fafafa",
                             wrapText = T)

  bodyStyle <- createStyle(fontSize = 11,
                           fontName = "Arial Narrow",
                           border = "TopBottomLeftRight ",
                           borderColour = "#4F81BD",
                           valign = "center",
                           halign = "left")

  negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")



row_number <- which(!is.na(cleaning_log[["question_to_update"]]))+1
row_number_latrine <- which(cleaning_log[["question_to_update"]]=="K_7_latrin_type, K_8_ind_share_latrin")+1



  hide_tab <- data.frame(name = c("change_value", "blank_response","no_action","remove_survey"),
                         latrine = c("K_7_latrin_type","K_8_ind_share_latrin"))


  wb <- createWorkbook()
  addWorksheet(wb,"cleaning_log")
  addWorksheet(wb,"hide_tab")

  writeData(wb, sheet = 1,cleaning_log, rowNames = F)
  writeData(wb, sheet =2,hide_tab, rowNames = F)

  addFilter(wb,sheet =  1, rows = 1, cols = 1:ncol(cleaning_log))
  freezePane(wb, sheet = 1, firstCol = TRUE, firstRow = T)
  addStyle(wb, sheet = 1, style = headerStyle, rows = 1, cols = 1:ncol(cleaning_log), gridExpand = FALSE)

  addStyle(wb, sheet = 1, bodyStyle, rows = 2:nrow(cleaning_log)+1, cols = 1:ncol(cleaning_log), gridExpand = TRUE)

  dataValidation(wb, 1, col = 8, rows =1:nrow(cleaning_log)+1, type = "list",
                 value = "'hide_tab'!$A$2:$A$5")

  dataValidation(wb, 1, col = 5, rows =row_number_latrine, type = "list",
                 value = "'hide_tab'!$B$2:$B$3")




  conditionalFormatting(wb, sheet = 1,
                        cols = 7,
                        rows = 1:nrow(cleaning_log)+1, rule = 'AND($H2="change_value", $G2="")', style = negStyle)

  conditionalFormatting(wb, sheet = 1,
                        cols = 5,
                        rows =row_number, rule = paste0("AND($H",min(row_number),'="change_value", $E',min(row_number),'="")'), style = negStyle)

  setColWidths(wb, 1, cols = 1, widths = 12)
  setColWidths(wb, 1, cols = 2, widths = 30)
  setColWidths(wb, 1, cols = c(3:4), widths = 15)
  setColWidths(wb, 1, cols = c(5:6), widths = 30)
  setColWidths(wb, 1, cols = 7, widths = 25)
  setColWidths(wb, 1, cols = 8, widths = 20)
  setColWidths(wb, 1, cols = 9, widths = 50)
  setColWidths(wb, 1, cols = c(10:11), widths = 30)
  setColWidths(wb, 1, cols = 12,widths = 30)


  # setColWidths(wb, 1, cols = 1:ncol(cleaning_log), widths = 25)

  addStyle(wb, 1, rows = 1:nrow(cleaning_log)+1, cols = c(7:12),
           gridExpand = TRUE, style = createStyle(locked = FALSE,
                                                  fontSize = 11,
                                                  fontName = "Arial Narrow",
                                                  border = "TopBottomLeftRight ",
                                                  borderColour = "#4F81BD",
                                                  valign = "center",
                                                  halign = "left"))

  addStyle(wb, 1, rows = row_number, cols = c(5),
           gridExpand = TRUE, style = createStyle(locked = FALSE,
                                                  fontSize = 11,
                                                  fontName = "Arial Narrow",
                                                  border = "TopBottomLeftRight ",
                                                  borderColour = "#4F81BD",
                                                  valign = "center",
                                                  halign = "left"))





  protectWorksheet(wb, 1, protect = TRUE,lockAutoFilter = FALSE,
                   lockDeletingRows = TRUE,password = "nevermind")


  setRowHeights(wb, 1, 1, 20)
  sheetVisibility(wb)[2] <- FALSE



  saveWorkbook(wb, file = file_path, overwrite = TRUE)

}


# for_combine -------------------------------------------------------------


write_cleaning_log_v2 <- function(cleaning_log,deletion_log,file_path){

  headerStyle <- createStyle(fontSize = 12,
                             fontColour = "black",
                             halign = "center",
                             valign = "center",
                             fontName ="Arial Narrow",
                             textDecoration = "bold",
                             fgFill = "#428ce2",
                             border = "TopBottomLeftRight ",
                             borderColour = "#fafafa",
                             wrapText = T)

  bodyStyle <- createStyle(fontSize = 11,
                           fontName = "Arial Narrow",
                           border = "TopBottomLeftRight ",
                           borderColour = "#4F81BD",
                           valign = "center",
                           halign = "left")

  negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")



  # row_number <- which(!is.na(cleaning_log[["question_to_update"]]))+1
  # row_number_latrine <- which(cleaning_log[["question_to_update"]]=="K_7_latrin_type, K_8_ind_share_latrin")+1



  hide_tab <- data.frame(name = c("change_value", "blank_response","no_action","remove_survey"),
                         latrine = c("K_7_latrin_type","K_8_ind_share_latrin"))
  options("openxlsx.dateFormat" = "dd/mm/yyyy")

  wb <- createWorkbook()
  addWorksheet(wb,"cleaning_log")
  addWorksheet(wb,"deletion_log")

  addWorksheet(wb,"hide_tab")

  writeData(wb, sheet = 1,cleaning_log, rowNames = F)
  writeData(wb, sheet = 2,deletion_log, rowNames = F)
  writeData(wb, sheet =3,hide_tab, rowNames = F)

  addFilter(wb,sheet =  1, rows = 1, cols = 1:ncol(cleaning_log))
  addFilter(wb,sheet =  2, rows = 1, cols = 1:ncol(deletion_log))

  freezePane(wb, sheet = 1, firstCol = TRUE, firstRow = T)
  freezePane(wb, sheet = 2, firstCol = TRUE, firstRow = T)

  addStyle(wb, sheet = 1, style = headerStyle, rows = 1, cols = 1:ncol(cleaning_log), gridExpand = FALSE)
  addStyle(wb, sheet = 2, style = headerStyle, rows = 1, cols = 1:ncol(deletion_log), gridExpand = FALSE)


  addStyle(wb, sheet = 1, bodyStyle, rows = 1:nrow(cleaning_log)+1, cols = 1:ncol(cleaning_log), gridExpand = TRUE)
  addStyle(wb, sheet = 2, bodyStyle, rows = 1:nrow(deletion_log)+1, cols = 1:ncol(deletion_log), gridExpand = TRUE)


  dataValidation(wb, 1, col = 12, rows =1:nrow(cleaning_log)+1, type = "list",
                 value = "'hide_tab'!$A$2:$A$5")


  conditionalFormatting(wb, sheet = 1,
                        cols = 11,
                        rows = 1:nrow(cleaning_log)+1, rule = 'AND($L2="change_value", $K2="")', style = negStyle)
  conditionalFormatting(wb, sheet = 1,
                        cols = 12,
                        rows = 1:nrow(cleaning_log)+1, rule = 'AND($L2="")', style = negStyle)


  setColWidths(wb, 1, cols = 1, widths = 12)
  setColWidths(wb, 1, cols = 2:3, widths = 30)
  setColWidths(wb, 1, cols = c(4), widths = 35)
  setColWidths(wb, 1, cols = c(5), widths = 15)
  setColWidths(wb, 1, cols = c(6), widths = 12)

  setColWidths(wb, 1, cols = c(7:8), widths = 20)
  setColWidths(wb, 1, cols = 9, widths = 25)
  setColWidths(wb, 1, cols = 10, widths = 40)
  setColWidths(wb, 1, cols = 11, widths = 30)
  setColWidths(wb, 1, cols = c(12), widths = 20)
  setColWidths(wb, 1, cols = 13:14,widths = 50)

  # setColWidths(wb, 1, cols = 1:ncol(cleaning_log), widths = 25)
  setColWidths(wb, 2, cols = 1:ncol(deletion_log), widths = 25)

  addStyle(wb, 1, rows = 1:nrow(cleaning_log)+1, cols = c(1,3:8,10:14),
           gridExpand = TRUE, style = createStyle(locked = FALSE,
                                                  fontSize = 11,
                                                  fontName = "Arial Narrow",
                                                  border = "TopBottomLeftRight ",
                                                  borderColour = "#4F81BD",
                                                  valign = "center",
                                                  halign = "left"))




  protectWorksheet(wb, 1, protect = TRUE,lockAutoFilter = FALSE,
                   lockDeletingRows = TRUE,password = "nevermind")
  protectWorksheet(wb, 2, protect = TRUE,lockAutoFilter = FALSE,
                   lockDeletingRows = TRUE,password = "nevermind")


  setRowHeights(wb, 1, 1, 20)
  sheetVisibility(wb)[3] <- FALSE



  saveWorkbook(wb, file = file_path, overwrite = TRUE)

}







