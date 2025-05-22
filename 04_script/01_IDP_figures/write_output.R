

write_protracted <- function(analysis_hh,analysis_indv,file_path){

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

  bodyStyle <- createStyle(fontSize = 11,
                           fontName = "Aptos",
                           border = "TopBottomLeftRight ",
                           borderColour = "#4F81BD",
                           valign = "center",
                           halign = "left")

  negStyle <- openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")


  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb,"IDP-HH")
  openxlsx::addWorksheet(wb,"IDP-INDV")
  openxlsx::writeData(wb, sheet = 1,analysis_hh, rowNames = F)
  openxlsx::writeData(wb, sheet = 2,analysis_indv, rowNames = F)

  freezePane(wb, sheet = 1, firstActiveRow = 5,firstActiveCol = "E")
  freezePane(wb, sheet = 2, firstActiveRow = 5,firstActiveCol = "E")

  addStyle(wb, sheet = 1, style = headerStyle, rows = 2:4, cols = 1:ncol(analysis_hh), gridExpand = T)
  addStyle(wb, sheet = 1, bodyStyle, rows = 4:nrow(analysis_hh)+1, cols = 1:ncol(analysis_hh), gridExpand = TRUE)
  addFilter(wb,sheet =  1, row = 4, cols = 1:5)

  addStyle(wb, sheet = 2, style = headerStyle, rows = 2:4, cols = 1:ncol(analysis_indv), gridExpand = T)
  addStyle(wb, sheet = 2, bodyStyle, rows = 4:nrow(analysis_indv)+1, cols = 1:ncol(analysis_indv), gridExpand = TRUE)
  addFilter(wb,sheet =  2, row = 4, cols = 1:5)


  merger_cols <- analysis_hh[1,] |> as.character()

  for(i in unique(merger_cols)){
    print(i)
    to_merge <- which(merger_cols == i)
    if(length(to_merge) > 1){
    print(to_merge)
    openxlsx::mergeCells(wb,sheet = 1,cols = to_merge,rows = 2)
    openxlsx::mergeCells(wb,sheet = 2,cols = to_merge,rows = 2)

    }
  }

  merger_cols_2 <- analysis_hh[2,] |> as.character()

  for(i in unique(merger_cols_2)){
    print(i)
    to_merge <- which(merger_cols_2 == i)
    if(length(to_merge) > 1){
    print(to_merge)
    openxlsx::mergeCells(wb,sheet = 1,cols = to_merge,rows = 3)
    openxlsx::mergeCells(wb,sheet = 2,cols = to_merge,rows = 3)

    }
}
  openxlsx::setColWidths(wb, 1, cols = 1:5, widths = 18)
  openxlsx::setColWidths(wb, 1, cols = 6:ncol(analysis_hh), widths = 25)
  openxlsx::deleteData(wb, sheet = 1, cols = 1:ncol(analysis_hh), rows = 1, gridExpand = TRUE)

  openxlsx::setColWidths(wb, 2, cols = 1:5, widths = 18)
  openxlsx::setColWidths(wb, 2, cols = 6:ncol(analysis_indv), widths = 25)
  openxlsx::deleteData(wb, sheet = 2, cols = 1:ncol(analysis_indv), rows = 1, gridExpand = TRUE)


  for(i in 1:5){
    openxlsx::mergeCells(wb,sheet = 1,cols = i,rows = 2:4)
    openxlsx::mergeCells(wb,sheet = 2,cols = i,rows = 2:4)

    }

  for(i in 6:9){
    openxlsx::mergeCells(wb,sheet = 1,cols = i,rows = 3:4)
    openxlsx::mergeCells(wb,sheet = 2,cols = i,rows = 3:4)


  }




  openxlsx::saveWorkbook(wb, file = file_path, overwrite = TRUE)

}
