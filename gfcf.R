library(readxl)
library(tidyverse)

gfcf_path <- "C:/Users/55130748/Desktop/experimentalregionalgfcf19972020byassetandindustry.xlsx"

gfcf_sheets <- excel_sheets(gfcf_path)
gfcf_data_sheets <- gfcf_sheets[grepl("[0-9].[0-9]", gfcf_sheets)]

gfcf <- lapply(gfcf_data_sheets, function(sht) {
  read_excel(gfcf_path, sheet = sht, skip = 3, na = c("[w]", "[low]")) |>
    pivot_longer(cols = `1997`:`2020`, names_to = "date")
})

gfcf_final <- lapply(gfcf, function(x) {
  max_itl <- max(str_sub(names(x)[grepl("ITL[0-9]", names(x))], 4, 4))
  select(x, date, contains(paste0("ITL", max_itl)), contains("SIC07"), Asset, value) |>
    rename(geog_name = 2, geog_code = 3)
}) |>
  bind_rows()

write_csv(gfcf_final, "C:/Users/55130748/Desktop/gfcf.csv")
