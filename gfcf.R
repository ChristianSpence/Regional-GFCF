# https://www.ons.gov.uk/economy/regionalaccounts/grossdisposablehouseholdincome/articles/experimentalregionalgrossfixedcapitalformationgfcfestimatesbyassettype1997to2020/2022-05-10

gfcf_path <- "data-raw/experimentalregionalgfcf19972020byassetandindustry.xlsx"

gfcf_sheets <- readxl::excel_sheets(gfcf_path)
gfcf_data_sheets <- gfcf_sheets[grepl("[0-9].[0-9]", gfcf_sheets)]

gfcf <- lapply(gfcf_data_sheets, function(sht) {
  readxl::read_excel(gfcf_path,
                     sheet = sht,
                     skip = 3,
                     na = c("[w]", "[low]")) |>
    tidyr::pivot_longer(cols = `1997`:`2020`, names_to = "date") |>
    dplyr::filter(!is.na(Asset)) # Remove empty rows from data sheet 2
})

gfcf_final <- lapply(gfcf, function(x) {
  max_itl <- max(stringr::str_sub(names(x)[grepl("ITL[0-9]", names(x))], 4, 4))
  dplyr::select(x,
                date,
                dplyr::contains(paste0("ITL", max_itl)),
                dplyr::contains("SIC07"), Asset, value) |>
    dplyr::rename(geog_name = 2, geog_code = 3)
}) |>
  dplyr::bind_rows()
