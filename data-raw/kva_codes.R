# Load procedure codes from the Swedish National Board of Health and Welfare.

data_url <- 'https://www.socialstyrelsen.se/globalassets/sharepoint-dokument/dokument-webb/klassifikationer-och-koder/kva-inkl-beskrivningstexter.xlsx'

temp_file <- tempfile(fileext = '.xlsx')

data_url |>
  httr2::request() |>
  httr2::req_perform() |>
  httr2::resp_body_raw() |>
  writeBin(temp_file)

kva_codes <-
  temp_file |>
  readxl::read_xlsx(
    sheet = 2,
    range = readxl::cell_cols('B:C')
  )

# Extract codes indicating a dialysis catheter was placed.

kva_codes_dialysis_catheter <-
  kva_codes |>
  filter(
    stringr::str_detect(
      Titel,
      stringr::regex(
        '^(?!.*peritoneal)(?=.*dialys)(?=.*kateter)',
        ignore_case = TRUE
      )
    )
  ) |>
  pull(Kod)

# Extract codes indicating dialysis treatment.

kva_codes_dialysis_treatment <-
  kva_codes |>
  filter(
    stringr::str_detect(
      Titel,
      stringr::regex(
        '^(?!.*peritoneal)(?!.*kateter)(?!.*fistel)(?!.*lever)(?!.*cyklo)(?!.*CVK)(?=.*dialys)',
        ignore_case = TRUE
      )
    )
  ) |>
  pull(Kod)

# Store variables in package data.

usethis::use_data(kva_codes_dialysis_catheter, overwrite = TRUE)
usethis::use_data(kva_codes_dialysis_treatment, overwrite = TRUE)
