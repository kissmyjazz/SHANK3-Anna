library(tidyverse)

extract_weights <- function(fpath) {
  df <- readxl::read_excel(fpath, sheet = "Weight Log", na = c("", "N/A"),
                   trim_ws = TRUE) %>%
    dplyr::rename(animal_id = ...1) %>%
    dplyr::filter(stringr::str_starts(animal_id, "[0-9]")) %>%
    dplyr::mutate_at(vars(-animal_id), as.integer) %>%
    tidyr::pivot_longer(-animal_id, names_to = "date", values_to = "weight",
                        values_drop_na = TRUE,
                        names_transform = list(date = as.numeric)) %>%
    dplyr::mutate(date = lubridate::as_date(date, origin = "1899-12-30"),
           animal_id = substr(animal_id, 3, 6))

  invisible(df)
}

# test
file_path <- "C:\\Users\\matrovd2\\Downloads\\SHANK Summary Data - Liquid.xlsx"

df <- extract_weights(file_path)