library(tidyverse)

#  each `\` needs to be escaped once by another `\`, so UNC addresses are
# required to start with four \\\\-s

file_path <- "\\\\nimhlabstore-a.nimh.nih.gov\\lab\\SBN\\Chudasama\\Anna\\SHANK\\Trialwise Data Updated 2021-08-10"

ingest_data <- function(fpath, output_name = NULL) {
  if(!dir.exists(fpath)) {
    stop("Please provide the correct path to the files' directory")
  }

  # extract the list of filepaths
  flist <- list.files(file_path, pattern = "*.csv", full.names = TRUE)

  # list of file names
  fname <- map(flist, basename)

  # column names
  varnames <- c("skip", "trial_nr", "correct_trial", "incorrect_trial",
                "omission_trial", "correction_trial", "cue_position_rewarded",
                "cue_position_nonrewarded", "initiation_touch_correct",
                "initiation_touch_incorrect", "initiation_touch_blank",
                "perseverative_touch_correct", "perseverative_touch_incorrect",
                "perseverative_touch_blank", "consume_touch_correct",
                "consume_touch_incorrect", "consume_touch_blank",
                "timeout_touch_correct", "timeout_touch_incorrect",
                "timeout_touch_blank", "iti_touch_correct", "iti_touch_incorrect",
                "iti_touch_blank", "initiation_latency", "response_latency",
                'reward_latency')

  df <- flist %>% purrr::map(read_csv, skip = 13,
                                       col_names = varnames,
                                       na = "",
                                       trim_ws = TRUE,
                             col_types = cols())  %>%
    set_names(fname) %>%
    bind_rows(.id = "file_name") %>%
    dplyr::mutate(skip = NULL,
                  fname = map(file_name, stringr::str_split_fixed,
                              pattern = "[_\\.]", n = 6),
                  date = map_chr(fname, 1) %>% as.Date(format = "%Y-%m-%d"),
                  animal_id = map_chr(fname, 2) %>% factor(),
                  sex = map_chr(fname, 3) %>% factor(),
                  genotype = map_chr(fname, 4) %>% factor(),
                  protocol = map_chr(fname, 5) %>% factor(),
                  fname = NULL) %>%
    dplyr::select(date:protocol, everything()) %>%
    # filter out rows with lots of missing values
    dplyr::filter(!rowSums(is.na(.)) >= 20)
  if(!is.null(output_name)) {
    out_path <- paste0(fpath, paste0("\\", output_name, ".rds"))
    write_rds(df, out_path)
  }
  invisible(df)
}

df <- ingest_data(file_path, "all_data")
