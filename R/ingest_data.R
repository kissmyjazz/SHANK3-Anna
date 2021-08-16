library(tidyverse)

#  each `\` needs to be escaped once by another `\`, so UNC addresses are
# required to start with four \\\\-s

file_path <- "\\\\nimhlabstore-a.nimh.nih.gov\\lab\\SBN\\Chudasama\\Anna\\SHANK\\Trialwise Data Updated 2021-08-10"

test_file_path <- "\\\\nimhlabstore-a.nimh.nih.gov\\lab\\SBN\\Chudasama\\Anna\\SHANK\\test_sandbox"

ingest_data <- function(fpath, output_name = NULL) {
  if(!dir.exists(fpath)) {
    stop("Please provide the correct path to the files' directory")
  }

  # extract the list of filepaths
  flist <- list.files(fpath, pattern = "*.csv", full.names = TRUE)

  # list of file names
  fname <- purrr::map(flist, basename)

  # column names
  varnames <- c("skip", "skip2", "trial_nr", "correct_trial", "incorrect_trial",
                "omission_trial", "correction_trial", "cue_position_rewarded",
                "cue_position_nonrewarded", "initiation_touch_correct",
                "initiation_touch_incorrect", "initiation_touch_blank",
                "perseverative_touch_correct", "perseverative_touch_incorrect",
                "perseverative_touch_blank", "consume_touch_correct",
                "consume_touch_incorrect", "consume_touch_blank",
                "timeout_touch_correct", "timeout_touch_incorrect",
                "timeout_touch_blank", "iti_touch_correct", "iti_touch_incorrect",
                "iti_touch_blank", "response_latency",
                "reward_latency", "initiation_latency")

  df <- flist %>% purrr::map(read_csv, skip = 13,
                                       col_names = varnames,
                                       na = "",
                                       trim_ws = TRUE,
                             col_types = cols())  %>%
    purrr::set_names(fname) %>%
    dplyr::bind_rows(.id = "file_name") %>%
    dplyr::mutate(skip = NULL, skip2 = NULL,
                  fname = purrr::map(file_name, stringr::str_split_fixed,
                              pattern = "[_\\.]", n = 6),
                  date = purrr::map_chr(fname, 1) %>% as.Date(format = "%Y-%m-%d"),
                  animal_id = purrr::map_chr(fname, 2) %>% factor(),
                  sex = purrr::map_chr(fname, 3) %>% factor(),
                  genotype = purrr::map_chr(fname, 4) %>% factor(),
                  protocol = purrr::map_chr(fname, 5) %>% factor(),
                  response_latency = dplyr::na_if(response_latency, 0),
                  reward_latency = dplyr::na_if(reward_latency, 0),
                  initiation_latency = dplyr::na_if(initiation_latency, 0),
                  fname = NULL) %>%
    dplyr::select(date:protocol, everything()) %>%
    # filter out rows with lots of missing values
    dplyr::filter(!rowSums(is.na(.)) >= 20) %>%
    # filter out data where initiation latency is NA
    dplyr::filter(!is.na(initiation_latency))

  if(!is.null(output_name)) {
    dir.create(paste0(fpath, "\\joined_data"))
    Sys.chmod(list.dirs(fpath), "777")
    out_path <- paste0(fpath, "\\joined_data", paste0("\\", output_name, ".rds"))
    write_rds(df, out_path)
  }
  invisible(df)
}

df <- ingest_data(test_file_path, "test_data")
write_csv(df, "\\\\nimhlabstore-a.nimh.nih.gov\\lab\\SBN\\Chudasama\\Anna\\SHANK\\test_sandbox\\joined_data.csv")
