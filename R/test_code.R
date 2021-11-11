# extract the list of filepaths
fpath <- "\\\\nimhlabstore-a.nimh.nih.gov\\lab\\SBN\\Chudasama\\Anna\\SHANK\\test_sandbox"
file_path <- "\\\\nimhlabstore-a.nimh.nih.gov\\lab\\SBN\\Chudasama\\Anna\\SHANK\\Trialwise Updated 2021-10-08"


flist <- list.files(file_path, pattern = "*.csv", full.names = TRUE)

# list of file names
fname <- purrr::map(flist, basename)

# column names for a scipt with changing pairs of stimuli
varnames_step <- c("skip", "skip2", "trial_nr", "correct_trial", "incorrect_trial",
              "omission_trial", "correction_trial", "cue_position_rewarded",
              "cue_position_nonrewarded", "initiation_touch_correct",
              "initiation_touch_incorrect", "initiation_touch_blank",
              "perseverative_touch_correct", "perseverative_touch_incorrect",
              "perseverative_touch_blank", "consume_touch_correct",
              "consume_touch_incorrect", "consume_touch_blank",
              "timeout_touch_correct", "timeout_touch_incorrect",
              "timeout_touch_blank", "iti_touch_correct", "iti_touch_incorrect",
              "iti_touch_blank", "response_latency",
              "reward_latency", "pair", "initiation_latency")

# column names for a previous version of the script
varnames_old <- varnames_step[-27]

# partial functions of read_csv and str_detect to read in the proper number
# of columns depending on the script

read_csv_step <- purrr::partial(readr::read_csv, col_names = varnames_step)
read_csv_old <- purrr::partial(readr::read_csv, col_names = varnames_old)
detect_step <- purrr::partial(stringr::str_detect, pattern = "_step")
add_pair <- purrr::partial(tibble::add_column, pair = NA,
                          .before = "initiation_latency")

# Logical vector testing for the presence of "_step" in filenames
detected_step <- detect_step(fname)

df <- flist %>% purrr::map_if(detected_step,
                           read_csv_step,
                           skip = 13,
                           na = "",
                           trim_ws = TRUE,
                           col_types = cols(),
                           .else = read_csv_old) %>%
  purrr::map_if(!detected_step, add_pair) %>%
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
                fname = NULL,
                pair = dplyr::case_when(protocol == "old-SD" ~ 1,
                                        protocol == "new-SD" |
                                        protocol == "reversal" |
                                        protocol ==  "space" ~ 2,
                                        TRUE ~ pair),
                pair = as.integer(pair))

