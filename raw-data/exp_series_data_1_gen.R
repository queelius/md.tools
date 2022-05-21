library(readr)
library(usethis)
library(md.tools)

exp_series_regular_cand_model <- md.tools::md_read_csv_with_meta("./raw-data/exp_series_regular_cand_model.csv")
usethis::use_data(exp_series_regular_cand_model,overwrite = TRUE)

