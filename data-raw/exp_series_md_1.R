library(readr)
library(usethis)
library(md.tools)

exp_series_md_1 <- md.tools::md_read_csv_with_meta("./raw-data/exp_series_md_1.csv")
usethis::use_data(exp_series_md_1,overwrite = TRUE)

