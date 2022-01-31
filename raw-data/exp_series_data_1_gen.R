# series system with m=3 nodes, each exponentially distributed
# system parameter value: rate=(3,4,5)
# sample size: n=1000
# candidate model m0
# each candidate set has w=2 candidates
# starting seed is 123, so it will always produce the same output

library(readr)
library(usethis)

data_sets <- masked.data::md_read_json("./exp_series_data.json")

# only generate data for the first data set
usethis::use_data(data_sets[[1]], overwrite = TRUE)

