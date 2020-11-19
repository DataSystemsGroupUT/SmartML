library(purrr)
library(stringr)
library(jsonlite)
library(devtools)


files <- dir(path <- "inst/extdata/hyperband_jsons", pattern = "*.json")
names_clf <- files %>%
  map_chr(~ str_remove(.x, pattern = ".json"))
paths <- file.path(path, files)
jsons <- paths %>%
  map(.f = ~ jsonlite::fromJSON(txt = .x, flatten = T))
names(jsons) <- names_clf

## Then:

save(jsons, file = "R/sysdata.rda")
save(jsons, file = "sysdata.rda")
load("sysdata.rda")
