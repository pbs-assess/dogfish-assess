library(dplyr)
library(gfplot)

if (Sys.info()[["user"]] == "seananderson") {
  d <- readRDS("data/raw/catch.rds")
  table(d$major_stat_area_name)
  d_4b5abcde <- tidy_catch(d, areas = c("5[CDE]+", "5[AB]+", "4B"))
  saveRDS(d_4b5abcde, file = "data/generated/catch-4B5ABCDE-summarized.rds")
}

# outside: 5AB + 5CDE
# inside: 4B

# cached and force-pushed to GitHub:
d_4b5abcde <- readRDS("data/generated/catch-4B5ABCDE-summarized.rds")
plot_catch(d_4b5abcde)
