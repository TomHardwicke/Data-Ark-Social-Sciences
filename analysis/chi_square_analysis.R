library(tidyverse)

# field categories
dat <- data.frame(
  "med_no" = c(59,2,14),
  "med_yes" = c(51,5,15),
  row.names = c("Not available", "Restricted", "Unrestricted"),
  stringsAsFactors = FALSE
)
colnames(dat) <- c("Non-med", "Med")

fisher.test(dat)

# publication date categories
dat2 <- data.frame(
  "early" = c(55,0,14),
  "late" = c(55,7,15),
  row.names = c("Not available", "Restricted", "Unrestricted"),
  stringsAsFactors = FALSE
)

fisher.test(dat2)