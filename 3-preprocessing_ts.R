
install.packages("tidyverse")

library(data.table)
library(dplyr)
library(tidyverse)
library(readr)

# Read raw S1 GEE ts
s1_raw <- data.table(read_csv('output/gee/Ethiopia_refl_s1_pol.csv'))
l8_raw <- data.table(read_csv('output/gee/Ethiopia_refl_l8_pol.csv'))

# Average S1 ts on plot and date
s1_ts <- s1_raw[, .(VV=mean(VV)), by=list(plotID, date)]

# Merge L8 dt
l8_raw <- l8_raw[,c('date','SR_B4','SR_B5','SR_B6','QA_PIXEL','plotID')]

# Write averaged ts as csv
fwrite(s1_ts, "output/gee/Ethiopia_s1_refl.csv")
fwrite(l8_raw, "output/gee/Ethiopia_l8_refl_pol.csv")

