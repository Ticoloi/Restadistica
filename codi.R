library(data.table)
library(tidyverse)
library(simmer)
library(simmer.plot)

data3<- fread("aigua.csv")
#save.image(file="aigua.RData")

load("~/Documents/Programacio/R/water/aigua.RData")
# Save the city object
#saveRDS(data3, "aigua.rdata"


summarise(data3,
          any = any(),
          n.na = sum(is.na(resource)))
