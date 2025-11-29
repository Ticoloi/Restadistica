library(data.table)
library(tidyverse)
library(simmer)
library(simmer.plot)


data3<- fread("aigua.csv")
#save.image(file="aigua.RData")
#load("~/Documents/Programacio/R/water/aigua.RData")
# Save the city object
#saveRDS(data3, "aigua.rdata")

glimpse(data3)
summarise(data3,
          n = n(),
          n.na = sum(is.na(`Codi comarca`)))

summarise(data3,
          n.cat = n_distinct(`Codi comarca`))

recompte = count(data3, `Codi comarca`)
arrange(recompte, desc(n))

x = pull(arrange(recompte, desc(n)), n, name = `Codi comarca`)
barplot(x, las = 2, cex.names = 0.7)
