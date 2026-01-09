library(data.table)
library(tidyverse)
library(simmer)
library(simmer.plot)
library(dplyr)

# Lectura dades ----------------------------------------------------------

consum_aigua <- fread("taules/aigua.csv")
#https://analisi.transparenciacatalunya.cat/Medi-Ambient/Consum-d-aigua-a-Catalunya-per-comarques/2gws-ubmt/about_data

# Eliminem punts dels milers i convertim a número
#Importantt!!!!
consum_aigua$'Domèstic xarxa' <- as.numeric(gsub("\\.", "", consum_aigua$'Domèstic xarxa'))
consum_aigua$`Activitats econòmiques i fonts pròpies` <- as.numeric(gsub("\\.", "", consum_aigua$`Activitats econòmiques i fonts pròpies`))
consum_aigua$`Població` <- as.numeric(gsub("\\.", "", consum_aigua$`Població`))
consum_aigua$`Total` <- as.numeric(gsub("\\.", "", consum_aigua$`Total`))


# comarques: 

comarques <- consum_aigua %>%
  dplyr::distinct(`Comarca`)

n_comarques = (nrow(comarques))
noms_comarques <- comarques$Comarca

# llista regresuio lineal

regresio_lineal <- vector("list", n_comarques)
names(regresio_lineal) <- noms_comarques

# regresio lineal per cada comarca
for (i in seq_along(noms_comarques)) {
  
  comarca_i <- noms_comarques[i]
  
  dades_comarca <- consum_aigua %>%
    filter(Comarca == comarca_i)
  
  regresio_lineal[[i]] <- lm(
    `Total` ~ Any,
    data = dades_comarca
  )
}

# ara s'ha de trobar el punt crític
taus <- seq(min(consum_aigua$Any), max(consum_aigua$Any), 1)

punt_optim <- vector("list", n_comarques)
names(punt_optim) <- noms_comarques

model_amb_tau <- vector("list", n_comarques)
names(model_amb_tau) <- noms_comarques

# CORRECCIÓ: Càlcul correcte dels models amb tau per comarca
for (i in seq_along(noms_comarques)) {
  comarca_i <- noms_comarques[i]
  dades_comarca <- consum_aigua %>% filter(Comarca == comarca_i)
  
  sse <- sapply(taus, function(tau) {
    m <- lm(Total ~ Any + I(pmax(0, Any - tau)), data = dades_comarca)  # CANVI AQUÍ
    sum(residuals(m)^2)
  })
  
  punt_optim[[i]] <- taus[which.min(sse)]
  model_amb_tau[[i]] <- lm(Total ~ Any + I(pmax(0, Any - punt_optim[[i]])), 
                           data = dades_comarca)  # CANVI AQUÍ
}