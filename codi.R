library(data.table)
library(tidyverse)
library(simmer)
library(simmer.plot)
library(dplyr)

  # Lectura dades ----------------------------------------------------------

consum_aigua<- fread("aigua.csv")
#https://analisi.transparenciacatalunya.cat/Economia/Producte-interior-brut-territorial/ung3-i3j2/about_data
pib_provincia<- fread("pib_provincia.csv")

# Eliminem punts dels milers i convertim a número
#Importantt!!!!
consum_aigua$'Domèstic xarxa' <- as.numeric(gsub("\\.", "", consum_aigua$'Domèstic xarxa'))
consum_aigua$`Activitats econòmiques i fonts pròpies` <- as.numeric(gsub("\\.", "", consum_aigua$`Activitats econòmiques i fonts pròpies`))
consum_aigua$`Població` <- as.numeric(gsub("\\.", "", consum_aigua$`Població`))
# Mostrar barres ----------------------------------------------------------

comarques_girona <- c(
  "ALT EMPORDÀ, L'", "BAIX EMPORDÀ, EL", "GIRONÈS, EL", "SELVA, LA",
  "RIPOLLÈS, EL", "GARROTXA, LA", "PLA DE L'ESTANY, EL"
)

consum_total_girona <- consum_aigua %>%
  filter(Comarca %in% comarques_girona) %>%
  group_by(Any) %>%
  summarise(
    Consum_total = sum(`Domèstic xarxa` + `Activitats econòmiques i fonts pròpies`, na.rm = TRUE),
    Domèstic_total = sum(`Domèstic xarxa`, na.rm = TRUE),
    Activitats_total = sum(`Activitats econòmiques i fonts pròpies`, na.rm = TRUE), Població  = sum(`Població`, na.rm = TRUE)
  )

pib_girona <- pib_provincia %>%
  filter(`àmbit territorial de planificació` == "Comarques Gironines" ) %>%
  rename(Total = `branques d'activitat`) %>% # canvia el nom de la columna
  filter(Total == "total") %>%
  rename(Any = any) %>% # canvia el nom de la columna
  rename(PIB = valor) %>%
  dplyr::select(Any, PIB, `àmbit territorial de planificació`) %>%
  group_by(Any)

# PIV i aigua a alt_pirineu_i_aran<s (WIP) ----------------------------------------------------------

comarques_alt_pirineu_i_aran <- c(
  "CERDANYA, LA", "VAL D'ARAN, LA", "PALLARS JUSSÀ, EL", "PALLARS SOBIRÀ, EL", "ALT URGELL, L'",
  "ALTA RIBAGORÇA, L'"

)

consum_total_alt_pirineu_i_aran<- consum_aigua %>%
  filter(Comarca %in% comarques_alt_pirineu_i_aran) %>%
  group_by(Any) %>%
  summarise(
    Consum_total = sum(`Domèstic xarxa` + `Activitats econòmiques i fonts pròpies`, na.rm = TRUE),
    Domèstic_total = sum(`Domèstic xarxa`, na.rm = TRUE),
    Activitats_total = sum(`Activitats econòmiques i fonts pròpies`, na.rm = TRUE), Població  = sum(`Població`, na.rm = TRUE)
  )

pib_alt_pirineu_i_aran <- pib_provincia %>%
  filter(`àmbit territorial de planificació` == "Alt Pirineu i Aran") %>%
  rename(Total = `branques d'activitat`) %>% # canvia el nom de la columna
  filter(Total == "total") %>%
  rename(Any = any) %>% # canvia el nom de la columna
  rename(PIB = valor) %>%
  dplyr::select(Any, PIB, `àmbit territorial de planificació`) %>%
  group_by(Any)



# PIV i aigua a Ponent (WIP) ----------------------------------------------------------

comarques_ponent <- c(
  "GARRIGUES, LES", "NOGUERA, LA", "PLA D'URGELL, EL", "SEGARRA, LA",
  "SEGRIÀ, EL", "URGELL, L'"
)

consum_total_ponent<- consum_aigua %>%
  filter(Comarca %in% comarques_ponent) %>%
  group_by(Any) %>%
  summarise(
    Consum_total = sum(`Domèstic xarxa` + `Activitats econòmiques i fonts pròpies`, na.rm = TRUE),
    Domèstic_total = sum(`Domèstic xarxa`, na.rm = TRUE),
    Activitats_total = sum(`Activitats econòmiques i fonts pròpies`, na.rm = TRUE), Població  = sum(`Població`, na.rm = TRUE)
  )

pib_ponent <- pib_provincia %>%
  filter(`àmbit territorial de planificació` == "Ponent") %>%
  rename(Total = `branques d'activitat`) %>% # canvia el nom de la columna
  filter(Total == "total") %>%
  rename(Any = any) %>% # canvia el nom de la columna
  rename(PIB = valor) %>%
  dplyr::select(Any, PIB, `àmbit territorial de planificació`) %>%
  group_by(Any)


#PIB i aigua a Terres de l'Ebre ----------------------------------------------------------

comarques_terres_ebre <- c(
  "BAIX EBRE, EL", "MONTSIÀ, EL", "RIBERA D'EBRE, LA", "TERRA ALTA"

)

consum_total_terres_ebre<- consum_aigua %>%
  filter(Comarca %in% comarques_terres_ebre) %>%
  group_by(Any) %>%
  summarise(
    Consum_total = sum(`Domèstic xarxa` + `Activitats econòmiques i fonts pròpies`, na.rm = TRUE),
    Domèstic_total = sum(`Domèstic xarxa`, na.rm = TRUE),
    Activitats_total = sum(`Activitats econòmiques i fonts pròpies`, na.rm = TRUE), Població  = sum(`Població`, na.rm = TRUE)
  )

pib_terres_ebre <- pib_provincia %>%
  filter(`àmbit territorial de planificació` == "Terres de l'Ebre") %>%
  rename(Total = `branques d'activitat`) %>%  #canvia el nom de la columna
  filter(Total == "total") %>%
  rename(Any = any) %>% # canvia el nom de la columna
  rename(PIB = valor) %>%
  dplyr::select(Any, PIB, `àmbit territorial de planificació`) %>%
  group_by(Any)

# Camp de Tarragona ----------------------------------------------------------

comarques_camp_tarragona <- c(
  "ALT CAMP, L'", "BAIX CAMP, EL", "CONCA DE BARBERÀ, LA", "PRIORAT, EL",
  "TARRAGONÈS, EL", "BAIX PENEDÈS, EL"
)


consum_total_camp_tarragona <- consum_aigua %>%
  filter(Comarca %in% comarques_camp_tarragona) %>%
  group_by(Any) %>%
  summarise(
    Consum_total = sum(`Domèstic xarxa` + `Activitats econòmiques i fonts pròpies`, na.rm = TRUE),
    Domèstic_total = sum(`Domèstic xarxa`, na.rm = TRUE),
    Activitats_total = sum(`Activitats econòmiques i fonts pròpies`, na.rm = TRUE), Població  = sum(`Població`, na.rm = TRUE)
  )

pib_camp_tarragona <- pib_provincia %>%
  filter(`àmbit territorial de planificació` == "Camp de Tarragona") %>%
  rename(Total = `branques d'activitat`) %>%
  filter(Total == "total") %>%
  rename(Any = any) %>%
  rename(PIB = valor) %>%
  dplyr::select(Any, PIB, `àmbit territorial de planificació`) %>%
  group_by(Any)


# Catalunya Central ----------------------------------------------------------

comarques_catalunya_central <- c(
  "ANOIA, L'", "BAGES, EL", "BERGUEDÀ, EL", "SOLSONÈS, EL",
  "OSONA", "Moianès", "LLUÇANÈS, EL"
)

consum_total_catalunya_central <- consum_aigua %>%
  filter(Comarca %in% comarques_catalunya_central) %>%
  group_by(Any) %>%
  summarise(
    Consum_total = sum(`Domèstic xarxa` + `Activitats econòmiques i fonts pròpies`, na.rm = TRUE),
    Domèstic_total = sum(`Domèstic xarxa`, na.rm = TRUE),
    Activitats_total = sum(`Activitats econòmiques i fonts pròpies`, na.rm = TRUE), Població  = sum(`Població`, na.rm = TRUE)
  )

pib_catalunya_central <- pib_provincia %>%
  filter(`àmbit territorial de planificació` == "Comarques Centrals") %>%
  rename(Total = `branques d'activitat`) %>%
  filter(Total == "total") %>%
  rename(Any = any) %>%
  rename(PIB = valor) %>%
  dplyr::select(Any, PIB, `àmbit territorial de planificació`) %>%
  group_by(Any)


# Barcelona Metropolitana ----------------------------------------------------------

comarques_metropolita <- c(
  "BARCELONÈS, EL", "BAIX LLOBREGAT, EL", "GARRAF, EL", "ALT PENEDÈS, L'",
  "MARESME, EL", "VALLÈS OCCIDENTAL, EL", "VALLÈS ORIENTAL, EL"
)


consum_total_metropolita <- consum_aigua %>%
  filter(Comarca %in% comarques_metropolita) %>%
  group_by(Any) %>%
  summarise(
    Consum_total = sum(`Domèstic xarxa` + `Activitats econòmiques i fonts pròpies`, na.rm = TRUE),
    Domèstic_total = sum(`Domèstic xarxa`, na.rm = TRUE),
    Activitats_total = sum(`Activitats econòmiques i fonts pròpies`, na.rm = TRUE), Població  = sum(`Població`, na.rm = TRUE)
  )

pib_metropolita <- pib_provincia %>%
  filter(`àmbit territorial de planificació` == "Metropolità") %>%
  rename(Total = `branques d'activitat`) %>%
  filter(Total == "total") %>%
  rename(Any = any) %>%
  rename(PIB = valor) %>%
  dplyr::select(Any, PIB, `àmbit territorial de planificació`) %>%
  group_by(Any)

# PONENT ----------------------------------------------------------

comarques_ponent <- c(
  "GARRIGUES, LES", "NOGUERA, LA", "PLA D'URGELL, EL", "SEGARRA, LA",
  "SEGRIÀ, EL", "URGELL, L'"
)

consum_total_ponent <- consum_aigua %>%
  filter(Comarca %in% comarques_ponent) %>%
  group_by(Any) %>%
  summarise(
    Consum_total = sum(`Domèstic xarxa` + `Activitats econòmiques i fonts pròpies`, na.rm = TRUE),
    Domèstic_total = sum(`Domèstic xarxa`, na.rm = TRUE),
    Activitats_total = sum(`Activitats econòmiques i fonts pròpies`, na.rm = TRUE), Població  = sum(`Població`, na.rm = TRUE)
  )


pib_ponent <- pib_provincia %>%
  filter(`àmbit territorial de planificació` == "Ponent") %>%
  rename(Total = `branques d'activitat`) %>%
  filter(Total == "total") %>%
  rename(Any = any) %>%
  rename(PIB = valor) %>%
  dplyr::select(Any, PIB, `àmbit territorial de planificació`) %>%
  group_by(Any)


# Taula amb totes les regions ----------------------------------------------------------

taula_resultat_completa <- bind_rows(
  taula_girona = consum_total_girona %>% inner_join(pib_girona, by = "Any"),
  taula_alt_pirineu_i_aran = consum_total_alt_pirineu_i_aran %>% inner_join(pib_alt_pirineu_i_aran, by = "Any"),
  taula_ponent = consum_total_ponent %>% inner_join(pib_ponent, by = "Any"),
  taula_ebre = consum_total_terres_ebre %>% inner_join(pib_terres_ebre, by = "Any"),
  taula_camp_tarragona = consum_total_camp_tarragona %>% inner_join(pib_camp_tarragona, by = "Any"),
  taula_catcentral = consum_total_catalunya_central %>% inner_join(pib_catalunya_central, by = "Any"),
  taula_metropolita = consum_total_metropolita %>% inner_join(pib_metropolita, by = "Any"),
  taula_ponent = consum_total_ponent %>% inner_join(pib_ponent, by = "Any")
)


library(dplyr)
library(ggplot2)
library(tidyr)

# Calcula variació percentual any-a-any
taula_variacions <- taula_resultat_completa %>%
  arrange(Any) %>%
  group_by(Any)  %>%
  mutate(
    PIB_var = (PIB / lag(PIB) - 1) * 100,
    Poblacio_var = (Població / lag(Població) - 1) * 100
  ) %>%
  drop_na()   # Eliminem el primer any, que no té variació

# Preparem dades per graficar
taula_long <- taula_variacions %>%
  dplyr::select(Any, PIB_var, Poblacio_var) %>%
  pivot_longer(cols = c(PIB_var, Poblacio_var),
               names_to = "Variable",
               values_to = "Variacio")

# Gràfic
ggplot(taula_long, aes(x = Any, y = Variacio, color = Variable, group = Variable)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Variació percentual anual (%) de PIB i població",
    x = "Any",
    y = "Variació (%)",
    color = "Indicador"
  ) +
  scale_color_manual(values = c("PIB_var" = "darkblue",
                                "Poblacio_var" = "darkorange"),
                     labels = c("PIB_var" = "PIB (%)",
                                "Poblacio_var" = "Població (%)")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )





