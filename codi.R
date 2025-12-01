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

# Mostrar barres ----------------------------------------------------------

# Calcular el consum total per any de totes les comarques
consum_total_any <- consum_aigua %>%
  group_by(Any) %>%
  summarise(
    Consum_total = sum(`Domèstic xarxa` + `Activitats econòmiques i fonts pròpies`, na.rm = TRUE),
    Domèstic_total = sum(`Domèstic xarxa`, na.rm = TRUE),
    Activitats_total = sum(`Activitats econòmiques i fonts pròpies`, na.rm = TRUE)
  )

# Mostrar les dades per verificar
print(consum_total_any)

# Gràfic del consum total per any (línia) - AMB TOTS ELS ANYS
ggplot(consum_total_any, aes(x = Any, y = Consum_total)) +
  geom_line(color = "darkred", linewidth = 1.2) +
  geom_point(color = "darkred", size = 3) +
  labs(title = "Consum total d'aigua per any (Totes les comarques)",
       subtitle = "Suma de consum domèstic i activitats econòmiques",
       x = "Any",
       y = "Consum total (m³)") +
  scale_x_continuous(breaks = unique(consum_total_any$Any)) +  # AQUESTA ÉS LA CLAU
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "gray50"),
        axis.text.x = element_text(angle = 45, hjust = 1))  # Giro els anys per llegibilitat

# I ARA el gràfic comparatiu de les dues categories - AMB TOTS ELS ANYS
consum_total_any %>%
  pivot_longer(cols = c(Domèstic_total, Activitats_total),
               names_to = "Categoria",
               values_to = "Consum") %>%
  mutate(Categoria = case_when(
    Categoria == "Domèstic_total" ~ "Consum Domèstic",
    Categoria == "Activitats_total" ~ "Activitats Econòmiques"
  )) %>%
  ggplot(aes(x = Any, y = Consum, color = Categoria, group = Categoria)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(title = "Consum d'aigua per tipus i any (Totes les comarques)",
       x = "Any",
       y = "Consum (m³)",
       color = "Tipus de consum") +
  scale_x_continuous(breaks = unique(consum_total_any$Any)) +  # AQUESTA ÉS LA CLAU
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("Consum Domèstic" = "darkblue",
                                "Activitats Econòmiques" = "darkorange")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))  # Giro els anys per llegibilitat

# PIV i aigua a les comarques gironines (WIP) ----------------------------------------------------------

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
    Activitats_total = sum(`Activitats econòmiques i fonts pròpies`, na.rm = TRUE)
  )

pib_girona <- pib_provincia %>%
  filter(`àmbit territorial de planificació` == "Comarques Gironines" ) %>%
  rename(Total = `branques d'activitat`) %>% # canvia el nom de la columna
  filter(Total == "total") %>%
  rename(Any = any) %>% # canvia el nom de la columna
  rename(PIB = valor) %>%
  dplyr::select(Any, PIB, `àmbit territorial de planificació`) %>%
  group_by(Any)

taula_resultat <- consum_total_girona %>%
  inner_join(pib_girona, by = "Any")




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
    Activitats_total = sum(`Activitats econòmiques i fonts pròpies`, na.rm = TRUE)
  )

pib_alt_pirineu_i_aran <- pib_provincia %>%
  filter(`àmbit territorial de planificació` == "Alt Pirineu i Aran") %>%
  rename(Total = `branques d'activitat`) %>% # canvia el nom de la columna
  filter(Total == "total") %>%
  rename(Any = any) %>% # canvia el nom de la columna
  rename(PIB = valor) %>%
  dplyr::select(Any, PIB, `àmbit territorial de planificació`) %>%
  group_by(Any)


taula_resultat <- bind_rows(
  taula_girona = consum_total_girona %>% inner_join(pib_girona, by = "Any"),
  taula_alt_pirineu_i_aran = consum_total_alt_pirineu_i_aran %>% inner_join(pib_alt_pirineu_i_aran, by = "Any"),
)



