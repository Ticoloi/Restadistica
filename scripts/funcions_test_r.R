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


# glimpse ----------------------------------------------------------

# consum_aigua pugui fer
# funcions del tidyverse
glimpse(consum_aigua)
summarise(consum_aigua,
          n = n(),
          n.na = sum(is.na(`Codi comarca`)))

# Compta nombre de files
summarise(consum_aigua,
          n.cat = n_distinct(`Codi comarca`))

# Amb missings
recompte = count(consum_aigua, `Codi comarca`)


#ggplot(consum_aigua, aes(y = `Codi comarca`, x = `Domèstic xarxa`)) +
#  geom_point(alpha = 0.5, color = "darkblue") +
#  coord_flip() +
#  labs(title = "Valors de Domèstic xarxa per Comarca",
#       y = "Codi Comarca",
#       x = "Domèstic xarxa") +
#  theme_minimal()

# plot 1 ----------------------------------------------------------

ggplot(consum_aigua %>% filter(`Comarca` == "BAIX CAMP, EL"), aes(y = Any, x = `Domèstic xarxa`)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  coord_flip() +
  labs(title = "Consum per any a la comarca BAIX CAMP",
       y = "Any",
       x = "Domèstic xarxa") +
  theme_minimal()

# plot 2 ----------------------------------------------------------

consum_aigua <- consum_aigua %>%
  mutate(`Domèstic xarxa` = as.numeric(`Domèstic xarxa`))

# I després ja pots fer el gràfic
data_baix_camp <- consum_aigua %>%
  filter(`Comarca` == "BAIX CAMP, EL") %>%
  group_by(Any) %>%
  summarise(`Domèstic xarxa` = sum(`Domèstic xarxa`, na.rm = TRUE))

ggplot(data_baix_camp, aes(x = Any, y = `Domèstic xarxa`)) +
  geom_line(color = "darkblue", linewidth = 1) +
  geom_point(alpha = 0.7, color = "darkblue", size = 3) +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
  labs(title = "Consum per any a la comarca BAIX CAMP",
       x = "Any",
       y = "Domèstic xarxa (Total)") +
  theme_minimal()


# plots 3 ----------------------------------------------------------


consum_aigua <- consum_aigua %>%
  mutate(`Domèstic xarxa` = as.numeric(`Domèstic xarxa`))

# I després ja pots fer el gràfic
data_baix_camp <- consum_aigua %>%
  filter(`Comarca` == "BAIX CAMP, EL") %>%
  group_by(Any) %>%
  summarise(`Domèstic xarxa` = sum(`Domèstic xarxa`, na.rm = TRUE))

ggplot(data_baix_camp, aes(x = Any, y = `Domèstic xarxa`)) +
  geom_line(color = "darkblue", linewidth = 1) +
  geom_point(alpha = 0.7, color = "darkblue", size = 3) +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
  labs(title = "Consum per any a la comarca BAIX CAMP",
       x = "Any",
       y = "Domèstic xarxa (Total)") +
  theme_minimal()




# I ARA el gràfic comparatiu de les dues categories - AMB TOTS ELS ANYS
taula_girona %>%
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
  scale_x_continuous(breaks = unique(consum_total_girona$Any)) +  # AQUESTA ÉS LA CLAU
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("Consum Domèstic" = "darkblue",
                                "Activitats Econòmiques" = "darkorange")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))  # Giro els anys per llegibilitat


# I ARA el gràfic comparatiu de les dues categories - AMB TOTS ELS ANYS
taula_girona %>%
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
  scale_x_continuous(breaks = unique(taula_girona$Any)) +  # AQUESTA ÉS LA CLAU
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("PIB" = "darkblue",
                                "Poblacio" = "darkorange")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))  # Giro els anys per llegibilitat



# Mostrar les dades per verificar
#print(consum_total_any)

# Gràfic del consum total per any (línia) - AMB TOTS ELS ANYS
#ggplot(consum_total_any, aes(x = Any, y = Consum_total)) +
# geom_line(color = "darkred", linewidth = 1.2) +
# geom_point(color = "darkred", size = 3) +
# labs(title = "Consum total d'aigua per any (Totes les comarques)",
#      subtitle = "Suma de consum domèstic i activitats econòmiques",
#      x = "Any",
#      y = "Consum total (m³)") +
# scale_x_continuous(breaks = unique(consum_total_any$Any)) +  # AQUESTA ÉS LA CLAU
# scale_y_continuous(labels = scales::comma) +
# theme_minimal() +
# theme(plot.title = element_text(face = "bold", size = 14),
#       plot.subtitle = element_text(color = "gray50"),
#       axis.text.x = element_text(angle = 45, hjust = 1))  # Giro els anys per llegibilitat
#
# I ARA el gràfic comparatiu de les dues categories - AMB TOTS ELS ANYS
#consum_total_any %>%
# pivot_longer(cols = c(Domèstic_total, Activitats_total),
#              names_to = "Categoria",
#              values_to = "Consum") %>%
# mutate(Categoria = case_when(
#   Categoria == "Domèstic_total" ~ "Consum Domèstic",
#   Categoria == "Activitats_total" ~ "Activitats Econòmiques"
## )) %>%
#ggplot(aes(x = Any, y = Consum, color = Categoria, group = Categoria)) +
# geom_line(linewidth = 1.2) +
# geom_point(size = 3) +
# labs(title = "Consum d'aigua per tipus i any (Totes les comarques)",
###      x = "Any",
#    y = "Consum (m³)",
#      color = "Tipus de consum") +
# scale_x_continuous(breaks = unique(consum_total_any$Any)) +  # AQUESTA ÉS LA CLAU
# scale_y_continuous(labels = scales::comma) +
# scale_color_manual(values = c("Consum Domèstic" = "darkblue",
#                               "Activitats Econòmiques" = "darkorange")) +
# theme_minimal() +
# theme(plot.title = element_text(face = "bold", size = 14),
#       legend.position = "bottom",
#       axis.text.x = element_text(angle = 45, hjust = 1))  # Giro els anys per llegibilitat

# PIV i aigua a les comarques gironines (WIP) ----------------------------------------------------------
