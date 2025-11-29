library(data.table)
library(tidyverse)
library(simmer)
library(simmer.plot)


data3<- fread("aigua.csv")

# Eliminem punts dels milers i convertim a número
#Importantt!!!!
data3$'Domèstic xarxa' <- as.numeric(gsub("\\.", "", data3$'Domèstic xarxa'))

# Data3 pugui fer
# funcions del tidyverse
glimpse(data3)
summarise(data3,
          n = n(),
          n.na = sum(is.na(`Codi comarca`)))


# Compta nombre de files
summarise(data3,
          n.cat = n_distinct(`Codi comarca`))


# Amb missings
recompte = count(data3, `Codi comarca`)




#ggplot(data3, aes(y = `Codi comarca`, x = `Domèstic xarxa`)) +
#  geom_point(alpha = 0.5, color = "darkblue") +
#  coord_flip() +
#  labs(title = "Valors de Domèstic xarxa per Comarca",
#       y = "Codi Comarca",
#       x = "Domèstic xarxa") +
#  theme_minimal()


ggplot(data3 %>% filter(`Comarca` == "BAIX CAMP, EL"), aes(y = Any, x = `Domèstic xarxa`)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  coord_flip() +
  labs(title = "Consum per any a la comarca BAIX CAMP",
       y = "Any",
       x = "Domèstic xarxa") +
  theme_minimal()

data3 <- data3 %>%
  mutate(`Domèstic xarxa` = as.numeric(`Domèstic xarxa`))

# I després ja pots fer el gràfic
data_baix_camp <- data3 %>%
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
