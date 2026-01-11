library(data.table)
library(tidyverse)
library(gridExtra)
library(scales)
library(data.table)
library(simmer)
library(simmer.plot)
library(dplyr)
library(purrr)


source('src/main.R')

# Seleccionar les principals comarques (top 5 per consum)
ultim_any <- max(consum_aigua$Any)
top_comarques <- consum_aigua %>%
  filter(Any == ultim_any) %>%
  filter(Comarca == 'BARCELONÈS, EL') %>%
  arrange(desc(Total)) %>%
  #head(5) %>%
  pull(Comarca)

# 1. PLOT: Dades originals (sense model) ------------------------------------
plot_dades <- consum_aigua %>%
  filter(Comarca %in% top_comarques) %>%
  ggplot(aes(x = Any, y = Total, color = Comarca)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(title = "Evolució del Consum d'Aigua - Dades Originals",
       x = "Any",
       y = "Consum Total (m³)",
       color = "Comarca") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

print(plot_dades)

# 2. PLOT: Regressió Lineal Simple ------------------------------------------
plot_lineal <- consum_aigua %>%
  filter(Comarca %in% top_comarques) %>%
  ggplot(aes(x = Any, y = Total, color = Comarca)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, size = 1) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(title = "Evolució del Consum d'Aigua - Regressió Lineal",
       x = "Any",
       y = "Consum Total (m³)",
       color = "Comarca") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

print(plot_lineal)

# 3. PLOT: Regressió amb Punt de Trencament (Tau) - CORREGIT ---------------

for (i in seq_along(0:(n_comarques - 1))) {
nombre_comarca <- 1
# PLOT DE PROVA
dada_to_show <- consum_aigua %>% filter(Comarca == noms_comarques[nombre_comarca])

dada_to_show <- dada_to_show %>%
  mutate(latency_pred = predict(model_amb_tau[[noms_comarques[nombre_comarca]]], newdata = dada_to_show),
         latency_pred2 = predict(regresio_lineal[[noms_comarques[nombre_comarca]]], newdata = dada_to_show))

ggplot(dada_to_show, aes(x = Any, y = Total)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_line(aes(y = latency_pred), color = "red", linewidth = 1.2) +
  geom_line(aes(y = latency_pred2), color = "blue", linewidth = 1.2)
  labs(
    title = "Regressió lineal múltiple amb knot a load = 150",
    x = "Load",
    y = "Latency"
  ) +
  theme_minimal()
}

# OPCIONAL: Combinar els 3 gràfics en un panell -----------------------------
#panell_comparacio <- grid.arrange(
#  plot_dades,
#  plot_lineal,
#  ncol = 1
#)


# OPCIONAL: Guardar individualment ------------------------------------------
# ggsave("plot_dades_originals.png", plot_dades, width = 12, height = 6)
#ggsave("plot_regressio_lineal.png", plot_lineal, width = 12, height = 6)
