library(data.table)
library(tidyverse)
library(simmer)
library(simmer.plot)
library(dplyr)

# Carregar funcions/dades del teu script
source("scripts/codi.R")

# -------------------------------------------------------------------------
# Crear la taula final amb totals anuals
# -------------------------------------------------------------------------

taula_final <- taula_resultat_completa %>% 
  dplyr::select(Any, Domèstic_total, Activitats_total, Consum_total, Població) %>% 
  group_by(Any) %>%
  summarise(
    Domèstic_total      = sum(Domèstic_total, na.rm = TRUE),
    Activitats_total    = sum(Activitats_total, na.rm = TRUE),
    Consum_total        = sum(Consum_total, na.rm = TRUE),
    Població_total      = sum(Població, na.rm = TRUE)
  )

# -------------------------------------------------------------------------
# Transformar dades per al gràfic de CONSUM (long format)
# -------------------------------------------------------------------------

taula_plot_consum <- taula_final %>%
  pivot_longer(
    cols = c(Domèstic_total, Activitats_total, Consum_total),
    names_to = "Categoria",
    values_to = "Consum"
  ) %>%
  mutate(
    Categoria = case_when(
      Categoria == "Domèstic_total"   ~ "Consum Domèstic",
      Categoria == "Activitats_total" ~ "Activitats Econòmiques",
      Categoria == "Consum_total"     ~ "Consum Total"
    )
  )

# -------------------------------------------------------------------------
# Gràfic consum aigua
# -------------------------------------------------------------------------

ggplot(taula_plot_consum, aes(x = Any, y = Consum, color = Categoria, group = Categoria)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Consum d'aigua per tipus i any (Totes les comarques)",
    x = "Any",
    y = "Consum (m³)",
    color = "Tipus de consum"
  ) +
  scale_x_continuous(breaks = unique(taula_final$Any)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_color_manual(values = c(
    "Consum Domèstic"       = "darkblue",
    "Activitats Econòmiques" = "darkorange",
    "Consum Total"          = "darkgreen"
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ========================================================================
#                          GRÀFIC DE POBLACIÓ
# ========================================================================

# -------------------------------------------------------------------------
# Preparar dades per al gràfic de població
# -------------------------------------------------------------------------

taula_poblacio_plot <- taula_final %>%
  dplyr::select(Any, Població_total)

# -------------------------------------------------------------------------
# Gràfic població
# -------------------------------------------------------------------------

ggplot(taula_poblacio_plot, aes(x = Any, y = Població_total)) +
  geom_line(color = "darkblue", linewidth = 1.2) +
  geom_point(color = "darkblue", size = 3) +
  labs(
    title = "Població total per any (Totes les comarques)",
    x = "Any",
    y = "Població (persones)"
  ) +
  scale_x_continuous(breaks = unique(taula_poblacio_plot$Any)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# ========================================================================
#                          GRÀFIC Consum_doméstic / Consum_total
# ========================================================================

# -------------------------------------------------------------------------
# Preparar dades per al gràfic de Consum_doméstic / Consum_total
# -------------------------------------------------------------------------

total_sobre_domestic <- taula_final %>%
  mutate(resultat = Domèstic_total / Consum_total) %>% 
  dplyr::select(Any, resultat)
# -------------------------------------------------------------------------
# Gràfic Consum_doméstic / Consum_total
# -------------------------------------------------------------------------

ggplot(total_sobre_domestic, aes(x = Any, y = resultat)) +
  geom_line(color = "darkblue", linewidth = 1.2) +
  geom_point(color = "darkblue", size = 3) +
  labs(
    title = "Consum_doméstic / Consum_total",
    x = "Any",
    y = "Consum_doméstic / Consum_total"
  ) +
  scale_x_continuous(breaks = unique(taula_poblacio_plot$Any)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# ========================================================================
#                          GRÀFIC Poblacio / Consum_total
# ========================================================================

# -------------------------------------------------------------------------
# Preparar dades per al gràfic de Poblacio / Consum_total
# -------------------------------------------------------------------------

total_sobre_domestic <- taula_final %>%
  mutate(resultat = Població_total / Consum_total) %>% 
  dplyr::select(Any, resultat)
# -------------------------------------------------------------------------
# Gràfic Poblacio / Consum_total
# -------------------------------------------------------------------------

ggplot(total_sobre_domestic, aes(x = Any, y = resultat)) +
  geom_line(color = "darkblue", linewidth = 1.2) +
  geom_point(color = "darkblue", size = 3) +
  labs(
    title = "Poblacio / Consum_total",
    x = "Any",
    y = "Poblacio / Consum_total"
  ) +
  scale_x_continuous(breaks = unique(taula_poblacio_plot$Any)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ========================================================================
#                          GRÀFIC Poblacio / Consum_total per cada regió
# ========================================================================
# Afegir columna Consum per persona
taula_plot <- taula_resultat_completa %>%
  mutate(Consum_per_capita = Consum_total / Població)

# Gràfic amb totes les regions al mateix plot
ggplot(taula_plot, aes(x = Any, y = Consum_per_capita, color = Regio, group = Regio)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Consum d'aigua per càpita per regió",
    x = "Any",
    y = "Consum per persona (m³/any)",
    color = "Regió"
  ) +
  scale_x_continuous(breaks = unique(taula_plot$Any)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )



# ========================================================================
#                          GRÀFIC Poblacio / Consum_total per cada regió / Valor primer any
# ========================================================================
# Afegir columna Consum per persona
taula_plot <- taula_resultat_completa %>%
  mutate(Consum_per_capita = Consum_total / Població)

# Gràfic amb totes les regions al mateix plot
ggplot(taula_resultat_completa, aes(x = Any, y = Consum_per_capita, color = Regio, group = Regio)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Consum d'aigua per càpita per regió",
    x = "Any",
    y = "Consum per persona (m³/any)",
    color = "Regió"
  ) +
  scale_x_continuous(breaks = unique(taula_resultat_completa$Any)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


# ========================================================================
#           GRÀFIC Població / Consum_total relativitzat al primer any
# ========================================================================

# Calculem Consum_per_capita
taula_plot <- taula_resultat_completa %>%
  mutate(Consum_per_capita = Consum_total / Població)

# Obtenir valor del primer any per cada regió
primer_any_valor <- taula_plot %>%
  group_by(Regio) %>%
  filter(Any == min(Any)) %>%
  dplyr::select(Regio, Consum_per_capita) %>%
  rename(Consum_per_capita_inicial = Consum_per_capita)

# Afegir valor inicial a la taula completa
taula_plot <- taula_plot %>%
  left_join(primer_any_valor, by = "Regio") %>%
  mutate(Consum_per_capita_index = Consum_per_capita / Consum_per_capita_inicial)

# -------------------------------------------------------------------------
# Gràfic amb índex (relatiu al primer any)
# -------------------------------------------------------------------------

ggplot(taula_plot, aes(x = Any, y = Consum_per_capita_index, color = Regio, group = Regio)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Consum d'aigua per càpita relatiu al primer any per regió",
    x = "Any",
    y = "Consum per persona (índex, primer any = 1)",
    color = "Regió"
  ) +
  scale_x_continuous(breaks = unique(taula_plot$Any)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01), limits = c(0, NA)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


# ========================================================================
#           GRÀFIC Consum_total / Domèstic_total relativitzat al primer any
# ========================================================================

# Calculem Consum_per_capita
taula_plot <- taula_resultat_completa %>%
  mutate(Consum_per_capita = Domèstic_total / Consum_total)

# Obtenir valor del primer any per cada regió
primer_any_valor <- taula_plot %>%
  group_by(Regio) %>%
  filter(Any == min(Any)) %>%
  dplyr::select(Regio, Consum_per_capita) %>%
  rename(Consum_per_capita_inicial = Consum_per_capita)

# Afegir valor inicial a la taula completa
taula_plot <- taula_plot %>%
  left_join(primer_any_valor, by = "Regio") %>%
  mutate(Consum_per_capita_index = Consum_per_capita / Consum_per_capita_inicial)

# -------------------------------------------------------------------------
# Gràfic amb índex (relatiu al primer any)
# -------------------------------------------------------------------------

ggplot(taula_plot, aes(x = Any, y = Consum_per_capita_index, color = Regio, group = Regio)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Consum d'aigua domestic per total relatiu al primer any per regió",
    x = "Any",
    y = "Consum domestic/total (índex, primer any = 1)",
    color = "Regió"
  ) +
  scale_x_continuous(breaks = unique(taula_plot$Any)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01), limits = c(0, NA)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Normalitzarem a 


# ========================================================================
#           GRÀFIC Consum_total / Domèstic_total relativitzat al primer any
# ========================================================================

# Calculem Consum_per_capita
taula_plot <- taula_resultat_completa %>%
  mutate(Consum_per_capita = Domèstic_total / Consum_total)

# Obtenir valor del primer any per cada regió
primer_any_valor <- taula_plot %>%
  group_by(Regio) %>%
  filter(Any == min(Any)) %>%
  dplyr::select(Regio, Consum_per_capita) %>%
  rename(Consum_per_capita_inicial = Consum_per_capita)

# Afegir valor inicial a la taula completa
taula_plot <- taula_plot %>%
  left_join(primer_any_valor, by = "Regio") %>%
  mutate(Consum_per_capita_index = Consum_per_capita / Consum_per_capita_inicial)

# -------------------------------------------------------------------------
# Gràfic amb índex (relatiu al primer any)
# -------------------------------------------------------------------------

ggplot(taula_plot, aes(x = Any, y = Consum_per_capita_index, color = Regio, group = Regio)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Consum d'aigua domestic per total relatiu al primer any per regió",
    x = "Any",
    y = "Consum domestic/total (índex, primer any = 1)",
    color = "Regió"
  ) +
  scale_x_continuous(breaks = unique(taula_plot$Any)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01), limits = c(0, NA)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )



# ========================================================================
#           GRÀFIC  Domèstic_total relativitzat al primer any
# ========================================================================

# Calculem Consum_per_capita
taula_a_estudiar <- taula_resultat_completa %>%
  mutate(Consum_per_capita = Domèstic_total)

# Obtenir valor del primer any per cada regió
primer_any_valor <- taula_a_estudiar %>%
  group_by(Regio) %>%
  filter(Any == min(Any)) %>%
  dplyr::select(Regio, Consum_per_capita) %>%
  rename(Consum_per_capita_inicial = Consum_per_capita)

# Afegir valor inicial a la taula completa
taula_a_estudiar <- taula_a_estudiar %>%
  left_join(primer_any_valor, by = "Regio") %>%
  mutate(Consum_per_capita_index = Consum_per_capita / Consum_per_capita_inicial)

# -------------------------------------------------------------------------
# Gràfic amb índex (relatiu al primer any)
# -------------------------------------------------------------------------

ggplot(taula_a_estudiar, aes(x = Any, y = Consum_per_capita_index, color = Regio, group = Regio)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Consum d'aigua domestic per total relatiu al primer any per regió",
    x = "Any",
    y = "Consum domestic/total (índex, primer any = 1)",
    color = "Regió"
  ) +
  scale_x_continuous(breaks = unique(taula_a_estudiar$Any)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01), limits = c(0, NA)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )