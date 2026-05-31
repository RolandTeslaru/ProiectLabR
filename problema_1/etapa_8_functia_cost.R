# ==========================================================
# ETAPA 8 — Funcția de cost pentru cele 3 strategii
# (cerința ulterioară 1)
# ----------------------------------------------------------
# Introducem două costuri:
#   c1 = costul unei verificări manuale
#   c2 = costul de a NU detecta o cerere suspectă
#
# Cost total per an = c1 * verificări + c2 * nedetectate
#
# Calculăm costul pentru fiecare din cele 3 strategii pe un
# singur an de simulare și comparăm din ce se compune.
# ==========================================================

library(dplyr)
library(tidyr)
library(ggplot2)

source("/Users/rolandteslaru/Desktop/ProiectLabR/problema_1/functii.R")

set.seed(42)

# Costuri, valori alese arbitrar
c1 <- 1 # nu costa mult sa verific o cerere 
c2 <- 300 # costa mult ratearea unei cerereri suspecte (duce la frauda, damage etc)

# Un singur an pentru fiecare strategie
sim_aleator <- simuleaza_aleatoare_full(
  n_iteratii = 1, p_sus = 0.005, p_verif = 0.10
)
sim_adaptiv <- simuleaza_adaptiva_full(
  n_iteratii = 1, p_sus = 0.005,
  prag = 1000, p_verif_mic = 0.05, p_verif_mare = 0.20
)
sim_geo <- simuleaza_geografica_full(n_iteratii = 1)

# Calculul costului per strategie
calc_cost <- function(df, eticheta) {
  data.frame(
    strategie         = eticheta,
    verificari        = sum(df$n_verificate),
    nedetectate       = sum(df$nedetectate),
    cost_verif        = c1 * sum(df$n_verificate),
    cost_nedetectare  = c2 * sum(df$nedetectate),
    cost_total        = c1 * sum(df$n_verificate) +
                        c2 * sum(df$nedetectate)
  )
}

costuri <- rbind(
  calc_cost(sim_aleator, "aleatoare"),
  calc_cost(sim_adaptiv, "adaptiva"),
  calc_cost(sim_geo,     "geografica")
) |> arrange(cost_total)

cat(sprintf("=== Funcția de cost (c1 = %d, c2 = %d) ===\n", c1, c2))
print(costuri)

cat(sprintf("\nStrategia cu cost minim: %s (cost total = %.0f)\n",
            costuri$strategie[1], costuri$cost_total[1]))

componente <- costuri |>
  select(strategie, cost_verif, cost_nedetectare) |>
  pivot_longer(c(cost_verif, cost_nedetectare),
               names_to = "componenta", values_to = "cost") |>
  mutate(componenta = recode(componenta,
                             cost_verif = "Verificări (c1)",
                             cost_nedetectare = "Nedetectări (c2)"))

g <- ggplot(componente, aes(x = reorder(strategie, cost),
                            y = cost, fill = componenta)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::comma(round(cost))),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 6) +
  # Etichetă cu costul TOTAL deasupra fiecărei coloane
  geom_text(data = costuri,
            aes(x = strategie, y = cost_total,
                label = paste0("Total: ", scales::comma(cost_total))),
            inherit.aes = FALSE,
            vjust = -0.6, fontface = "bold", size = 5.5) +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0.02, 0.12))) +
  scale_fill_manual(values = c("Verificări (c1)" = "steelblue",
                               "Nedetectări (c2)" = "tomato")) +
  labs(title = "Din ce se compune costul fiecărei strategii?",
       subtitle = sprintf("c1 = %d (verificare), c2 = %d (nedetectare)",
                          c1, c2),
       x = "Strategie", y = "Cost anual", fill = NULL) +
  theme_minimal(base_size = 22) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 22),
        plot.subtitle = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 17),
        legend.text = element_text(size = 16))

print(g)
