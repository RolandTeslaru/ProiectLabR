# ==========================================================
# ETAPA 6 — 1000 de iterații (cerința ulterioară 2)
# ----------------------------------------------------------
# Un singur an este doar UN eșantion → indicatorii oscilează.
# Rulăm 1000 de ani independenți și comparăm:
#   - media (cea mai bună estimare a "adevărului")
#   - deviația standard (cât de mult variază între ani)
# ==========================================================

library(dplyr)
library(ggplot2)

source("/Users/rolandteslaru/Desktop/ProiectLabR/problema_1/functii.R")

set.seed(42)

cat("Rulează 1000 iterații pentru aleatoare + adaptivă...\n")
t0 <- Sys.time()

sim_aleator <- simuleaza_aleatoare_full(
  n_iteratii = 1000, p_sus = 0.005, p_verif = 0.10
)
sim_adaptiv <- simuleaza_adaptiva_full(
  n_iteratii = 1000, p_sus = 0.005,
  prag = 1000, p_verif_mic = 0.05, p_verif_mare = 0.20
)

cat("Durată:", round(difftime(Sys.time(), t0, units = "secs"), 1), "s\n\n")

# Agregare per iterație
agregat <- function(df) {
  df |>
    group_by(iteratie, strategie) |>
    summarise(
      rata_detectie = sum(detectate) / pmax(sum(n_sus), 1),
      .groups = "drop"
    )
}

ind <- bind_rows(agregat(sim_aleator), agregat(sim_adaptiv))

# Media ± SD
rezumat <- ind |>
  group_by(strategie) |>
  summarise(
    rata_medie = mean(rata_detectie),
    rata_sd    = sd(rata_detectie)
  )

cat("=== Media și variabilitatea peste 1000 iterații ===\n")
print(rezumat)

# Boxplot: distribuția ratei de detecție pe iterații
g <- ggplot(ind, aes(x = strategie, y = rata_detectie, fill = strategie)) +
  geom_boxplot(width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Rata de detecție pe 1000 de ani simulați",
       subtitle = sprintf("Adaptivă: %.1f%% ± %.1f%%   Aleatoare: %.1f%% ± %.1f%%",
                          rezumat$rata_medie[1] * 100,
                          rezumat$rata_sd[1] * 100,
                          rezumat$rata_medie[2] * 100,
                          rezumat$rata_sd[2] * 100),
       x = "Strategie", y = "Rata de detecție") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

print(g)
