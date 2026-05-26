# ==========================================================
# Proiect PS — Simulare evenimente rare
# Script principal: rulează cele 3 strategii și compară rezultatele
# ==========================================================

# 1. Încarcă pachetele necesare
library(dplyr)
library(ggplot2)
library(tidyr)

# 2. Încarcă funcțiile definite în celălalt fișier
source("/Users/rolandteslaru/Desktop/ProiectLabR/problema_1/functii.R")

# 3. Setează un seed ca rezultatele să fie reproductibile
set.seed(42)

# ==========================================================
# PARTEA A: O singură simulare de an pentru fiecare strategie
# (folosit pentru graficele zilnice și verificare rapidă)
# ==========================================================

an_aleator <- simuleaza_aleatoare_full(n_iteratii = 1, n_zile = 365,
                                        lambda = 1000, p_sus = 0.005,
                                        p_verif = 0.10)

an_adaptiv <- simuleaza_adaptiva_full(n_iteratii = 1, n_zile = 365,
                                       lambda = 1000, p_sus = 0.005,
                                       prag = 1000,
                                       p_verif_mic = 0.05,
                                       p_verif_mare = 0.20)

an_geo <- simuleaza_geografica_full(n_iteratii = 1, n_zile = 365,
                                     lambda = 1000)

# Vedem ce s-a generat
head(an_aleator)
head(an_adaptiv)
head(an_geo)

# Statistici sumare per strategie pentru anul ăsta
sumar_an <- function(df, eticheta) {
  data.frame(
    Strategie = eticheta,
    Total_suspecte = sum(df$n_sus),
    Total_detectate = sum(df$detectate),
    Rata_detectie = round(sum(df$detectate) / sum(df$n_sus), 3),
    Zile_cu_detectie = sum(df$detectate >= 1),
    Total_verificari = sum(df$n_verificate)
  )
}

rbind(
  sumar_an(an_aleator, "Aleatoare 10%"),
  sumar_an(an_adaptiv, "Adaptivă 5%/20%"),
  sumar_an(an_geo,     "Geografică")
)

# ==========================================================
# PARTEA B: 1000 de simulări de an pentru fiecare strategie
# (folosit pentru media + variabilitatea indicatorilor)
# ==========================================================

cat("Rulează 1000 de iterații pentru fiecare strategie...\n")

t0 <- Sys.time()

sim_aleator <- simuleaza_aleatoare_full(n_iteratii = 1000, p_sus = 0.005,
                                         p_verif = 0.10)
sim_adaptiv <- simuleaza_adaptiva_full(n_iteratii = 1000, p_sus = 0.005,
                                        prag = 1000,
                                        p_verif_mic = 0.05,
                                        p_verif_mare = 0.20)
sim_geo <- simuleaza_geografica_chunks(n_iteratii = 1000, chunk = 50)

cat("Durată totală:", round(difftime(Sys.time(), t0, units = "secs"), 1), "s\n")

# Agregare per iterație (un "an" = o iterație)
indicatori_per_iteratie <- function(df) {
  df |>
    group_by(iteratie, strategie) |>
    summarise(
      total_suspecte = sum(n_sus),
      total_detectate = sum(detectate),
      rata_detectie = sum(detectate) / pmax(sum(n_sus), 1),  # evită 0/0
      zile_cu_detectie = sum(detectate >= 1),
      total_verificari = sum(n_verificate),
      .groups = "drop"
    )
}

ind_aleator <- indicatori_per_iteratie(sim_aleator)
ind_adaptiv <- indicatori_per_iteratie(sim_adaptiv)
ind_geo     <- indicatori_per_iteratie(sim_geo)

# Combinăm într-un singur tabel
toate_indicatorii <- bind_rows(ind_aleator, ind_adaptiv, ind_geo)

# Media + deviație standard pe cele 1000 de iterații
rezumat_final <- toate_indicatorii |>
  group_by(strategie) |>
  summarise(
    rata_medie = mean(rata_detectie),
    rata_sd = sd(rata_detectie),
    zile_detectie_medie = mean(zile_cu_detectie),
    zile_detectie_sd = sd(zile_cu_detectie),
    verificari_medii = mean(total_verificari),
    verificari_sd = sd(total_verificari)
  )

print(rezumat_final)

# ==========================================================
# PARTEA C: Graficele
# ==========================================================

# Histogramă: numărul de cereri suspecte pe zi (strategia aleatoare)
g1 <- ggplot(an_aleator, aes(x = n_sus)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Distribuția numărului zilnic de cereri suspecte",
       x = "Cereri suspecte pe zi", y = "Frecvență") +
  theme_minimal()

# Histogramă: detectatele pe zi (toate strategiile)
g2 <- bind_rows(an_aleator, an_adaptiv, an_geo) |>
  ggplot(aes(x = detectate, fill = strategie)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Distribuția numărului zilnic de detecții",
       x = "Detectate pe zi", y = "Frecvență") +
  theme_minimal()

# Evoluție zilnică: suspecte vs detectate (strategia geografică)
g3 <- an_geo |>
  pivot_longer(cols = c(n_sus, detectate),
               names_to = "tip", values_to = "valoare") |>
  ggplot(aes(x = zi, y = valoare, color = tip)) +
  geom_line(alpha = 0.7) +
  labs(title = "Evoluția zilnică (strategia geografică)",
       x = "Ziua", y = "Număr") +
  theme_minimal()

# Comparație: rata de detecție pe strategii (1000 de iterații)
g4 <- toate_indicatorii |>
  ggplot(aes(x = strategie, y = rata_detectie, fill = strategie)) +
  geom_boxplot() +
  labs(title = "Rata de detecție (1000 simulări)",
       x = "Strategie", y = "Detectate / Suspecte") +
  theme_minimal() +
  theme(legend.position = "none")

# Afișează graficele
print(g1)
print(g2)
print(g3)
print(g4)
