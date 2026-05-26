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
# Costuri pentru funcția de cost (cerința ulterioară 1)
# c1 = costul de a verifica o cerere (efort operațional)
# c2 = costul de a NU detecta o cerere suspectă (pierdere de securitate)
# Justificare: o cerere suspectă nedetectată e mult mai costisitoare
# decât o verificare manuală. Raportul c2/c1 trebuie să depășească
# N/s ≈ 1/p_sus = 200 pentru ca verificarea aleatoare să fie
# economic justificată. Alegem c2/c1 = 300 → optim interior real.
c1_cost <- 1
c2_cost <- 300

indicatori_per_iteratie <- function(df) {
  df |>
    group_by(iteratie, strategie) |>
    summarise(
      total_suspecte = sum(n_sus),
      total_detectate = sum(detectate),
      rata_detectie = sum(detectate) / pmax(sum(n_sus), 1),  # evită 0/0
      rata_nedetectie = 1 - sum(detectate) / pmax(sum(n_sus), 1),
      zile_cu_detectie = sum(detectate >= 1),
      prob_detectie_zi = mean(detectate >= 1),
      total_verificari = sum(n_verificate),
      verificari_zilnice_medii = mean(n_verificate),
      cost_total = c1_cost * sum(n_verificate) +
                   c2_cost * sum(nedetectate),
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
    rata_nedetectie_medie = mean(rata_nedetectie),
    prob_detectie_zi_medie = mean(prob_detectie_zi),
    prob_detectie_zi_sd = sd(prob_detectie_zi),
    zile_detectie_medie = mean(zile_cu_detectie),
    zile_detectie_sd = sd(zile_cu_detectie),
    verificari_zilnice_medii = mean(verificari_zilnice_medii),
    verificari_medii = mean(total_verificari),
    verificari_sd = sd(total_verificari),
    cost_mediu = mean(cost_total),
    cost_sd = sd(cost_total)
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
  theme_minimal(base_size = 14)

# Histogramă: detectatele pe zi (toate strategiile)
g2 <- bind_rows(an_aleator, an_adaptiv, an_geo) |>
  ggplot(aes(x = detectate, fill = strategie)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Distribuția numărului zilnic de detecții",
       x = "Detectate pe zi", y = "Frecvență") +
  theme_minimal(base_size = 14)

# Evoluție zilnică: suspecte vs detectate (strategia geografică)
g3 <- an_geo |>
  pivot_longer(cols = c(n_sus, detectate),
               names_to = "tip", values_to = "valoare") |>
  ggplot(aes(x = zi, y = valoare, color = tip)) +
  geom_line(alpha = 0.7) +
  labs(title = "Evoluția zilnică (strategia geografică)",
       x = "Ziua", y = "Număr") +
  theme_minimal(base_size = 14)

# Comparație: rata de detecție pe strategii (1000 de iterații)
g4 <- toate_indicatorii |>
  ggplot(aes(x = strategie, y = rata_detectie, fill = strategie)) +
  geom_boxplot() +
  labs(title = "Rata de detecție (1000 simulări)",
       x = "Strategie", y = "Detectate / Suspecte") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Afișează graficele
print(g1)
print(g2)
print(g3)
print(g4)

# ==========================================================
# PARTEA D: Studiul efectului procentului de verificare (cerința 7)
# Rulează strategia aleatoare cu 1%, 5%, 10%, 20%, 30%
# ==========================================================

cat("\nRulează studiul pe procente de verificare...\n")
t0 <- Sys.time()

studiu <- studiu_procente_verificare(
  procente = c(0.01, 0.05, 0.10, 0.20, 0.30),
  n_iteratii = 1000,
  p_sus = 0.005,
  c1 = c1_cost, c2 = c2_cost
)

# Versiune mai densă pentru curba de cost (găsim minimul)
studiu_cost <- studiu_procente_verificare(
  procente = seq(0.01, 0.50, by = 0.02),
  n_iteratii = 300,
  p_sus = 0.005,
  c1 = c1_cost, c2 = c2_cost
)

cat("Durată studiu procente:",
    round(difftime(Sys.time(), t0, units = "secs"), 1), "s\n")

print(studiu)

# Grafic: curba probabilității de detecție vs procent verificat
g5 <- ggplot(studiu, aes(x = p_verif, y = prob_detectie_zi_medie)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(size = 3, color = "steelblue") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Efectul procentului de verificare asupra detecției",
       subtitle = "Strategia aleatoare, p_sus = 0.005, 1000 simulări per punct",
       x = "Procent verificat din cereri",
       y = "P(detecție ≥ 1 cerere suspectă într-o zi)") +
  theme_minimal(base_size = 14)

# Grafic: rata medie de detecție vs procent verificat
g6 <- ggplot(studiu, aes(x = p_verif, y = rata_detectie_medie)) +
  geom_line(color = "firebrick", linewidth = 1) +
  geom_point(size = 3, color = "firebrick") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.4) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Rata medie de detecție vs procent verificat",
       subtitle = "Linia punctată = identitatea (predicție teoretică)",
       x = "Procent verificat", y = "Rata medie de detecție") +
  theme_minimal(base_size = 14)

print(g5)
print(g6)

# ==========================================================
# PARTEA E: Funcția de cost (cerința ulterioară 1)
# Costul total ca funcție de procentul verificat
# ==========================================================

# Procentul optim (minimul empiric al costului)
p_optim <- studiu_cost$p_verif[which.min(studiu_cost$cost_mediu)]
cost_min <- min(studiu_cost$cost_mediu)
cat(sprintf("\nProcent optim de verificare: %.0f%% (cost mediu = %.0f)\n",
            p_optim * 100, cost_min))

g7 <- ggplot(studiu_cost, aes(x = p_verif, y = cost_mediu)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(size = 2, color = "darkgreen") +
  geom_vline(xintercept = p_optim, linetype = "dashed",
             color = "red", alpha = 0.7) +
  annotate("text", x = p_optim, y = cost_min,
           label = sprintf("optim: %.0f%%", p_optim * 100),
           hjust = -0.2, vjust = -0.5, color = "red") +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Costul total mediu vs procentul de verificare",
       subtitle = sprintf("c1 = %d (verificare), c2 = %d (nedetectare)",
                          c1_cost, c2_cost),
       x = "Procent verificat", y = "Cost total mediu pe an") +
  theme_minimal(base_size = 14)

# Comparația costurilor pe strategii (boxplot)
g8 <- toate_indicatorii |>
  ggplot(aes(x = strategie, y = cost_total, fill = strategie)) +
  geom_boxplot() +
  labs(title = "Distribuția costului total anual pe strategii",
       subtitle = sprintf("c1 = %d, c2 = %d, 1000 simulări",
                          c1_cost, c2_cost),
       x = "Strategie", y = "Cost total anual") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

print(g7)
print(g8)
