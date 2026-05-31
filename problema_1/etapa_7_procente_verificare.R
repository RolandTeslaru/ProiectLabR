# Etapa 7
# variaza procentul de verificare c(1%, 5%, 10%, 20%, 30%)
#   - rata medie de detecție
#   - probabilitatea zilnică de detecție
# VALIDARE TEORETICĂ: pentru strategia aleatoare,
# distribuția hipergeometrică prezice
#   E[rata_detectie] = k/N = p_verif
# Linia teoretică y = x ar trebui să se suprapuna perfect.

library(dplyr)
library(ggplot2)

source("/Users/rolandteslaru/Desktop/ProiectLabR/problema_1/functii.R")

set.seed(42)

studiu <- studiu_procente_verificare(
  procente = c(0.01, 0.05, 0.10, 0.20, 0.30),
  n_iteratii = 1000,
  p_sus = 0.005
)

cat("=== Studiul pe procente de verificare ===\n")
print(studiu)

# Grafic 1: rata medie vs procent (vs linia teoretică)
g1 <- ggplot(studiu, aes(x = p_verif, y = rata_detectie_medie)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "gray50") +
  geom_line(color = "firebrick", linewidth = 1) +
  geom_point(size = 3, color = "firebrick") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Rata empirică de detecție vs predicția teoretică",
       subtitle = "Linia punctată: y = x (predicție hipergeometrică)",
       x = "Procent verificat", y = "Rata medie de detecție") +
  theme_minimal(base_size = 24)

# Grafic 2: P(detecție ≥ 1 / zi) — curba de saturație
g2 <- ggplot(studiu, aes(x = p_verif, y = prob_detectie_zi_medie)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(size = 3, color = "steelblue") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "P(detecție ≥ 1 / zi) vs procentul verificat",
       subtitle = "Curbă de saturație — randamente descrescătoare",
       x = "Procent verificat",
       y = "P(detecție ≥ 1 într-o zi)") +
  theme_minimal(base_size = 24)

print(g1)
print(g2)
