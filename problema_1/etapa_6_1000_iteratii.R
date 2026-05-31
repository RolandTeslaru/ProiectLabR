# Etapa 6, (cerinta ulteriorara 2)
#
library(dplyr)
library(ggplot2)

source("/Users/rolandteslaru/Desktop/ProiectLabR/problema_1/functii.R")

set.seed(42)

cat("Ruleaza 1000 iteratii pentru aleatoare + adaptiva\n")
t0 <- Sys.time()

sim_aleator <- simuleaza_aleatoare_full(
  n_iteratii = 1000, p_sus = 0.005, p_verif = 0.10
)
sim_adaptiv <- simuleaza_adaptiva_full(
  n_iteratii = 1000, p_sus = 0.005,
  prag = 1000, p_verif_mic = 0.05, p_verif_mare = 0.20
)

cat("Durata:", round(difftime(Sys.time(), t0, units = "secs"), 1), "s\n\n")

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

# Media + SD
rezumat <- ind |>
  group_by(strategie) |>
  summarise(
    rata_medie = mean(rata_detectie),
    rata_sd    = sd(rata_detectie)
  )


cat("=== Media și variabilitatea peste 1000 iterații ===\n")
print(rezumat)

# Boxplot
# De ce ? pentru ca ne arata nu doar media, ci si variabiliaatea (SD) a ratei de detectie intre cele 1000 de iteratii
#  adica cat de consistente sunt rezultatele peste cele 1000 de ani simulati
q_adapt <- quantile(ind$rata_detectie[ind$strategie == "adaptiva"],
                    probs = c(0.25, 0.50, 0.75))
q_alea  <- quantile(ind$rata_detectie[ind$strategie == "aleatoare"],
                    probs = c(0.25, 0.50, 0.75))

g <- ggplot(ind, aes(x = strategie, y = rata_detectie, fill = strategie)) +
  geom_boxplot(width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  # Adnotare Q1 (adaptiva)
  annotate("text", x = 1.32, y = q_adapt[1],
           label = sprintf("Q1 = %.1f%%\n(25%% iterații sub)", q_adapt[1]*100),
           hjust = 0, size = 3.5, color = "gray30") +
  # Adnotare mediană (adaptiva)
  annotate("text", x = 1.32, y = q_adapt[2],
           label = sprintf("Mediană = %.1f%%\n(50%% sub / 50%% peste)", q_adapt[2]*100),
           hjust = 0, size = 3.5, color = "gray30") +
  # Adnotare Q3 (adaptiva)
  annotate("text", x = 1.32, y = q_adapt[3],
           label = sprintf("Q3 = %.1f%%\n(75%% iterații sub)", q_adapt[3]*100),
           hjust = 0, size = 3.5, color = "gray30") +
  # Adnotare mustăți (aleatoare)
  annotate("text", x = 1.68, y = q_alea[1] - 0.005,
           label = "mustață jos\n(limita valorilor normale)",
           hjust = 1, size = 3.2, color = "gray40") +
  annotate("text", x = 1.68, y = q_alea[3] + 0.005,
           label = "mustață sus\n(limita valorilor normale)",
           hjust = 1, size = 3.2, color = "gray40") +
  labs(title = "Rata de detecție pe 1000 de ani simulați",
       subtitle = sprintf("Adaptivă: %.1f%% ± %.1f%%   Aleatoare: %.1f%% ± %.1f%%",
                          rezumat$rata_medie[1] * 100,
                          rezumat$rata_sd[1] * 100,
                          rezumat$rata_medie[2] * 100,
                          rezumat$rata_sd[2] * 100),
       x = "Strategie", y = "Rata de detecție",
       caption = "Dreptunghiul = mijlocul 50% al iterațiilor (Q1–Q3)  |  Punctele = outlieri (iterații neobișnuite)") +
  theme_minimal(base_size = 20) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(size = 11, color = "gray40"))

library(patchwork)

# Bell curve — histograma celor 1000 de rate cu curba normala suprapusa
g_bell <- ggplot(ind, aes(x = rata_detectie, fill = strategie, color = strategie)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 0.002, alpha = 0.4, position = "identity") +
  # curba normala pentru fiecare strategie
  stat_function(data = subset(ind, strategie == "adaptiva"),
                fun = dnorm,
                args = list(mean = rezumat$rata_medie[rezumat$strategie == "adaptiva"],
                            sd   = rezumat$rata_sd[rezumat$strategie == "adaptiva"]),
                color = "#F8766D", linewidth = 1.2) +
  stat_function(data = subset(ind, strategie == "aleatoare"),
                fun = dnorm,
                args = list(mean = rezumat$rata_medie[rezumat$strategie == "aleatoare"],
                            sd   = rezumat$rata_sd[rezumat$strategie == "aleatoare"]),
                color = "#00BFC4", linewidth = 1.2) +
  # linii verticale pentru medii
  geom_vline(data = rezumat,
             aes(xintercept = rata_medie, color = strategie),
             linetype = "dashed", linewidth = 1) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Distribuția ratei de detecție — bell curve",
       subtitle = "Histograma celor 1000 de iterații cu curba normală suprapusă",
       x = "Rata de detecție", y = "Densitate",
       fill = "Strategie", color = "Strategie",
       caption = "Liniile punctate = media fiecărei strategii") +
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(size = 11, color = "gray40"))

print(g / g_bell)


# Observatii:

# SD ul la aleatoare este mai mic decat la adaptiva, ceea ce inseamna ca rezultatele aleatoare 
# sunt mai consistente (mai putin variabile) intre cele 1000 de iteratii, 
# in timp ce adaptiva are o variabilitate mai mare a ratei de detectie intre iteratii.

# Dar adaptiva are o rata de detectie medie mult mai mare decat aleatoare, 
# ceea ce inseamna ca, in ciuda variabilitatii mai mari, adaptiva prinde mult mai multe cereri suspecte 
# decat aleatoare.

# Idealul ar fi sa avem o strategie care sa aiba atat rata de detectie mare, cat si variabilitate mica,
