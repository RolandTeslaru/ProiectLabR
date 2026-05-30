# Etapa 2: Simularea unui an de cereri

library(ggplot2)

set.seed(42)

# La fel ca in prima etapa.

n_zile  <- 365
lambda  <- 1000
p_sus   <- 0.005
p_verif <- 0.10 # alegem un procent aleator de verificare (10%)

# de data asta generam vecotrizat pentru n_zile = 365
n_req        <- rpois(n_zile, lambda) 
n_sus        <- rbinom(n_zile, size = n_req, prob = p_sus)
n_verificate <- round(n_req * p_verif)
detectate    <- rhyper(nn = n_zile, m = n_sus, n = n_req - n_sus,
                       k = n_verificate)
nedetectate  <- n_sus - detectate

an <- data.frame(
  zi = 1:n_zile,
  n_req, n_sus, n_verificate, detectate, nedetectate
)

cat("=== STATISTICI ANUALE ===\n")
cat(sprintf("Total cereri:           %d\n", sum(n_req)))
cat(sprintf("Total cereri suspecte:  %d\n", sum(n_sus)))
cat(sprintf("Total detectate:        %d\n", sum(detectate)))
cat(sprintf("Total nedetectate:      %d\n", sum(nedetectate)))
cat(sprintf("Total verficiari:       %d\n", sum(n_verificate)))

# Grafic: histograma cererilor suspecte pe zi
g <- ggplot(an, aes(x = n_sus)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Distributia numarului zilnic de cereri suspecte",
       subtitle = sprintf("p_sus = %.3f, lambda = %d", p_sus, lambda),
    x = "Cereri suspecte intr-o zi", y = "Numar de zile") +
  theme_minimal(base_size = 14)

print(g)
