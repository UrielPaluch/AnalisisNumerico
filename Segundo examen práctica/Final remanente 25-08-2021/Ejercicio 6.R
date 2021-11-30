# Uriel Paluch
# 895700
# Ejercicio 6



# Ejercicio A -------------------------------------------------------------



set.seed(895096)

p0 <- 75
mu <- 0.15
sigma <- 0.2

anios <- 0.5

# Simulacion
m <- 1000

# Time steps
n <- 182.5

dt <- anios/n

# Matriz de camino de precios
P0aT <- matrix(NA, nrow = m, ncol = n+1)

P0aT[,1] <- p0

for (i in 1:m) {
  for(t in 2:(n+1)){
    P0aT[i,t] <- P0aT[i, t-1] * exp((mu-0.5*sigma^2) * dt + sigma * sqrt(dt) * rnorm(1))
  }
}

ggplot() +
  geom_histogram(aes(P0aT[,ncol(P0aT)]), binwidth = 5) +
  xlab("Price") + ylab("Times")


# Ejercicio b -------------------------------------------------------------

df_p0aT <- as.tibble(P0aT[,ncol(P0aT)])

a <- df_p0aT %>% filter(value > 65) %>%  filter(value < p0)

nrow(a) / nrow(df_p0aT)


# Ejercicio c -------------------------------------------------------------

a <- df_p0aT %>%  filter(value < mean(P0aT[,ncol(P0aT)]))
nrow(a) / nrow(df_p0aT)
