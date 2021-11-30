# Uriel Paluch
# 895700


# Librerias ---------------------------------------------------------------
library(tidyverse)

# Ejercicio 4.1 Camino de precios -----------------------------------------
set.seed(895700)
p0 <- 73
mu <- 0.13
sigma <- 0.1

anios <- 0.5

# Simulacion
m <- 1233

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

mean(P0aT[,ncol(P0aT)])
sd(P0aT[,ncol(P0aT)])


# 4.2 Probabilidad 1 ------------------------------------------------------

df_p0aT <- as.tibble(P0aT[,ncol(P0aT)])

a <- df_p0aT %>% filter(value > 38) %>%  filter(value < p0)

nrow(a) / nrow(df_p0aT)

# 4.3 Probabilidad 2 ------------------------------------------------------

df_p0aT <- as.tibble(P0aT[,ncol(P0aT)])

a <- df_p0aT %>% filter(value > mean(P0aT[,ncol(P0aT)]))

nrow(a) / nrow(df_p0aT)
