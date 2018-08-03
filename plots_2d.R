library(MASS)
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
library(ggplot2)
library(deSolve)
library(purrrlyr)

pendulum = function(t, state, parameters) {
  list(c(state[['yp']], -parameters[['a']] * sin(state[['y']])))
}

linearized = function(t, state, parameters) {
  list(c(state[['yp']], -parameters[['a']] * state[['y']]))
}

as = abs(rnorm(100))
ts = seq(0, 4, length = 100)
y0 = c(y = 2.0 * pi / 4.0, yp = 0.0)
fout = lapply(as, function(a) { as_tibble(ode(y = y0, times = ts, func = full, parms = c(a = a))[,]) %>% mutate(a = a) }) %>% bind_rows
lout = as_tibble(ode(y = y0, times = ts, func = linearized, parms = c(a = a))[,])

fout %>% ggplot(aes(y, yp)) +
  geom_path(aes(group = a), alpha = 0.25, size = 0.5)

s = 1.0
m = 0.75
inits = list(y = c(rep(0, 20),
  seq(0, 1, length = 20),
  rep(1, 20),
  seq(0, 1, length = 20)),
  yp = c(seq(0, 1, length = 20),
         rep(0, 20),
         seq(0, 1, length = 20),
         rep(1, 20))) %>%
  as.tibble %>%
  mutate(y = y * s + m,
         yp = yp * s + m)

ode(y = y0, times = c(0.0, t), func = full, parms = c(a = 1.0))[2,2:3] %>% as.list
trajs = lapply(1:nrow(inits), function(i) {
  as_tibble(ode(y = c(y = inits[i, 1] %>% pull, yp = inits[i, 2] %>% pull), times = ts, func = full, parms = c(a = 1.0))[,]) %>%
    mutate(r = i)
  }) %>% bind_rows

trajs %>%
  ggplot(aes(y, yp)) +
  geom_path(aes(group = r), size = 0.5) +
  geom_point(data = trajs %>% filter(time == ts[[1]]), color = "blue", size = 0.5) +
  geom_point(data = trajs %>% filter(time == ts[[length(ts)]]), color = "red", size = 0.5)



inits %>%
  ggplot(aes(y, yp)) +
  geom_point()

