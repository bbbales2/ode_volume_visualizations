library(MASS)
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
library(ggplot2)
library(deSolve)

a = rnorm(2000)

f1 = a * 1
f3 = a * 3

list(f1 = f1, f3 = f3) %>%
  as.tibble %>%
  gather(which, value) %>%
  ggplot(aes(value)) +
  geom_density(aes(colour = which, fill = which), alpha = 0.5)

ts = seq(1.0, 5.0, length = 100)

get_density_plot = function(f, ts, a, a_sample = c()) {
  if(length(a_sample) == 0) {
    a_sample = sample(a, 20)
  }
  
  lapply(ts, function(t) { list(t = t, f = f(t, a)) %>% as.tibble }) %>%
    bind_rows %>%
    group_by(t) %>%
    summarize(fm2 = quantile(f, 0.021),
              fm1 = quantile(f, 0.136),
              median = quantile(f, 0.50),
              fp1 = quantile(f, 0.864),
              fp2 = quantile(f, 0.979)) %>%
    ggplot(aes(t)) +
    geom_ribbon(aes(ymin = fm2, ymax = fm1), alpha = 0.25) +
    geom_ribbon(aes(ymin = fm1, ymax = median), alpha = 0.50) +
    geom_ribbon(aes(ymin = median, ymax = fp1), alpha = 0.50) +
    geom_ribbon(aes(ymin = fp1, ymax = fp2), alpha = 0.25) +
    geom_line(data = lapply(1:length(a_sample), function(s) { list(s = s, t = ts, f = f(ts, a_sample[[s]])) %>% as.tibble }) %>% bind_rows,
              aes(t, f, group = s), color = "red", alpha = 0.5, size = 0.5) +
    ggtitle(paste(trimws(deparse(f)), collapse = ''))
}

get_density_plot(function(t, a) { a * t }, ts, a)

get_density_plot(function(t, a) { exp(a * t / 10.0) }, ts, a)

get_density_plot(function(t, a) { sin(a / 2.0 + 2 * t) }, ts, a)

get_density_plot(function(t, a) { a + sin(2 * t) }, ts, a)
