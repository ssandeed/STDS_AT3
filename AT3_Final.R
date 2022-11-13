# START
# Install Packages and Libraries
install.packages("rstanarm")
install.packages("kableExtra")
install.packages("performance")
install.packages("see")
install.packages("ggeffects")
install.packages("BayesPostEst")

# Install Libraries
library(kableExtra)
library(tidyverse)
library(rstanarm)
library(performance)
library(see)
library(bayesplot)
library(bayestestR)
library(ggeffects)
library(BayesPostEst)


# Read Data
df <- read.csv("lung_cancer.csv") 

# Selecting relevant features
df <- df[c(3,4,5,6,7)]


# Applying Bayesian GLM
model.glm.bayes <- stan_glm(Lung_cancer_death_rate ~ Dr_per_10000 + hos_bed_per_1000, data = df)
summary(model.glm.bayes)

mcmc_dens(model.glm.bayes)

# Plotting Dr_per_10000
model_plot <- ggplot(data = df, 
                     aes(Dr_per_10000, Lung_cancer_death_rate)) + 
  geom_point(size = 2, alpha = 0.9) +
  theme_bw()

model_plot

model.glm.bayes.df <- as_tibble(model.glm.bayes) %>% 
  rename(intercept = `(Intercept)`)
head(model.glm.bayes.df) %>% kableExtra::kable()

model_plot <- model_plot +
  geom_abline(data = model.glm.bayes.df,  # New data to plot
              aes(intercept = intercept, 
                  slope = Dr_per_10000), 
              alpha = 0.1, color = "gray50")

model_plot

model_plot <- model_plot +
  geom_abline(slope = mean(model.glm.bayes.df$Dr_per_10000), 
              intercept = mean(model.glm.bayes.df$intercept), 
              color = "brown", size = 1)
model_plot


# Plotting hos_bed_per_1000
model_plot <- ggplot(data = df, 
                     aes(hos_bed_per_1000, Lung_cancer_death_rate)) + 
  geom_point(size = 2, alpha = 0.9) +
  theme_bw()

model_plot

model.glm.bayes.df <- as_tibble(model.glm.bayes) %>% 
  rename(intercept = `(Intercept)`)
head(model.glm.bayes.df) %>% kableExtra::kable()

model_plot <- model_plot +
  geom_abline(data = model.glm.bayes.df,  # New data to plot
              aes(intercept = intercept, 
                  slope = hos_bed_per_1000), 
              alpha = 0.1, color = "gray50")
model_plot

model_plot <- model_plot +
  geom_abline(slope = mean(model.glm.bayes.df$hos_bed_per_1000), 
              intercept = mean(model.glm.bayes.df$intercept), 
              color = "red", size = 1)
model_plot

# Using informative priors of 10 countries
df_clean.prelim <- df[1:10,]  # Select the first 10 rows

model.glm.bayes.prelim <- stan_glm(Lung_cancer_death_rate ~ Dr_per_10000 + hos_bed_per_1000, data = df_clean.prelim) %>% invisible()

model.glm.bayes.prelim.df <- as_tibble(model.glm.bayes.prelim) %>% 
  rename(intercept = `(Intercept)`) %>% 
  slice_sample(n = 400)

# plot for Dr_per_10000
model_plot.prelim <- ggplot(df_clean.prelim, aes(Dr_per_10000, Lung_cancer_death_rate)) + 
  geom_abline(data = model.glm.bayes.prelim.df,
              aes(intercept = intercept, 
                  slope = Dr_per_10000), 
              alpha = 0.1, color = "gray50") + 
  geom_abline(slope = mean(model.glm.bayes.prelim.df$Dr_per_10000), 
              intercept = mean(model.glm.bayes.prelim.df$intercept), 
              color = "blue", size = 1) + 
  geom_point(size = 2, alpha = 0.9) + 
  theme_bw()

model_plot.prelim


cred_ints_95.prelim <- posterior_interval(model.glm.bayes.prelim, prob = 0.95) %>%
  as_tibble(rownames = 'parameter') %>%
  rename(cred_int_2.5 = `2.5%`,
         cred_int_97.5 = `97.5%`)

cred_int_plot.prelim <- ggplot(data = cred_ints_95.prelim,
                               aes(xmin = cred_int_2.5,
                                   xmax = cred_int_97.5,
                                   y = parameter)) +
  geom_errorbarh(height = 0.5) +
  
  geom_vline(xintercept = 0, size = 1.5, colour = 'darkred') +
  
  labs(x = 'parameter_value',
       title = 'Parameter estimate 95% credible intervals for lung cancer death rate by doctor density') +
  
  theme_bw()

cred_int_plot.prelim

summary(model.glm.bayes.prelim)
pp_check(model.glm.bayes.prelim, "dens_overlay")
p_direction(model.glm.bayes.prelim)



# Using informative priors for Dr_per_10000
my_prior <- normal(location = c(0.4),  # the reported mean
                   scale = c(0.6))  # the reported standard deviation

df_clean.new <- df[11:179,]  # The new data


model.glm.bayes.with_prior <- stan_glm(Lung_cancer_death_rate ~ Dr_per_10000 + hos_bed_per_1000, 
                                       data = df_clean.new,
                                       prior = my_prior) %>% invisible()

model.glm.bayes.with_prior.df <- as_tibble(model.glm.bayes.with_prior) %>% 
  rename(intercept = `(Intercept)`) %>% 
  slice_sample(n = 400)


model_plot.new <- ggplot(df_clean.new, aes(Dr_per_10000, Lung_cancer_death_rate)) + 
  geom_abline(data = model.glm.bayes.with_prior.df,
              aes(intercept = intercept, 
                  slope = Dr_per_10000), 
              alpha = 0.1, color = "gray50") + 
  geom_abline(slope = mean(model.glm.bayes.with_prior.df$Dr_per_10000), 
              intercept = mean(model.glm.bayes.with_prior.df$intercept), 
              color = "blue", size = 1) + 
  geom_point(size = 2, alpha = 0.9) + 
  theme_bw()

model_plot.new


cred_ints_95.with_prior <- posterior_interval(model.glm.bayes.with_prior, prob = 0.95) %>%
  as_tibble(rownames = 'parameter') %>%
  rename(cred_int_2.5 = `2.5%`,
         cred_int_97.5 = `97.5%`)

cred_int_plot.with_prior <- ggplot(data = cred_ints_95.with_prior,
                                   aes(xmin = cred_int_2.5,
                                       xmax = cred_int_97.5,
                                       y = parameter)) +
  geom_errorbarh(height = 0.5) +
  
  geom_vline(xintercept = 0, size = 1.5, colour = 'darkred') +
  
  labs(x = 'parameter_value',
       title = 'Parameter estimate 95% credible intervals for lung cancer death rate') +
  
  theme_bw()

cred_int_plot.with_prior

pp_check(model.glm.bayes.with_prior, "dens_overlay")
p_direction(model.glm.bayes.with_prior)

# ---------------------------------------------------

# plot for hos_bed_per_1000
# Using informative priors for hos_bed_per_1000
my_prior <- normal(location = c(2.8),  # the reported mean
                   scale = c(3.9))  # the reported standard deviation

df_clean.new <- df[11:179,]  # The new data


model.glm.bayes.with_prior <- stan_glm(Lung_cancer_death_rate ~ Dr_per_10000 + hos_bed_per_1000, 
                                       data = df_clean.new,
                                       prior = my_prior) %>% invisible()

model.glm.bayes.with_prior.df <- as_tibble(model.glm.bayes.with_prior) %>% 
  rename(intercept = `(Intercept)`) %>% 
  slice_sample(n = 400)


model_plot.new <- ggplot(df_clean.new, aes(hos_bed_per_1000, Lung_cancer_death_rate)) + 
  geom_abline(data = model.glm.bayes.with_prior.df,
              aes(intercept = intercept, 
                  slope = hos_bed_per_1000), 
              alpha = 0.1, color = "gray50") + 
  geom_abline(slope = mean(model.glm.bayes.with_prior.df$hos_bed_per_1000), 
              intercept = mean(model.glm.bayes.with_prior.df$intercept), 
              color = "blue", size = 1) + 
  geom_point(size = 2, alpha = 0.9) + 
  theme_bw()

model_plot.new


cred_ints_95.with_prior <- posterior_interval(model.glm.bayes.with_prior, prob = 0.95) %>%
  as_tibble(rownames = 'parameter') %>%
  rename(cred_int_2.5 = `2.5%`,
         cred_int_97.5 = `97.5%`)

cred_int_plot.with_prior <- ggplot(data = cred_ints_95.with_prior,
                                   aes(xmin = cred_int_2.5,
                                       xmax = cred_int_97.5,
                                       y = parameter)) +
  geom_errorbarh(height = 0.5) +
  
  geom_vline(xintercept = 0, size = 1.5, colour = 'darkred') +
  
  labs(x = 'parameter_value',
       title = 'Parameter estimate 95% credible intervals for lung cancer death rate') +
  
  theme_bw()

cred_int_plot.with_prior

pp_check(model.glm.bayes.with_prior, "dens_overlay")
p_direction(model.glm.bayes.with_prior)
#END
