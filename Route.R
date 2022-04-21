path <- getwd()  # Set the working directory as the current path

library(readr)
library(ggridges)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)

traffic_data <- read_csv("Traffic_Data.csv")

#Summary Statistics
traffic_data %>%
  group_by(Route) %>%
  get_summary_stats(Seconds, type = "mean_sd")


# Compute t-test
res <- t.test(Seconds ~ Route, data = traffic_data)
res

stat.test <- traffic_data %>% 
  t_test(Seconds ~ Route) %>%
  add_significance()
stat.test

stat.test2 <- traffic_data %>%
  t_test(Seconds ~ Route, var.equal = TRUE) %>%
  add_significance()
stat.test2

traffic_data %>%
  t_test(Seconds ~ Route, detailed = TRUE) %>%
  add_significance()

# Effect size
#Cohen's d for Student t-test
traffic_data %>%  cohens_d(Seconds ~ Route, var.equal = TRUE)

traffic_data %>%  cohens_d(Seconds ~ Route, var.equal = FALSE)

# Create side-by-side box-plots with t-test statistics
bxp <- ggboxplot(
  traffic_data, x = "Route", y = "Seconds", 
  ylab = "Time", xlab = "Route Home", add = "jitter"
)
# Add p-value and significance levels
stat.test <- stat.test %>% add_xy_position(x = "Route Home")
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))



# Side-by-side vertical boxplots of Traffic Routes with dots
ggplot(traffic_data, aes(x = Route, y = Seconds)) +
  geom_boxplot(aes(x = Route, group = Route), width = .25) +
  geom_dotplot(
    aes(x = Route, group = Route),
    binaxis = "y",
    binwidth = 2,
    stackdir = "center"
  ) 
  

#Plot stacked dotplot charts
ggplot(traffic_data, aes(x = Seconds)) + geom_dotplot(binwidth = 10)

ggplot(traffic_data, aes(x=Seconds)) +
  geom_dotplot(binwidth=5) +
  theme_bw() +
  facet_wrap(~ Route, ncol=1)