library(flexdashboard)
library(shiny)
library(gganimate)
library(plotly)
library(tidyverse)
library(rpart)
library(rattle)
library(ggplot2)

data <- read.csv("~/pecan/shiny/Terrestial_30min/terrestrial_30min_example.csv")
data <- filter(data, variable=='nee' & observation!="NA" & site_id=="BART")
daily_data <- data %>%
  group_by(date) %>%
  summarise(
    mean = mean(mean),
    quantile02.5 = mean(quantile02.5),
    quantile97.5 = mean(quantile97.5),
    observation = mean(observation)
  )
x.breaks = daily_data$date
labels = rev(seq(from = 1, to = length(x.breaks), by =1))

p <- ggplot(daily_data, aes(group = 1)) +
  geom_ribbon(aes(x = as.factor(date), ymin = quantile02.5, ymax = quantile97.5, fill = "95% Confidence Interval"), alpha = 0.4) +
  geom_line(aes(x = as.factor(date), y = mean, color = "Predicted")) +
  geom_line(aes(x = as.factor(date), y = observation, color = "Observed Data"), size = 1) +  
  ggtitle(paste0("Forecast Horizon for ", "Test")) +
  scale_color_manual(name = "Legend", labels = c("Observed Data", "Predicted"), values = c("Observed Data" = "firebrick4", "Predicted" = "skyblue1")) +
  scale_fill_manual(labels = c("95% Confidence Interval"), values = c("95% Confidence Interval" = "blue1")) +
  scale_y_continuous(name = "NEE (kg C m-2 s-1)") +
  scale_x_discrete(name = "Days from Observed Date", breaks = x.breaks, labels = labels) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12), legend.title = element_blank(), legend.text = element_text(size = 10), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 11), axis.title.y = element_text(size = 12))

p

