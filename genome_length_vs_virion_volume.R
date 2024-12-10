
library(ggplot2)
library(janitor)
library(tidyr)
library(dplyr)

data <- read.csv("question-5-data/Cui_etal2014.csv") %>% 
  clean_names()

### Transforming data

log_data <- data %>% 
  mutate(log_virion_volume = log(virion_volume_nm_nm_nm),
         log_genome_length = log(genome_length_kb))

### Linear model

linear_model <- lm(data = log_data, log_virion_volume ~ log_genome_length)
summary(linear_model)

### Plot

genome_length_vs_virion_volume <- ggplot(data = log_data,
                                         aes(x = log_genome_length,
                                             y = log_virion_volume)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "log[Genome length (kb)]",
    y = "log[Virion volume (nm3)]") +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

genome_length_vs_virion_volume

### Saving image

library(ragg)

agg_png("question-5-data/genome_length_vs_virion_volume.png", 
        width = 15, 
        height = 15, 
        units = "cm", 
        res = 300, 
        scaling = 1)
genome_length_vs_virion_volume
dev.off()

### Transformed data

transformed_data <- ggplot(data = log_data,
                           aes(x = log_genome_length,
                               y = log_virion_volume)) +
  geom_point() +
  labs(x = "log[Genome length (kb)]",
       y = "log[Virion volume (nm3)]") +
    theme_bw() +
    theme(axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"))

agg_png("question-5-data/transformed_data.png", 
        width = 10, 
        height = 10, 
        units = "cm", 
        res = 300, 
        scaling = 1)
transformed_data
dev.off()

