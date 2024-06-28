## Jaroslaw Kantorowicz and Bastián González-Bustamante
## Faculty of Governance and Global Affairs, Leiden University
## June 2024

## Packages
library(ggplot2)

## Data
data <- read.csv("data/tidy/summary_UK_US.csv", sep = ",", encoding = "UTF-8")
bootstrap <- 30
data$model <- ifelse(data$model == "ct", "Chronologically Trained Model",
                     ifelse(data$model == "naive", "Naive Time Model",
                            ifelse(data$model == "overlapping", "Overlapping Model", 
                                   ifelse(data$model == "ct-alt", "Alt. Chronologically Trained", 
                                          ifelse(data$model == "distilBERT", "DistilBERT Embeddings", data$model)))))

## 95% CIs
data$se <- data$std / sqrt(bootstrap)
data$lower_ci <- data$cosine - 1.96 * data$se
data$upper_ci <- data$cosine + 1.96 * data$se

## Chronologically Trained Model UK
pdf("results/grey_plots/01_CT_UK.pdf", width = 6.826666666666667, height = 5.12)
png("results/grey_plots/01_CT_UK.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = period, y = cosine, color = cluster, group = cluster), 
       data = subset(data, model == "Chronologically Trained Model" & country == "UK" 
                     & cluster != "Corruption" & cluster != "Procedural - Judiciary (ext)" & cluster != "Procedural - Rules (ext)" & cluster != "Substantive - Democracy (ext)")) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(0.3)) +
  geom_point(aes(shape = cluster), size = 1.5, position = position_dodge(0.3)) + 
  geom_line(aes(color = cluster, linetype = cluster), position = position_dodge(0.3), size = 0.5) +
  scale_colour_grey() +
  theme_classic() + coord_cartesian(expand = TRUE, ylim = c(0.2, 0.8)) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = NULL, y = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

## Chronologically Trained Model US
pdf("results/grey_plots/02_CT_US.pdf", width = 6.826666666666667, height = 5.12)
png("results/grey_plots/02_CT_US.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = period, y = cosine, color = cluster, group = cluster), 
       data = subset(data, model == "Chronologically Trained Model" & country == "US" 
                     & cluster != "Corruption" & cluster != "Procedural - Judiciary (ext)" & cluster != "Procedural - Rules (ext)" & cluster != "Substantive - Democracy (ext)")) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(0.3)) +
  geom_point(aes(shape = cluster), size = 1.5, position = position_dodge(0.3)) + 
  geom_line(aes(color = cluster, linetype = cluster), position = position_dodge(0.3), size = 0.5) +
  scale_colour_grey() +
  theme_classic() + coord_cartesian(expand = TRUE, ylim = c(0.2, 0.8)) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = NULL, y = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

## Chronologically Trained Model US full
pdf("results/grey_plots/03_CT_full_US.pdf", width = 6.826666666666667, height = 5.12)
png("results/grey_plots/03_CT_full_US.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = period, y = cosine, color = cluster, group = cluster), 
       data = subset(data, model == "Chronologically Trained Model" & country == "US_full"
                     & cluster != "Corruption" & cluster != "Procedural - Judiciary (ext)" & cluster != "Procedural - Rules (ext)" & cluster != "Substantive - Democracy (ext)")) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(0.3)) +
  geom_point(aes(shape = cluster), size = 1.5, position = position_dodge(0.3)) + 
  geom_line(aes(color = cluster, linetype = cluster), position = position_dodge(0.3), size = 0.5) +
  scale_colour_grey() +
  theme_classic() + coord_cartesian(expand = TRUE, ylim = c(0.2, 0.8)) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = NULL, y = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

## Alt. Chronologically Trained Model UK
pdf("results/grey_plots/A1_CT_alt_UK.pdf", width = 6.826666666666667, height = 5.12)
png("results/grey_plots/A1_CT_alt_UK.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = period, y = cosine, color = cluster, group = cluster), 
       data = subset(data, model == "Alt. Chronologically Trained" & country == "UK"
                     & cluster != "Corruption" & cluster != "Procedural - Judiciary (ext)" & cluster != "Procedural - Rules (ext)" & cluster != "Substantive - Democracy (ext)")) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(0.3)) +
  geom_point(aes(shape = cluster), size = 1.5, position = position_dodge(0.3)) + 
  geom_line(aes(color = cluster, linetype = cluster), position = position_dodge(0.3), size = 0.5) +
  scale_colour_grey() +
  theme_classic() + coord_cartesian(expand = TRUE, ylim = c(0.2, 0.8)) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = NULL, y = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

## Alt. Chronologically Trained Model US
pdf("results/grey_plots/A2_CT_alt_US.pdf", width = 6.826666666666667, height = 5.12)
png("results/grey_plots/A2_CT_alt_US.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = period, y = cosine, color = cluster, group = cluster), 
       data = subset(data, model == "Alt. Chronologically Trained" & country == "US"
                     & cluster != "Corruption" & cluster != "Procedural - Judiciary (ext)" & cluster != "Procedural - Rules (ext)" & cluster != "Substantive - Democracy (ext)")) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(0.3)) +
  geom_point(aes(shape = cluster), size = 1.5, position = position_dodge(0.3)) + 
  geom_line(aes(color = cluster, linetype = cluster), position = position_dodge(0.3), size = 0.5) +
  scale_colour_grey() +
  theme_classic() + coord_cartesian(expand = TRUE, ylim = c(0.2, 0.8)) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = NULL, y = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

## Alt. Chronologically Trained Model US full
pdf("results/grey_plots/A3_CT_alt_full_US.pdf", width = 6.826666666666667, height = 5.12)
png("results/grey_plots/A3_CT_alt_full_US.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = period, y = cosine, color = cluster, group = cluster), 
       data = subset(data, model == "Alt. Chronologically Trained" & country == "US_full"
                     & cluster != "Corruption" & cluster != "Procedural - Judiciary (ext)" & cluster != "Procedural - Rules (ext)" & cluster != "Substantive - Democracy (ext)")) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(0.3)) +
  geom_point(aes(shape = cluster), size = 1.5, position = position_dodge(0.3)) + 
  geom_line(aes(color = cluster, linetype = cluster), position = position_dodge(0.3), size = 0.5) +
  scale_colour_grey() +
  theme_classic() + coord_cartesian(expand = TRUE, ylim = c(0.2, 0.8)) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = NULL, y = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

## DistilBERT UK
pdf("results/grey_plots/A4_distilBERT_UK.pdf", width = 6.826666666666667, height = 5.12)
png("results/grey_plots/A4_distilBERT_UK.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = period, y = cosine, color = cluster, group = cluster), 
       data = subset(data, model == "DistilBERT Embeddings" & country == "UK"
                     & cluster != "Corruption" & cluster != "Procedural - Judiciary (ext)" & cluster != "Procedural - Rules (ext)" & cluster != "Substantive - Democracy (ext)")) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(0.3)) +
  geom_point(aes(shape = cluster), size = 1.5, position = position_dodge(0.3)) + 
  geom_line(aes(color = cluster, linetype = cluster), position = position_dodge(0.3), size = 0.5) +
  scale_colour_grey() +
  theme_classic() + coord_cartesian(expand = TRUE, ylim = c(0.2, 0.8)) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = NULL, y = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

## DistilBERT US (pending)
## A5

## Chronologically Trained Model UK
pdf("results/grey_plots/A6_CT_UK.pdf", width = 6.826666666666667, height = 5.12)
png("results/grey_plots/A6_CT_UK.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = period, y = cosine, color = cluster, group = cluster), 
       data = subset(data, model == "Chronologically Trained Model" & country == "UK"
                     & (cluster == "Substantive - Rights" | cluster == "Procedural - Judiciary (ext)" | cluster == "Procedural - Rules (ext)" | cluster == "Substantive - Democracy (ext)"))) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(0.3)) +
  geom_point(aes(shape = cluster), size = 1.5, position = position_dodge(0.3)) + 
  geom_line(aes(color = cluster, linetype = cluster), position = position_dodge(0.3), size = 0.5) +
  scale_colour_grey() +
  theme_classic() + coord_cartesian(expand = TRUE, ylim = c(0.2, 0.8)) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = NULL, y = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

## Chronologically Trained Model US
pdf("results/grey_plots/A7_CT_US.pdf", width = 6.826666666666667, height = 5.12)
png("results/grey_plots/A7_CT_US.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = period, y = cosine, color = cluster, group = cluster), 
       data = subset(data, model == "Chronologically Trained Model" & country == "US"
                     & (cluster == "Substantive - Rights" | cluster == "Procedural - Judiciary (ext)" | cluster == "Procedural - Rules (ext)" | cluster == "Substantive - Democracy (ext)"))) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(0.3)) +
  geom_point(aes(shape = cluster), size = 1.5, position = position_dodge(0.3)) + 
  geom_line(aes(color = cluster, linetype = cluster), position = position_dodge(0.3), size = 0.5) +
  scale_colour_grey() +
  theme_classic() + coord_cartesian(expand = TRUE, ylim = c(0.2, 0.8)) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = NULL, y = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

## Chronologically Trained Model US full
pdf("results/grey_plots/A8_CT_full_US.pdf", width = 6.826666666666667, height = 5.12)
png("results/grey_plots/A8_CT_full_US.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = period, y = cosine, color = cluster, group = cluster), 
       data = subset(data, model == "Chronologically Trained Model" & country == "US_full"
                     & (cluster == "Substantive - Rights" | cluster == "Procedural - Judiciary (ext)" | cluster == "Procedural - Rules (ext)" | cluster == "Substantive - Democracy (ext)"))) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(0.3)) +
  geom_point(aes(shape = cluster), size = 1.5, position = position_dodge(0.3)) + 
  geom_line(aes(color = cluster, linetype = cluster), position = position_dodge(0.3), size = 0.5) +
  scale_colour_grey() +
  theme_classic() + coord_cartesian(expand = TRUE, ylim = c(0.2, 0.8)) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = NULL, y = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()
