## Bastián González-Bustamante and Jaroslaw Kantorowicz
## Faculty of Governance and Global Affairs, Leiden University
## August 2024

## Packages
library(tidyverse)
library(ggplot2)

## Data
parlamint <- read.csv("data/tidy/summary_estimates_ParlaMint.csv", sep = ",", encoding = "UTF-8")
wjp <- read.csv("data/tidy/wjp_recalculations.csv", sep = ";")
bootstrap <- 30

## 95% CIs
parlamint$se <- parlamint$std / sqrt(bootstrap)
parlamint$lower_ci <- parlamint$cosine - 1.96 * parlamint$se
parlamint$upper_ci <- parlamint$cosine + 1.96 * parlamint$se

pdf("results/figures/crosslingual_Great_Britain.pdf", width = 6.826666666666667, height = 5.12)
png("results/figures/crosslingual_Great_Britain.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = cosine, y = factor(cluster, levels = rev(levels(factor(cluster)))),
           color = model, group = model), data = subset(parlamint, country == "Great Britain")) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.2, position = position_dodge(0.3)) +
  geom_point(shape = 18, size = 1.5, position = position_dodge(0.3)) + 
  scale_colour_grey(name = "Model") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom") +
  coord_cartesian(expand = TRUE, xlim = c(0, 1)) +
  theme(panel.grid.minor = element_blank()) +
  labs(y = NULL, x = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

pdf("results/figures/crosslingual_France.pdf", width = 6.826666666666667, height = 5.12)
png("results/figures/crosslingual_France.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = cosine, y = factor(cluster, levels = rev(levels(factor(cluster)))),
           color = model, group = model), data = subset(parlamint, country == "France")) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.2, position = position_dodge(0.3)) +
  geom_point(shape = 18, size = 1.5, position = position_dodge(0.3)) + 
  scale_colour_grey(name = "Model") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom") +
  coord_cartesian(expand = TRUE, xlim = c(0, 1)) +
  theme(panel.grid.minor = element_blank()) +
  labs(y = NULL, x = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

pdf("results/figures/crosslingual_Germany.pdf", width = 6.826666666666667, height = 5.12)
png("results/figures/crosslingual_Germany.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = cosine, y = factor(cluster, levels = rev(levels(factor(cluster)))),
           color = model, group = model), data = subset(parlamint, country == "Germany")) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.1, position = position_dodge(0.3)) +
  geom_point(shape = 18, size = 1.5, position = position_dodge(0.3)) + 
  scale_colour_grey(name = "Model") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom") +
  coord_cartesian(expand = TRUE, xlim = c(0, 1)) +
  theme(panel.grid.minor = element_blank()) +
  labs(y = NULL, x = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

pdf("results/figures/crosslingual_Netherlands.pdf", width = 6.826666666666667, height = 5.12)
png("results/figures/crosslingual_Netherlands.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = cosine, y = factor(cluster, levels = rev(levels(factor(cluster)))),
           color = model, group = model), data = subset(parlamint, country == "Netherlands")) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.2, position = position_dodge(0.3)) +
  geom_point(shape = 18, size = 1.5, position = position_dodge(0.3)) + 
  scale_colour_grey(name = "Model") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom") +
  coord_cartesian(expand = TRUE, xlim = c(0, 1)) +
  theme(panel.grid.minor = element_blank()) +
  labs(y = NULL, x = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

pdf("results/figures/crosslingual_Italy.pdf", width = 6.826666666666667, height = 5.12)
png("results/figures/crosslingual_Italy.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = cosine, y = factor(cluster, levels = rev(levels(factor(cluster)))),
           color = model, group = model), data = subset(parlamint, country == "Italy")) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.2, position = position_dodge(0.3)) +
  geom_point(shape = 18, size = 1.5, position = position_dodge(0.3)) + 
  scale_colour_grey(name = "Model") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom") +
  coord_cartesian(expand = TRUE, xlim = c(0, 1)) +
  theme(panel.grid.minor = element_blank()) +
  labs(y = NULL, x = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

pdf("results/figures/crosslingual_Spain.pdf", width = 6.826666666666667, height = 5.12)
png("results/figures/crosslingual_Spain.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = cosine, y = factor(cluster, levels = rev(levels(factor(cluster)))),
           color = model, group = model), data = subset(parlamint, country == "Spain")) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.2, position = position_dodge(0.3)) +
  geom_point(shape = 18, size = 1.5, position = position_dodge(0.3)) + 
  scale_colour_grey(name = "Model") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom") +
  coord_cartesian(expand = TRUE, xlim = c(0, 1)) +
  theme(panel.grid.minor = element_blank()) +
  labs(y = NULL, x = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

pdf("results/figures/crosslingual_Hungary.pdf", width = 6.826666666666667, height = 5.12)
png("results/figures/crosslingual_Hungary.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = cosine, y = factor(cluster, levels = rev(levels(factor(cluster)))),
           color = model, group = model), data = subset(parlamint, country == "Hungary")) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.2, position = position_dodge(0.3)) +
  geom_point(shape = 18, size = 1.5, position = position_dodge(0.3)) + 
  scale_colour_grey(name = "Model") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom") +
  coord_cartesian(expand = TRUE, xlim = c(0, 1)) +
  theme(panel.grid.minor = element_blank()) +
  labs(y = NULL, x = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

pdf("results/figures/crosslingual_Poland.pdf", width = 6.826666666666667, height = 5.12)
png("results/figures/crosslingual_Poland.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = cosine, y = factor(cluster, levels = rev(levels(factor(cluster)))),
           color = model, group = model), data = subset(parlamint, country == "Poland")) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.2, position = position_dodge(0.3)) +
  geom_point(shape = 18, size = 1.5, position = position_dodge(0.3)) + 
  scale_colour_grey(name = "Model") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom") +
  coord_cartesian(expand = TRUE, xlim = c(0, 1)) +
  theme(panel.grid.minor = element_blank()) +
  labs(y = NULL, x = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

pdf("results/figures/crosslingual_judiciary.pdf", width = 6.826666666666667, height = 5.12)
png("results/figures/crosslingual_judiciary.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = cosine, y = factor(country, levels = rev(levels(factor(country)))), color = model, group = model), 
       data = subset(parlamint, cluster == "Procedural - Judiciary")) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.2, position = position_dodge(0.8)) +
  geom_point(shape = 18, size = 2, position = position_dodge(0.8)) + 
  scale_colour_grey(name = "Model") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom") +
  coord_cartesian(expand = TRUE, xlim = c(0, 1)) +
  theme(panel.grid.minor = element_blank()) +
  labs(y = NULL, x = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

pdf("results/figures/crosslingual_rules.pdf", width = 6.826666666666667, height = 5.12)
png("results/figures/crosslingual_rules.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = cosine, y = factor(country, levels = rev(levels(factor(country)))), color = model, group = model), 
       data = subset(parlamint, cluster == "Procedural - Rules")) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.2, position = position_dodge(0.8)) +
  geom_point(shape = 18, size = 2, position = position_dodge(0.8)) + 
  scale_colour_grey(name = "Model") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom") +
  coord_cartesian(expand = TRUE, xlim = c(0, 1)) +
  theme(panel.grid.minor = element_blank()) +
  labs(y = NULL, x = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

pdf("results/figures/crosslingual_democracy.pdf", width = 6.826666666666667, height = 5.12)
png("results/figures/crosslingual_democracy.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = cosine, y = factor(country, levels = rev(levels(factor(country)))), color = model, group = model), 
       data = subset(parlamint, cluster == "Substantive - Democracy")) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.2, position = position_dodge(0.8)) +
  geom_point(shape = 18, size = 2, position = position_dodge(0.8)) + 
  scale_colour_grey(name = "Model") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom") +
  coord_cartesian(expand = TRUE, xlim = c(0, 1)) +
  theme(panel.grid.minor = element_blank()) +
  labs(y = NULL, x = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

pdf("results/figures/crosslingual_rights.pdf", width = 6.826666666666667, height = 5.12)
png("results/figures/crosslingual_rights.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(aes(x = cosine, y = factor(country, levels = rev(levels(factor(country)))), color = model, group = model), 
       data = subset(parlamint, cluster == "Substantive - Rights")) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.2, position = position_dodge(0.8)) +
  geom_point(shape = 18, size = 2, position = position_dodge(0.8)) + 
  scale_colour_grey(name = "Model") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom") +
  coord_cartesian(expand = TRUE, xlim = c(0, 1)) +
  theme(panel.grid.minor = element_blank()) +
  labs(y = NULL, x = "Cosine Similarity", title = NULL, subtitle = NULL, caption = NULL)
dev.off()

## Recoding WJP
wjp$constraints <- as.numeric(str_replace_all(wjp$Factor.1..Constraints.on.Government.Powers, ",", "."))
wjp$corruption <- as.numeric(str_replace_all(wjp$Factor.2..Absence.of.Corruption, ",", "."))
wjp$open_government <- as.numeric(str_replace_all(wjp$Factor.3..Open.Government, ",", "."))
wjp$rights <- as.numeric(str_replace_all(wjp$Factor.4..Fundamental.Rights, ",", "."))
wjp$security <- as.numeric(str_replace_all(wjp$Factor.5..Order.and.Security, ",", "."))
wjp$regulations <- as.numeric(str_replace_all(wjp$Factor.6..Regulatory.Enforcement, ",", "."))
wjp$civil_justice <- as.numeric(str_replace_all(wjp$Factor.7..Civil.Justice, ",", "."))
wjp$criminal_justice <- as.numeric(str_replace_all(wjp$Factor.8..Criminal.Justice, ",", "."))
wjp <- wjp %>% mutate(justice = (criminal_justice + civil_justice)/2)
wjp$WJP <- as.numeric(str_replace_all(wjp$WJP.Rule.of.Law.Index..Overall.Score, ",", "."))

wjp$constraints_weight <- 0.38/3.22 ## accountability 0.1180124
wjp$corruption_weight <- 0.46/3.22 ## corruption 0.1428571
wjp$open_government_weight <- 0.39/3.22 ## open government 0.121118
wjp$human_rights_weight <- 0.61/3.22 ## human rights 0.189441
wjp$security_weight <- 0.39/3.22 ## security 0.121118
wjp$regulations_weight <- 0.33/3.22 ## regulations 0.1024845
wjp$justice_weight <- 0.66/3.22 ## judicial independence 0.2049689

wjp <- wjp %>% mutate(WJP_Recalculated = constraints_weight*constraints +
                        corruption_weight * corruption +
                        open_government_weight * open_government +
                        human_rights_weight * rights +
                        security_weight * security +
                        regulations_weight * regulations +
                        justice_weight * justice)

wjp <- wjp %>% mutate(WJP_difference = abs(WJP - WJP_Recalculated))

wjp_order <- wjp %>% arrange(desc(WJP_difference))

wjp_selected <- wjp %>% filter(Country.Code == "POL" | 
                                 Country.Code == "HUN" | 
                                 Country.Code == "ITA" | 
                                 Country.Code == "FRA" | 
                                 Country.Code == "DEU" | 
                                 Country.Code == "ESP" | 
                                 Country.Code == "NLD" | 
                                 Country.Code == "GBR")

wjp_selected <- wjp_selected[order(-wjp_selected$WJP),]

wjp_selected <- wjp_selected %>% arrange(desc(WJP))

pdf("results/figures/recalculation_main.pdf", width = 6.826666666666667, height = 5.12)
png("results/figures/recalculation_main.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(wjp_selected, aes(y = reorder(Country.Code, WJP))) +
  geom_segment(aes(yend = Country.Code, x = WJP, xend = WJP_Recalculated)) +
  geom_point(aes(x = WJP, color = "WJP"), size = 3) + 
  geom_point(aes(x = WJP_Recalculated, color = "WJP Recalculated"), size = 4, shape = 18, alpha = 0.7) +  
  scale_colour_manual(name = NULL,  values = c("WJP Recalculated" = "black", "WJP" = "grey")) +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "bottom") +
  coord_cartesian(expand = TRUE, xlim = c(0, 1)) + 
  theme(panel.grid.minor = element_blank()) +
  labs(y = NULL, x = "Rule of Law Index", title = NULL, subtitle = NULL, caption = NULL) +
  scale_y_discrete(labels = c("NLD" = "Netherlands", "DEU" = "Germany", "GBR" = "Great Britain",
                              "FRA" = "France", "ESP" = "Spain", "ITA" = "Italy", "POL" = "Poland",
                              "HUN" = "Hungary"))
dev.off()

pdf("results/figures/recalculation_addition.pdf", width = 6.826666666666667, height = 5.12)
png("results/figures/recalculation_addition.png", width = (1024*2), height = (768*2), units = 'px', res = 300)
ggplot(wjp_order[c(1:10),], aes(y = reorder(Country.Code, WJP))) +
  geom_segment(aes(yend = Country.Code, x = WJP, xend = WJP_Recalculated)) +
  geom_point(aes(x = WJP, color = "WJP"), size = 3) + 
  geom_point(aes(x = WJP_Recalculated, color = "WJP Recalculated"), size = 4, shape = 18, alpha = 0.7) +  
  scale_colour_manual(name = NULL,  values = c("WJP Recalculated" = "black", "WJP" = "grey")) +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "bottom") +
  coord_cartesian(expand = TRUE, xlim = c(0, 1)) + 
  theme(panel.grid.minor = element_blank()) +
  labs(y = NULL, x = "Rule of Law Index", title = NULL, subtitle = NULL, caption = NULL) +
  scale_y_discrete(labels = c("PRT" = "Portugal", "CRI" = "Costa Rica", "ARE" = "United Arab Emirates",
                              "HRV" = "Croatia", "GEO" = "Georgia", "MNE" = "Montenegro", "PAN" = "Panama",
                              "CHN" = "China", "IRN" = "Iran", "VEN" = "Venezuela"))
dev.off()
