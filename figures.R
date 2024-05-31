library(dplyr)
library(ggplot2);library(hrbrthemes)
library(tidyverse)
library(ggpubr)


vax_pasc_dat = read.csv("vax_pasc_tbl.csv")
vax_sev_dat = read.csv("vax_sev_tbl.csv")
sev_pasc_dat = read.csv("sev_pasc_tbl.csv")
# ind_effect_dat = read.csv("ind_effect_tbl.csv")
pasc_risk_age = read.csv("pasc_risk_age.csv")

vax_pasc_dat = vax_pasc_dat %>%
  mutate(significance = LB > 1 | UB < 1) %>%
  mutate(Age.groups = factor(Age.groups, 
                             levels = c("18-35", "35-70", "over 70")))

colors <- c("Fully vaccinated/Boosted over 6 months" = "#79AF9799", "Fully vaccinated/Boosted within 6 months" = "#B24740FF")
shapes <- c("TRUE" = 19, "FALSE" = 1)
p_vax_pasc = ggplot(vax_pasc_dat, aes(x = Age.groups, y = OR, group = Group)) +
  geom_point(aes(color = Group, shape = as.character(significance)), 
             size = 4, show.legend = FALSE)  +
  geom_line(aes(color = Group), size = 0.70,show.legend = FALSE ) + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  geom_ribbon(aes(ymin = LB, ymax = UB, fill = Group), 
              linetype = 2, 
              alpha = 0.1, show.legend = FALSE) + 
  scale_color_manual(values = colors) +  
  scale_fill_manual(values = colors) + 
  scale_shape_manual(values = shapes) +
  theme_ipsum_rc(grid="X", 
                 axis_title_size = 15,
                 axis_text_size = 12 ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size=13),
    axis.text.x = element_text(hjust = 1),
  ) +
  labs(
    x = "",
    y = "Odds Ratio"
  ) + ggtitle("PASC")

vax_sev_dat = vax_sev_dat %>%
  mutate(significance = LB > 1 | UB < 1) %>%
  mutate(Age.groups = factor(Age.groups, 
                             levels = c("18-35", "35-70", "over 70")))
p_vax_sev = ggplot(vax_sev_dat, aes(x = Age.groups, y = OR, group = Group)) +
  geom_point(aes(color = Group, shape = as.character(significance)), 
             size = 4, show.legend = FALSE)  +
  geom_line(aes(color = Group), size = 0.70) + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  geom_ribbon(aes(ymin = LB, ymax = UB, fill = Group), 
              linetype = 2, 
              alpha = 0.1, show.legend = FALSE) + 
  scale_color_manual(values = colors) +  
  scale_fill_manual(values = colors) + 
  scale_shape_manual(values = shapes) +
  facet_wrap(~Outcome) +
  theme_ipsum_rc(grid="X", 
                 axis_title_size = 15,
                 axis_text_size = 12 ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size=13),
    axis.text.x = element_text(hjust = 1),
  ) +
  labs(
    x = "Age groups",
    y = "Odds Ratio"
  ) + ggtitle("Severity")


colors_severity <- c("hospitalization" = "#79AF9799", "ICU/Ventilation" = "#B24740FF")
sev_pasc_dat = sev_pasc_dat %>%
  mutate(significance = LB > 1 | UB < 1) %>%
  mutate(Age.groups = factor(Age.groups, 
                             levels = c("18-35", "35-70", "over 70")))
p_sev_pasc = ggplot(sev_pasc_dat, aes(x = Age.groups, y = OR, group = Group)) +
  geom_point(aes(color = Group, shape = as.character(significance)), 
             size = 4, show.legend = FALSE)  +
  geom_line(aes(color = Group), size = 0.70) + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  geom_ribbon(aes(ymin = LB, ymax = UB, fill = Group), 
              linetype = 2, 
              alpha = 0.1, show.legend = FALSE) + 
  scale_color_manual(values = colors_severity) +  
  scale_fill_manual(values = colors_severity) + 
  scale_shape_manual(values = shapes) +
  theme_ipsum_rc(grid="X", 
                 axis_title_size = 15,
                 axis_text_size = 12 ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size=13),
    axis.text.x = element_text(hjust = 1)
  ) +
  labs(
    x = "Age groups",
    y = "Odds Ratio"
  ) +
  guides(fill = guide_legend(title = "Vaccination"))



p_vax = ggarrange(p_vax_pasc, p_vax_sev, nrow = 2, 
                  heights = c(1, 1),
                  align = "v")
 
p_vax
png(
  filename="P:/PASC_retro/Vaccine/output/vax_effect_or_plots.png",
  width=10, height=8, units="in", res=300)

print(p_vax)
dev.off()




####absolute risks
#pasc_risk_age_dat = pasc_risk_age[,c(1,7,15,17,12)]
pasc_risk_age_dat = pasc_risk_age[,c(1,7,9,11)]
pasc_risk_age_dat = pasc_risk_age_dat %>%
  mutate(Age.groups = factor(Age.groups, 
                             levels = c("18-35","35-70","over 70")))%>%
  pivot_longer(cols = -Age.groups, names_to = "variable", values_to = "value") 

colnames(pasc_risk_age_dat) = c("Age.groups", "group", "value")
pasc_risk_age_dat = pasc_risk_age_dat %>% mutate(
  group = case_when(
    group == "absolute.hospitalization.risk" ~ "Hospitalization",
    group == "absolute.icuventilation.risk" ~ "ICU/Ventilation",
    group == "absolute.PASC.incident.risk" ~ "PASC"#,group == "percent.vaccine.within.6month.by.age" ~ "Vaccinated within 6 months"
  )
)

colors_risk <- c("Hospitalization" = "#374E55FF", 
                 "ICU/Ventilation" = "#DF8F44FF",
                 "PASC" = "#00A1D5FF"#,"Vaccinated within 6 months" = "#B24745FF"
                 )
p_pasc_risk = ggplot(pasc_risk_age_dat, aes(x = Age.groups, y = value, group = group)) +
  geom_point(aes(color = group), 
             size = 4, show.legend = FALSE)  +
  geom_line(aes(color = group), size = 0.70) + 
  scale_color_manual(values = colors_risk) +  
  scale_fill_manual(values = colors_risk) + 
  theme_ipsum_rc(grid="X", 
                 axis_title_size = 15,
                 axis_text_size = 12 ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size=13),
    axis.text.x = element_text(hjust = 1),
  ) +
  labs(
    x = "Age groups",
    y = "Absolute Risk"
  ) 

p_pasc_risk

# direct_dat = vax_pasc_dat[vax_pasc_dat$Group != "Fully vaccinated/Boosted over 6 months",][,c(2,3,5,6,7,8)]
# direct_dat$effect = "Direct"
# ind_dat = ind_effect_dat[,c(9, 11,10, 2,1)]
# ind_dat$effect = "Indirect"
# colnames(ind_dat) = colnames(direct_dat)
# ind_dat = ind_dat %>% 
#   mutate(effect = str_c(effect, "-", Outcome))
# direct_indirect_effect = rbind(ind_dat,direct_dat)
# direct_indirect_effect = direct_indirect_effect %>%
#   mutate(Age.groups = factor(Age.groups, 
#                              levels = c("under 30", "30-40", "40-50",
#                                         "50-60", "60-70",  "over 70")))
# colors_effect <- c("Indirect-hospitalization" = "#79AF9799", "Indirect-ICU/Ventilation" = "#B24740FF", "Direct" = "grey")
# ggplot(direct_indirect_effect, aes(x = Age.groups, y = OR, group = effect)) +
#   geom_point(aes(color = effect, ), 
#              size = 4, show.legend = FALSE)  +
#   geom_line(aes(color = effect), size = 0.70) + 
#   geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
#   scale_color_manual(values = colors_effect) +  
#   scale_fill_manual(values = colors_effect) + 
#   theme_ipsum_rc(grid="X", 
#                  axis_title_size = 15,
#                  axis_text_size = 12 ) +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     legend.text = element_text(size=13),
#     axis.text.x = element_text(hjust = 1),
#   ) +
#   labs(
#     x = "Age groups",
#     y = "Odds Ratio"
#   ) +
#   guides(fill = guide_legend(title = "Vaccination"))
# 
# 
# 
# 
