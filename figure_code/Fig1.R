library(ggplot2)
# age -----
df <- read.csv("figure_code/age.csv")
df$Age <- factor(df$Age, levels=c('<18', '18-29', '30-39','40-49', '>=50'))
#Var "Urban", "# Floors", "# Units per floor", "Floor area per capita" is denoted by "a", "b", "c", "d" respectively in age.csv. 
ggplot(df, aes(x = var, y= est))+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin=est_l, ymax=est_u, fill = Age, color=Age), size=0.6, width=0, position = position_dodge(width = 0.8))+
  scale_color_manual(values = c("#abd9e5", "#6DAFD7", "#4066f4", "#024690", "#181a60"))+
  
  geom_point(aes(color=Age), shape = 19, size = 1.8, position = position_dodge(width = 0.8))+
  scale_color_manual(values = c("#abd9e5", "#6DAFD7", "#4066f4", "#024690", "#181a60"))+
  
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 10, hjust = -0.1, vjust = -0.5),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))+
  scale_y_continuous(limit = c(-.5, 1), breaks = c(-.5, 0, .5, 1),
                     labels = c("-50%", "0", "50%", "100%"))+
  scale_x_discrete(expand=c(-0.8, -0.8), labels = c("Urban","# Floors", "# Units per floor",
                                                    "Floor area per capita"))+
  ylab("Change in odds ratio")

library(ggplot2)
# age_city -----
df <- read.csv("figure_code/age_city.csv")
df$Age <- factor(df$Age, levels=c('<18', '18-29', '30-39','40-49', '>=50'))
#Var "# Floors", "# Units per floor", "Floor area per capita" is denoted by "a", "b", "c" respectively in age_city.csv. 
ggplot(df, aes(x = var, y= est))+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin=est_l, ymax=est_u, fill = Age, color=Age), size=0.6, width=0, position = position_dodge(width = 0.8))+
  scale_color_manual(values = c("#abd9e5", "#6DAFD7", "#4066f4", "#024690", "#181a60"))+
  
  geom_point(aes(color=Age), shape = 19, size = 1.8, position = position_dodge(width = 0.8))+
  scale_color_manual(values = c("#abd9e5", "#6DAFD7", "#4066f4", "#024690", "#181a60"))+
  
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 10, hjust = -0.1, vjust = -0.5),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))+
  scale_y_continuous(limit = c(-.5, 1), breaks = c(-.5, 0, .5, 1),
                     labels = c("-50%", "0", "50%", "100%"))+
  scale_x_discrete(expand=c(-0.8, -0.8), labels = c("# Floors", "# Units per floor",
                                                    "Floor area per capita"))+
  ylab("Change in odds ratio")