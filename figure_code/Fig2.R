library(ggplot2)
# income -----
df <- read.csv("figure_code/income.csv")
df$Income <- factor(df$Income, levels=c('Low', 'Medium', 'High'))
#Var "Urban", "# Floors", "# Units per floor", "Floor area per capita" is denoted by "a", "b", "c", "d" respectively in income.csv. 
ggplot(df, aes(x = var, y= est))+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin=est_l, ymax=est_u, fill = Income, color=Income), size=0.6, width=0, position = position_dodge(width = 0.8))+
  scale_color_manual(values = c("#f5ac7a", "#f29683", "#db595e"))+
  
  geom_point(aes(color=Income), shape = 19, size = 1.8, position = position_dodge(width = 0.8))+
  scale_color_manual(values = c("#f5ac7a", "#f29683", "#db595e"))+
  
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
  scale_y_continuous(limit = c(-.25, 0.5), breaks = c(-.25, 0, .25, .5),
                     labels = c("-25%", "0", "25%", "50%"))+
  scale_x_discrete(expand=c(-0.8, -0.8), labels = c("Urban","# Floors", "# Units per floor",
                                                    "Floor area per capita"))+
  ylab("Change in odds ratio")

library(ggplot2)
# income_city -----
df <- read.csv("figure_code/income_city.csv")
df$Income <- factor(df$Income, levels=c('Low', 'Medium', 'High'))
#Var "# Floors", "# Units per floor", "Floor area per capita" is denoted by "a", "b", "c" respectively in income_city.csv. 
ggplot(df, aes(x = var, y= est))+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin=est_l, ymax=est_u, fill = Income, color=Income), size=0.6, width=0, position = position_dodge(width = 0.8))+
  scale_color_manual(values = c("#f5ac7a", "#f29683", "#db595e"))+
  
  geom_point(aes(color=Income), shape = 19, size = 1.8, position = position_dodge(width = 0.8))+
  scale_color_manual(values = c("#f5ac7a", "#f29683", "#db595e"))+
  
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
  scale_y_continuous(limit = c(-.25, 0.5), breaks = c(-.25, 0, .25, .5),
                     labels = c("-25%", "0", "25%", "50%"))+
  scale_x_discrete(expand=c(-0.8, -0.8), labels = c("# Floors", "# Units per floor",
                                                    "Floor area per capita"))+
  ylab("Change in odds ratio")

library(ggplot2)
# edu2 -----
df <- read.csv("figure_code/edu2.csv")
df$edu2 <- factor(df$edu2, levels=c('Low', 'Medium', 'High'))
#Var "Urban", "# Floors", "# Units per floor", "Floor area per capita" is denoted by "a", "b", "c", "d" respectively in edu2.csv. 
ggplot(df, aes(x = var, y= est))+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin=est_l, ymax=est_u, fill = edu2, color=edu2), size=0.6, width=0, position = position_dodge(width = 0.8))+
  scale_color_manual(values = c("#f5ac7a", "#f29683", "#db595e"))+
  
  geom_point(aes(color=edu2), shape = 19, size = 1.8, position = position_dodge(width = 0.8))+
  scale_color_manual(values = c("#f5ac7a", "#f29683", "#db595e"))+
  
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
  scale_y_continuous(limit = c(-.5, .5), breaks = c(-.5, -.25, 0, .25, .5),
                     labels = c("-50%", "-25%", "0", "25%", "50%"))+
  scale_x_discrete(expand=c(-0.8, -0.8), labels = c("Urban","# Floors", "# Units per floor",
                                                    "Floor area per capita"))+
  ylab("Change in odds ratio")

library(ggplot2)
# edu2_city -----
df <- read.csv("figure_code/edu2_city.csv")
df$edu2_city <- factor(df$edu2_city, levels=c('Low', 'Medium', 'High'))
#Var "# Floors", "# Units per floor", "Floor area per capita" is denoted by "a", "b", "c" respectively in edu2_city.csv. 
ggplot(df, aes(x = var, y= est))+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin=est_l, ymax=est_u, fill = edu2_city, color=edu2_city), size=0.6, width=0, position = position_dodge(width = 0.8))+
  scale_color_manual(values = c("#f5ac7a", "#f29683", "#db595e"))+
  
  geom_point(aes(color=edu2_city), shape = 19, size = 1.8, position = position_dodge(width = 0.8))+
  scale_color_manual(values = c("#f5ac7a", "#f29683", "#db595e"))+
  
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
  scale_y_continuous(limit = c(-.5, .5), breaks = c(-.5, -.25, 0, .25, .5),
                     labels = c("-50%", "-25%", "0", "25%", "50%"))+
  scale_x_discrete(expand=c(-0.8, -0.8), labels = c("# Floors", "# Units per floor",
                                                    "Floor area per capita"))+
  ylab("Change in odds ratio")

library(ggplot2)
# education -----
df <- read.csv("figure_code/education.csv")
df$education <- factor(df$education, levels=c('Low', 'Medium', 'High'))
#Var "Urban", "# Floors", "# Units per floor", "Floor area per capita" is denoted by "a", "b", "c", "d" respectively in education.csv. 
ggplot(df, aes(x = var, y= est))+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin=est_l, ymax=est_u, fill = education, color=education), size=0.6, width=0, position = position_dodge(width = 0.8))+
  scale_color_manual(values = c("#f5ac7a", "#f29683", "#db595e"))+
  
  geom_point(aes(color=education), shape = 19, size = 1.8, position = position_dodge(width = 0.8))+
  scale_color_manual(values = c("#f5ac7a", "#f29683", "#db595e"))+
  
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
  scale_y_continuous(limit = c(-1, 0.5), breaks = c(-1, -.75, -.5, -.25, 0, .25, .5),
                     labels = c("-100%", "-75%", "-50%", "-25%", "0", "25%", "50%"))+
  scale_x_discrete(expand=c(-0.8, -0.8), labels = c("Urban","# Floors", "# Units per floor",
                                                    "Floor area per capita"))+
  ylab("Change in odds ratio")

library(ggplot2)
# education_city -----
df <- read.csv("figure_code/education_city.csv")
df$education_city <- factor(df$education_city, levels=c('Low', 'Medium', 'High'))
#Var "# Floors", "# Units per floor", "Floor area per capita" is denoted by "a", "b", "c" respectively in education_city.csv. 
ggplot(df, aes(x = var, y= est))+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin=est_l, ymax=est_u, fill = education_city, color=education_city), size=0.6, width=0, position = position_dodge(width = 0.8))+
  scale_color_manual(values = c("#f5ac7a", "#f29683", "#db595e"))+
  
  geom_point(aes(color=education_city), shape = 19, size = 1.8, position = position_dodge(width = 0.8))+
  scale_color_manual(values = c("#f5ac7a", "#f29683", "#db595e"))+
  
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
  scale_y_continuous(limit = c(-1, 0.5), breaks = c(-1, -.75, -.5, -.25, 0, .25, .5),
                     labels = c("-100%", "-75%", "-50%", "-25%", "0", "25%", "50%"))+
  scale_x_discrete(expand=c(-0.8, -0.8), labels = c("# Floors", "# Units per floor",
                                                    "Floor area per capita"))+
  ylab("Change in odds ratio")