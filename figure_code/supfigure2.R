library(ggplot2)
# Rural vs urban -----
df <- read.csv("figure_code/figure3_logit1.csv")
ggplot(df, aes(x = level, y= logit, group=Symptom, color=Symptom))+
  geom_line(size = 0.8)+
  geom_errorbar(aes(ymin=logit_l, ymax=logit_u), color = 'black', alpha = 1, size=0.6, width=0)+
  geom_point(shape = 19, size = 2)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, vjust = -1),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))+
  guides(fill=F)+
  ylab("Logit")+
  xlab("Rural vs urban")+
  scale_color_manual(labels = c("Asymptomatic", "No fever", "Low fever", "High fever"),
                     values = c("#6DAFD7", "#F08841", "#ED684E", "#CC1118"))
ggsave("figure/f3_logit1.png", width = 16, height = 10, units = "cm", 
       dpi = 300, limitsize = F)

# Number of floors -----
df <- read.csv("figure_code/figure3_logit2.csv")
ggplot(df, aes(x = level, y= logit, group=Symptom, color=Symptom))+
  geom_line(size = 0.8)+
  geom_errorbar(aes(ymin=logit_l, ymax=logit_u), color = 'black', alpha = 1, size=0.6, width=0)+
  geom_point(shape = 19, size = 2)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, vjust = -0.5),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))+
  ylab("Logit")+
  xlab("Number of floors")+
  scale_color_manual(labels = c("Asymptomatic", "No fever", "Low fever", "High fever"),
                     values = c("#6DAFD7", "#F08841", "#ED684E", "#CC1118"))+
  scale_x_discrete(limits = c("Single family", "Low-rise", "Lower mid-rise", "Mid-rise", "Higher mid-rise", "High rise"), 
                   labels=c("Single\nfamily", "Low-rise", "Lower\nmid-rise", "Mid-rise", "Higher\nmid-rise", "High\nrise")) 
ggsave("figure/f3_logit2.png", width = 16, height = 10, units = "cm", 
       dpi = 300, limitsize = F)

# Units per floor -----
df <- read.csv("figure_code/figure3_logit3.csv")
ggplot(df, aes(x = level, y= logit, group=Symptom, color=Symptom))+
  geom_line(size = 0.8)+
  geom_errorbar(aes(ymin=logit_l, ymax=logit_u), color = 'black', alpha = 1, size=0.6, width=0)+
  geom_point(shape = 19, size = 2)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "white"),
        #plot.title = element_text(size = 24,face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 14),#hjust = 0.5, 
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, vjust = -0.5),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))+
  ylab("Logit")+
  xlab("Units per floor")+
  scale_color_manual(labels = c("Asymptomatic", "No fever", "Low fever", "High fever"),
                     values = c("#6DAFD7", "#F08841", "#ED684E", "#CC1118"))+
  scale_x_discrete(limits = c("1-2", "3-4", "5-6", "7-8", "9-10"), 
                   labels=c("1-2", "3-4", "5-6", "7-8", "9-10")) 
ggsave("figure/f3_logit3.png", width = 16, height = 10, units = "cm", 
       dpi = 300, limitsize = F)

# Floor area per capita -----
df <- read.csv("figure_code/figure3_logit4.csv")
ggplot(df, aes(x = level, y= logit, group=Symptom, color=Symptom))+
  geom_line(size = 0.8)+
  geom_errorbar(aes(ymin=logit_l, ymax=logit_u), color = 'black', alpha = 1, size=0.6, width=0)+
  geom_point(shape = 19, size = 2)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, vjust = -0.5),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))+
  ylab("Logit")+
  xlab("Floor area per capita (square meter)")+
  scale_color_manual(labels = c("Asymptomatic", "No fever", "Low fever", "High fever"),
                     values = c("#6DAFD7", "#F08841", "#ED684E", "#CC1118"))+
  scale_x_discrete(limits = c("<=10sqm", "10-20sqm", "30-39", "20-30sqm", ">30sqm"), 
                   labels=c("<=10", "10-20", "30-39", "20-30", ">30")) 
ggsave("figure/f3_logit4.png", width = 16, height = 10, units = "cm", 
       dpi = 300, limitsize = F)

# Household income -----
df <- read.csv("figure_code/figure3_logit5.csv")
ggplot(df, aes(x = level, y= logit, group=Symptom, color=Symptom))+
  geom_line(size = 0.8)+
  geom_errorbar(aes(ymin=logit_l, ymax=logit_u), color = 'black', alpha = 1, size=0.6, width=0)+
  geom_point(shape = 19, size = 2)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, vjust = -0.5),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))+
  ylab("Logit")+
  xlab("Household income (10,000 rmb)")+
  scale_color_manual(labels = c("Asymptomatic", "No fever", "Low fever", "High fever"),
                     values = c("#6DAFD7", "#F08841", "#ED684E", "#CC1118"))+
  scale_x_discrete(limits = c("<=3wrmb", "3-15wrmb", "15-50wrmb", ">50wrmb"), 
                   labels=c("<=3", "3-15", "15-50", ">50")) 
ggsave("figure/f3_logit5.png", width = 16, height = 10, units = "cm", 
       dpi = 300, limitsize = F)

# Education -----
df <- read.csv("figure_code/figure3_logit6.csv")
ggplot(df, aes(x = level, y= logit, group=Symptom, color=Symptom))+
  geom_line(size = 0.8)+
  geom_errorbar(aes(ymin=logit_l, ymax=logit_u), color = 'black', alpha = 1, size=0.6, width=0)+
  geom_point(shape = 19, size = 2)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, vjust = -0.5),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))+
  ylab("Logit")+
  xlab("Education")+
  scale_color_manual(labels = c("Asymptomatic", "No fever", "Low fever", "High fever"),
                     values = c("#6DAFD7", "#F08841", "#ED684E", "#CC1118"))+
  scale_x_discrete(limits = c("Juniorschool & lower", "High school", "Junior college", "Bachelor", "Master & higher"), 
                   labels=c("Junior school\n&lower", "High\nschool", "Junior\ncollege", "Bachelor", "Master\n&higher")) \
ggsave("figure/f3_logit6.png", width = 16, height = 10, units = "cm", 
       dpi = 300, limitsize = F)

# Highest education in the household -----
df <- read.csv("figure_code/figure3_logit7.csv")
ggplot(df, aes(x = level, y= logit, group=Symptom, color=Symptom))+
  geom_line(size = 0.8)+
  geom_errorbar(aes(ymin=logit_l, ymax=logit_u), color = 'black', alpha = 1, size=0.6, width=0)+
  geom_point(shape = 19, size = 2)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, vjust = -0.5),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))+
  ylab("Logit")+
  xlab("Highest education in the household")+
  scale_color_manual(labels = c("Asymptomatic", "No fever", "Low fever", "High fever"),
                     values = c("#6DAFD7", "#F08841", "#ED684E", "#CC1118"))+
  scale_x_discrete(limits = c("Juniorschool & lower", "High school", "Junior college", "Bachelor", "Master & higher"), 
                   labels=c("Junior school\n&lower", "High\nschool", "Junior\ncollege", "Bachelor", "Master\n&higher")) 
ggsave("figure/f3_logit7.png", width = 16, height = 10, units = "cm", 
       dpi = 300, limitsize = F)

# Age -----
df <- read.csv("figure_code/figure3_logit8.csv")
ggplot(df, aes(x = level, y= logit, group=Symptom, color=Symptom))+
  geom_line(size = 0.8)+
  geom_errorbar(aes(ymin=logit_l, ymax=logit_u), color = 'black', alpha = 1, size=0.6, width=0)+
  geom_point(shape = 19, size = 2)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, vjust = -0.5),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))+
  ylab("Logit")+
  xlab("Age")+
  scale_color_manual(labels = c("Asymptomatic", "No fever", "Low fever", "High fever"),
                     values = c("#6DAFD7", "#F08841", "#ED684E", "#CC1118"))+
  scale_x_discrete(limits = c("<18", "18-29", "30-39", "40-49", ">=50"), 
                   labels=c("<18", "18-29", "30-39", "40-49", ">=50"))
ggsave("figure/f3_logit8.png", width = 16, height = 10, units = "cm", 
       dpi = 300, limitsize = F)