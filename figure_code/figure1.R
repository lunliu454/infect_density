library(ggplot2)
# Population_density -----
figure1_A <- read.csv("figure/figure1_A.csv")
ggplot(figure1_A,aes(x=Population_density, y=Symptom, fill = Symptom))+
  stat_boxplot(geom = "errorbar", width = 0.2)+
  geom_boxplot(size = 0.2, outlier.colour = "black", outlier.size = 0.3, alpha = 0.7)+
  geom_point(position = "jitter", size = 0.0001, color = "black", alpha = 0.2)+
  labs(x="Population density (sqkm)", y="Symptom", size = 5)+
  #Symptom "Uninfected", "Asymptomatic", "No fever", "Low fever", "High fever" is denoted by "e", "d", "c", "b", "a" respectively in figure1_A.csv. 
  scale_y_discrete(breaks=c("e", "d", "c", "b", "a"),
                   labels = c ("Uninfected", "Asymptomatic", "No fever", "Low fever", "High fever"),
                   limits = c ("e", "d", "c", "b", "a"))+
  scale_fill_manual(labels = c("High fever", "Low fever", "No fever", "Asymptomatic", "Uninfected"),
                    values = c("#CC1118", "#ED684E",  "#F08841", "#6DAFD7", "#024690"))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14, vjust = -0.5),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.6))+
  guides(fill=F)
ggsave("figure/f1_A.png", width = 16, height = 10, units = "cm", 
       dpi = 300, limitsize = F)

# Settlement type -----
library(ggplot2)
figure1_B <- read.csv("figure/figure1_B.csv")
ggplot(figure1_B,aes(x=City,y=Percentage,fill=Symptom))+
  geom_bar(stat = 'identity', position = 'dodge', width = 0.8, alpha = 0.7)+
  #Symptom "Uninfected", "Asymptomatic", "No fever", "Low fever", "High fever" is denoted by "a", "b", "c", "d", "e" respectively in figure1_B.csv. 
  scale_fill_manual(labels = c("Uninfected", "Asymptomatic", "No fever", "Low fever", "High fever"), 
                    values = c(a = "#024690", b = "#6DAFD7", c = "#F08841", d = "#ED684E", e = "#CC1118")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.text=element_text (size=10),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))+
  labs(x = "Settlement type")+
  scale_y_continuous(limit = c(0, 0.5), breaks = c(0, .1, .2, .3, .4, .5),
                     labels = c("0", "10%", "20%", "30%", "40%","50%"))+
  scale_x_discrete(limits = c("Urban", "Rural"))
ggsave("figure/f1_B.png", width = 12, height = 10, units = "cm", 
       dpi = 300, limitsize = F)

# Number of floors -----
library(ggplot2)
figure1_C <- read.csv("figure/figure1_C.csv")
ggplot(figure1_C,aes(x=Floor,y=Percentage,fill=Symptom))+
  geom_bar(stat = 'identity', position = 'dodge', width = 0.8, alpha = 0.7)+ 
  #Symptom "Uninfected", "Asymptomatic", "No fever", "Low fever", "High fever" is denoted by "a", "b", "c", "d", "e" respectively in figure1_C.csv.
  scale_fill_manual(labels = c("Uninfected", "No symptom", "No fever", "Low fever", "High fever"), 
                    values = c(a = "#024690", b = "#6DAFD7", c = "#F08841", d = "#ED684E", e = "#CC1118"))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14, vjust = 3),
        axis.title.x = element_text(size = 14, vjust = -0.5),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))+
  guides(fill=F)+
  labs(x = "Number of floors")+
  scale_y_continuous(limit = c(0, 0.6), breaks = c(0, .1, .2, .3, .4, .5, .6),labels = c("0", "10%", "20%", "30%", "40%","50%", "60%"))+
  scale_x_discrete(limits = c("High rise", "Higher mid-rise", "Lower mid-rise", "Low rise", "Very low rise", "Single family"), 
                   labels=c("High\nrise", "Higher\nmid-rise", "Lower\nmid-rise", "Low\nrise", "Very\nlow rise", "Single\nfamily")) 
ggsave("figure/f1_C.png", width = 15, height = 10, units = "cm", 
       dpi = 300, limitsize = F)

# Units per floor -----
library(ggplot2)
figure1_D <- read.csv("figure/figure1_D.csv")
ggplot(figure1_D,aes(x=Unit,y=Percentage,fill=Symptom))+
  geom_bar(stat = 'identity', position = 'dodge', width = 0.8, alpha = 0.7)+
  #Symptom "Uninfected", "Asymptomatic", "No fever", "Low fever", "High fever" is denoted by "a", "b", "c", "d", "e" respectively in figure1_D.csv.
  scale_fill_manual(labels = c("Uninfected", "Asymptomatic", "No fever", "Low fever", "High fever"), 
                    values = c(a = "#024690", b = "#6DAFD7", c = "#F08841", d = "#ED684E", e = "#CC1118")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))+
  guides(fill=F)+
  labs(x = "Units per floor")+
  scale_y_continuous(limit = c(0, 0.6), breaks = c(0, .1, .2, .3, .4, .5, .6),labels = c("0", "10%", "20%", "30%", "40%","50%", "60%"))+
  scale_x_discrete(limits = c("1-3", "4-6", "7-9", ">=10"), labels=c("1-3", "4-6", "7-9", ">=10"))
ggsave("figure/f1_D.png", width = 12, height = 10, units = "cm", 
       dpi = 300, limitsize = F)

# Floor area per capita -----
library(ggplot2)
figure1_E <- read.csv("figure/figure1_E.csv")
ggplot(figure1_E,aes(x=Areapc, y=Symptom, fill = Symptom))+
  stat_boxplot(geom = "errorbar", width = 0.2)+
  geom_boxplot(size = 0.2, outlier.colour = "black", outlier.size = 0.3, alpha = 0.7)+
  geom_point(position = "jitter", size = 0.0001, color = "black", alpha = 0.3)+
  labs(x="Floor area per capita (sqm)", y="Symptom", size = 5)+
  #Symptom "Uninfected", "Asymptomatic", "No fever", "Low fever", "High fever" is denoted by "e", "d", "c", "b", "a" respectively in figure1_E.csv. 
  scale_y_discrete(breaks=c("e", "d", "c", "b", "a"),
                   labels = c ("Uninfected", "Asymptomatic", "No fever", "Low fever", "High fever"),
                   limits = c ("e", "d", "c", "b", "a"))+
  scale_fill_manual(labels = c("High fever", "Low fever", "No fever", "Asymptomatic", "Uninfected"),
                    values = c("#CC1118", "#ED684E",  "#F08841", "#6DAFD7", "#024690"))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14, vjust = -0.5),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.6))+
  guides(fill=F)
ggsave("figure/f1_E.png", width = 16, height = 10, units = "cm", 
       dpi = 300, limitsize = F)

