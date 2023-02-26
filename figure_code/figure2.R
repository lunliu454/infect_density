library(ggplot2)
df <- read.csv("figure/figure2.csv")
df$Age <- factor(df$Age, levels=c('<18', '18-29', '30-39','40-49', '>=50'))
#Var "Urban vs Rural", "Very low rise", "Low rise", "Lower mid-rise", "Higher mid-rise", "High rise", "Units", "Floor area per capita" is denoted by "a", "b", "c", "d", "e", "f", "g", "h" respectively in figure2.csv. 
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
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 10, hjust = -0.1, vjust = -0.5),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))+
  scale_y_continuous(limit = c(-.5, 3), breaks = c(-.5, 0, .5, 1, 1.5, 2, 2.5, 3),
                     labels = c("-50%", "0", "50%", "100%", "150%", "200%","250%", "300%"))+
  scale_x_discrete(expand=c(-0.8, -0.8), labels = c("Urban vs Rural",
                                                    "Very\nlow rise", "Low\nrise",  "Lower\nmid-rise", "Higher\nmid-rise","High\nrise", "Units",
                                                    "Floor area\nper capita"))+
  ylab("Change in odds ratio")+
  xlab("District scale                                             Neighborhood scale                                                  Household scale")