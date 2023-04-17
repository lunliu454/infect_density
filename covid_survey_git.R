library(dplyr)
library(stringr)
library(MASS)
library(ordinal)
library(VGAM)

t <- read.csv('data_clean.csv')
t <- t %>% 
  mutate(age = as.numeric(Q2), city = 2 - as.numeric(Q3), floor = as.numeric(Q5), 
         floor = recode(floor, '1'=6, '2'=5, '3'=4, '4'=3, '5'=2, '6'=1),
         unit = as.numeric(Q6), symp = as.numeric(Q12), symp = recode(symp, '5'=4),
         income = as.numeric(Q13), edu = as.numeric(Q14), edu2 = as.numeric(Q15),
         district = as.numeric(density),
         edu2 = ifelse(edu2==-3|is.na(edu2), edu, edu2))
bout <- function(x){
  bx <- format(round(b[x], 3))
  sex <- format(round(se[x], 3), scientific = F)
  if(p[x] > 0.05){
    return(paste0(bx, '\n(', sex, ')'))
  }
  if(p[x] <= 0.05 & p[x] > 0.01){
    return(paste0(bx, '*\n(', sex, ')'))
  }
  if(p[x] <= 0.01 & p[x] > 0.001){
    return(paste0(bx, '**\n(', sex, ')'))
  }
  if(p[x] <= 0.001){
    return(paste0(bx, '***\n(', sex, ')'))
  }
}
#### multivariate model - all ####
# district
fit1 <- vglm(symp ~ city + as.factor(floor) + unit + I(areapc/10) + income + edu2 + edu + age, data=t, 
             cumulative(parallel = T, reverse = T))

# neighborhood
fit2 <- vglm(symp ~ as.factor(floor) + unit + I(areapc/10) + income + edu2 + edu + age, data=t,
             cumulative(parallel = T, reverse = T))

# household
fit3 <- vglm(symp ~ I(areapc/10) + income + edu2 + edu + age, data=t,
             cumulative(parallel = T, reverse = T))

#### model result output ####
olr_table <- data.frame(var=c('Urban', '# Floors (Ref: Single-family house)', 
                '  Very low rise', '  Low rise', '  Lower mid-rise', '  Higher mid-rise', '  High rise',
                '# Units per floor', 'Floor area per capita', 'Income', 'Education of household', 
                'Education', 'Age', 'Threshold', '  Asymptomatic', '  No fever', '  Low fever', '  High fever'), 
                 b1=0, z1=0, p1=0,b2=0, z2=0, p2=0,b3=0, z3=0,p3=0)
b <- summary(fit1)@coef3[c(5:16, 1:4), 1]
b <- format(round(b, 3), scientific = F)
se <- summary(fit1)@coef3[c(5:16, 1:4), 2]
se <- format(round(se, 3), scientific = F)
z  <- summary(fit1)@coef3[c(5:16, 1:4), 3]
p  <- summary(fit1)@coef3[c(5:16, 1:4), 4]
olr_table$b1[c(1, 3:13, 15:18)] <- paste0(b,"\n(",se,")")
olr_table$z1[c(1, 3:13, 15:18)] <- format(round(z, 3), scientific = F)
olr_table$p1[c(1, 3:13, 15:18)] <- format(round(p, 3), scientific = F)
b <- summary(fit2)@coef3[c(5:15, 1:4), 1]
b <- format(round(b, 3), scientific = F)
se <- summary(fit2)@coef3[c(5:15, 1:4), 2]
se <- format(round(se, 3), scientific = F)
z  <- summary(fit2)@coef3[c(5:15, 1:4), 3]
p  <- summary(fit2)@coef3[c(5:15, 1:4), 4]
olr_table$b2[c(3:13, 15:18)] <- paste0(b,"\n(",se,")")
olr_table$z2[c(3:13, 15:18)] <- format(round(z, 3), scientific = F)
olr_table$p2[c(3:13, 15:18)] <- format(round(p, 3), scientific = F)
b <- summary(fit3)@coef3[c(5:9, 1:4), 1]
b <- format(round(b, 3), scientific = F)
se <- summary(fit3)@coef3[c(5:9, 1:4), 2]
se <- format(round(se, 3), scientific = F)
z  <- summary(fit3)@coef3[c(5:9, 1:4), 3]
p  <- summary(fit3)@coef3[c(5:9, 1:4), 4]
olr_table$b3[c(9:13, 15:18)] <- paste0(b,"\n(",se,")")
olr_table$z3[c(9:13, 15:18)] <- format(round(z, 3), scientific = F)
olr_table$p3[c(9:13, 15:18)] <- format(round(p, 3), scientific = F)
write.csv(olr_table, 'olr_table.csv')

#### multivariate model - city only ####
# neighborhood
fit2 <- vglm(symp ~ as.factor(floor) + unit + I(areapc/10) + income + edu2 + edu + age, data=filter(t,city==1),
             cumulative(parallel = T, reverse = T))
summary(fit2)
# household
fit3 <- vglm(symp ~ I(areapc/10) + income + edu2 + edu + age, data=filter(t,city==1),
             cumulative(parallel = T, reverse = T))
summary(fit3)

#### model result output ####
olr_table <- data.frame(var=c('# Floors (Ref: Single-family house)', 
                              '  Very low rise', '  Low rise', '  Lower mid-rise', '  Higher mid-rise', '  High rise',
                              '# Units per floor', 'Floor area per capita', 'Income', 'Education of household', 
                              'Education', 'Age', 'Threshold', '  Asymptomatic', '  No fever', '  Low fever', '  High fever'), 
                        b2=0, z2=0, p2=0,b3=0, z3=0,p3=0)
b <- summary(fit2)@coef3[c(5:15, 1:4), 1]
b <- format(round(b, 3), scientific = F)
se <- summary(fit2)@coef3[c(5:15, 1:4), 2]
se <- format(round(se, 3), scientific = F)
z  <- summary(fit2)@coef3[c(5:15, 1:4), 3]
p  <- summary(fit2)@coef3[c(5:15, 1:4), 4]
olr_table$b2[c(2:12, 14:17)] <- paste0(b,"\n(",se,")")
olr_table$z2[c(2:12, 14:17)] <- format(round(z, 3), scientific = F)
olr_table$p2[c(2:12, 14:17)] <- format(round(p, 3), scientific = F)
b <- summary(fit3)@coef3[c(5:9, 1:4), 1]
b <- format(round(b, 3), scientific = F)
se <- summary(fit3)@coef3[c(5:9, 1:4), 2]
se <- format(round(se, 3), scientific = F)
z  <- summary(fit3)@coef3[c(5:9, 1:4), 3]
p  <- summary(fit3)@coef3[c(5:9, 1:4), 4]
olr_table$b3[c(8:12, 14:17)] <- paste0(b,"\n(",se,")")
olr_table$z3[c(8:12, 14:17)] <- format(round(z, 3), scientific = F)
olr_table$p3[c(8:12, 14:17)] <- format(round(p, 3), scientific = F)
write.csv(olr_table, 'olr_table_city.csv')

#### statistical test for proportional ordinal ####
library(brant)
m <- polr(as.factor(symp) ~ areapc + floor + unit + city + income + 
            edu + edu2 + age, t, Hess = TRUE, method = 'logistic')
brant(m)
b_output <- as.data.frame(brant(m))
b_output <- format(round(b_output,2))
b_output$var <- c('All', 'Floor area per capita', '# Floors', '# Units per floor', 'Urban', 
                  'Income', 'Education', 'Education of household', 'Age')
b_output <- b_output[ , c(4, 1, 2, 3)]
write.csv(b_output, 'brant.csv')

library(brant)
m <- polr(as.factor(symp) ~ areapc + floor + unit + city + income + 
            edu + edu2 + age, t, Hess = TRUE, method = 'logistic')
brant(m)
b_output <- as.data.frame(brant(m))
b_output <- format(round(b_output,2))
b_output$var <- c('All', 'Floor area per capita', '# Floors', '# Units per floor', 'Urban', 
                  'Income', 'Education', 'Education of household', 'Age')
b_output <- b_output[ , c(4, 1, 2, 3)]
write.csv(b_output, 'brant.csv')


#### heterogeneity analysis by age ####
s <- t %>% mutate(age = dplyr::recode(age, '1'='u18', '2'='1829', '3'='3039',
                    '4'='4049', '5'='50+', '6'='50+', '7'='50+'))
agegroup <- c('u18', '1829', '3039', '4049', '50+')
ageout <- data.frame(var=c('Floor area per capita', '# Floors', '# Units per floor', 'Urban'), b_u18=0, b_1829=0, b_3039=0, b_4049=0, b_50=0)

#all
for (i in 1:5){
    fit1 <- vglm(symp ~ I(areapc/10) + income + edu2 + edu,
                 data=filter(s, age==agegroup[i]), cumulative(parallel = TRUE, reverse = T))
    fit2 <- vglm(symp ~ I(areapc/10) + floor + unit + income + edu2 + edu,
                 data=filter(s, age==agegroup[i]), cumulative(parallel = TRUE, reverse = T))
    fit3 <- vglm(symp ~ I(areapc/10) + floor + unit + city + income + edu2 + edu,
                 data=filter(s, age==agegroup[i]), cumulative(parallel = TRUE, reverse = T))
  b <- summary(fit1)@coef3[5, 1]
  se <- summary(fit1)@coef3[5, 2]
  p <- summary(fit1)@coef3[5, 4]
  ageout[1, i+1] <- sapply(1,bout)
  b <- summary(fit2)@coef3[6:7, 1]
  se <- summary(fit2)@coef3[6:7, 2]
  p <- summary(fit2)@coef3[6:7, 4]
  ageout[2:3, i+1] <- sapply(1:2,bout)
  b <- summary(fit3)@coef3[8, 1]
  se <- summary(fit3)@coef3[8, 2]
  p <- summary(fit3)@coef3[8, 4]
  ageout[4, i+1] <- sapply(1,bout)
}
write.csv(ageout, "age_output.csv")
#city
ageout <- data.frame(var=c('Floor area per capita', '# Floors', '# Units per floor'), b_u18=0, b_1829=0, b_3039=0, b_4049=0, b_50=0)
for (i in 1:5){
    fit1 <- vglm(symp ~ I(areapc/10) + income + edu2 + edu,
                 data=filter(s, age==agegroup[i], city==1), cumulative(parallel = TRUE, reverse = T))
    fit2 <- vglm(symp ~ I(areapc/10) + floor + unit + income + edu2 + edu,
                 data=filter(s, age==agegroup[i], city==1), cumulative(parallel = TRUE, reverse = T))
  b <- summary(fit1)@coef3[5, 1]
  se <- summary(fit1)@coef3[5, 2]
  p <- summary(fit1)@coef3[5, 4]
  ageout[1, i+1] <- sapply(1,bout)
  b <- summary(fit2)@coef3[6:7, 1]
  se <- summary(fit2)@coef3[6:7, 2]
  p <- summary(fit2)@coef3[6:7, 4]
  ageout[2:3, i+1] <- sapply(1:2,bout)
}
write.csv(ageout, "age_output_city.csv")

#### heterogeneity analysis by income ####
s <- t %>% mutate(incomeg = case_when(income < 5 ~ 1, income == 5 ~ 2, income > 5 ~ 3))
statusout <- data.frame(var=c('Floor area per capita', '# Floors', '# Units per floor', 'Urban'), b_low=0, b_medium=0, b_high=0)
for (i in 1:3){
  fit1 <- vglm(symp ~ I(areapc/10) + age + edu + edu2, data=filter(s, incomeg==i), 
               cumulative(parallel = T, reverse = T))
  fit2 <- vglm(symp ~ I(areapc/10) + floor + unit + age + edu + edu2, data=filter(s, incomeg==i), 
               cumulative(parallel = T, reverse = T))
  fit3 <- vglm(symp ~ I(areapc/10) + floor + unit + city + age + edu + edu2, data=filter(s, incomeg==i), 
               cumulative(parallel = T, reverse = T))
  b <- summary(fit1)@coef3[5, 1]
  se <- summary(fit1)@coef3[5, 2]
  p <- summary(fit1)@coef3[5, 4]
  statusout[1, i+1] <- sapply(1,bout)
  b <- summary(fit2)@coef3[6:7, 1]
  se <- summary(fit2)@coef3[6:7, 2]
  p <- summary(fit2)@coef3[6:7, 4]
  statusout[2:3, i+1] <- sapply(1:2,bout)
  b <- summary(fit3)@coef3[8, 1]
  se <- summary(fit3)@coef3[8, 2]
  p <- summary(fit3)@coef3[8, 4]
  statusout[4, i+1] <- sapply(1,bout)
}
write.csv(statusout, "income_output.csv")
# city
statusout <- data.frame(var=c('Floor area per capita', '# Floors', '# Units per floor'), b_low=0, b_medium=0, b_high=0)
for (i in 1:3){
  fit1 <- vglm(symp ~ I(areapc/10) + edu + edu2 + age, data=filter(s, incomeg==i, city==1), 
               cumulative(parallel = T, reverse = T))
  fit2 <- vglm(symp ~ I(areapc/10) + floor + unit + edu + edu2 + age, data=filter(s, incomeg==i, city==1), 
               cumulative(parallel = T, reverse = T))
  b <- summary(fit1)@coef3[5, 1]
  se <- summary(fit1)@coef3[5, 2]
  p <- summary(fit1)@coef3[5, 4]
  statusout[1, i+1] <- sapply(1,bout)
  b <- summary(fit2)@coef3[6:7, 1]
  se <- summary(fit2)@coef3[6:7, 2]
  p <- summary(fit2)@coef3[6:7, 4]
  statusout[2:3, i+1] <- sapply(1:2,bout)
}
write.csv(statusout, "income_output_city.csv")
#alternative classification
s <- t %>% mutate(incomeg = case_when(income < 4 ~ 1, income == 4 ~ 2,
                                income == 5 ~ 2, income > 5 ~ 3))
#### heterogeneity analysis by education ####
s <- t %>% mutate(edug = case_when(edu < 5 ~ 1, edu == 5 ~ 2, edu > 5 ~ 3))
#alternative classification
s <- t %>% mutate(edug = case_when(edu < 4 ~ 1, edu == 4| edu == 5 ~ 2, edu > 5 ~ 3))
statusout <- data.frame(var=c('Floor area per capita', '# Floors', '# Units per floor', 'Urban'), b_low=0, b_medium=0, b_high=0)
for (i in 1:3){
  fit1 <- vglm(symp ~ I(areapc/10) + age + income + edu2, data=filter(s, edug==i), 
               cumulative(parallel = T, reverse = T))
  fit2 <- vglm(symp ~ I(areapc/10) + floor + unit + age + income + edu2, data=filter(s, edug==i), 
               cumulative(parallel = T, reverse = T))
  fit3 <- vglm(symp ~ I(areapc/10) + floor + unit + city + age + income + edu2, data=filter(s, edug==i), 
               cumulative(parallel = T, reverse = T))
  b <- summary(fit1)@coef3[5, 1]
  se <- summary(fit1)@coef3[5, 2]
  p <- summary(fit1)@coef3[5, 4]
  statusout[1, i+1] <- sapply(1,bout)
  b <- summary(fit2)@coef3[6:7, 1]
  se <- summary(fit2)@coef3[6:7, 2]
  p <- summary(fit2)@coef3[6:7, 4]
  statusout[2:3, i+1] <- sapply(1:2,bout)
  b <- summary(fit3)@coef3[8, 1]
  se <- summary(fit3)@coef3[8, 2]
  p <- summary(fit3)@coef3[8, 4]
  statusout[4, i+1] <- sapply(1,bout)
}
write.csv(statusout, "edu_output.csv")
# city
statusout <- data.frame(var=c('Floor area per capita', '# Floors', '# Units per floor'), b_low=0, b_medium=0, b_high=0)
for (i in 1:3){
  fit1 <- vglm(symp ~ I(areapc/10) + edu2 + income + age, data=filter(s, edug==i, city==1),
               cumulative(parallel = T, reverse = T))
  fit2 <- vglm(symp ~ I(areapc/10) + floor + unit + edu2 + income + age,
               data=filter(s, edug==i, city==1), cumulative(parallel = T, reverse = T))
  b <- summary(fit1)@coef3[5, 1]
  se <- summary(fit1)@coef3[5, 2]
  p <- summary(fit1)@coef3[5, 4]
  statusout[1, i+1] <- sapply(1,bout)
  b <- summary(fit2)@coef3[6:7, 1]
  se <- summary(fit2)@coef3[6:7, 2]
  p <- summary(fit2)@coef3[6:7, 4]
  statusout[2:3, i+1] <- sapply(1:2,bout)
}
write.csv(statusout, "edu_output_city.csv")

#### heterogeneity analysis by hosuehold education ####
s <- t %>% mutate(edu2g = case_when(edu2 < 5 ~ 1, edu2 == 5 ~ 2, edu2 > 5 ~ 3))
#alternative classification
s <- t %>% mutate(edu2g = case_when(edu2 < 4 ~ 1, edu2 == 4| edu2 == 5 ~ 2, edu2 > 5 ~ 3))
statusout <- data.frame(var=c('Floor area per capita', '# Floors', '# Units per floor', 'Urban'), b_low=0, b_medium=0, b_high=0)
for (i in 1:3){
  fit1 <- vglm(symp ~ I(areapc/10) + age + income + edu, data=filter(s, edu2g==i), 
               cumulative(parallel = T, reverse = T))
  fit2 <- vglm(symp ~ I(areapc/10) + floor + unit + age + income + edu, data=filter(s, edu2g==i), 
               cumulative(parallel = T, reverse = T))
  fit3 <- vglm(symp ~ I(areapc/10) + floor + unit + city + age + income + edu, data=filter(s, edu2g==i), 
               cumulative(parallel = T, reverse = T))
  b <- summary(fit1)@coef3[5, 1]
  se <- summary(fit1)@coef3[5, 2]
  p <- summary(fit1)@coef3[5, 4]
  statusout[1, i+1] <- sapply(1,bout)
  b <- summary(fit2)@coef3[6:7, 1]
  se <- summary(fit2)@coef3[6:7, 2]
  p <- summary(fit2)@coef3[6:7, 4]
  statusout[2:3, i+1] <- sapply(1:2,bout)
  b <- summary(fit3)@coef3[8, 1]
  se <- summary(fit3)@coef3[8, 2]
  p <- summary(fit3)@coef3[8, 4]
  statusout[4, i+1] <- sapply(1,bout)
}
write.csv(statusout, "edu2_output.csv")
# city
statusout <- data.frame(var=c('Floor area per capita', '# Floors', '# Units per floor'), b_low=0, b_medium=0, b_high=0)
for (i in 1:3){
  fit1 <- vglm(symp ~ I(areapc/10) + edu + income + age, data=filter(s, edu2g==i, city==1),
               cumulative(parallel = T, reverse = T))
  fit2 <- vglm(symp ~ I(areapc/10) + floor + unit + edu + income + age,
               data=filter(s, edu2g==i, city==1), cumulative(parallel = T, reverse = T))
  b <- summary(fit1)@coef3[5, 1]
  se <- summary(fit1)@coef3[5, 2]
  p <- summary(fit1)@coef3[5, 4]
  statusout[1, i+1] <- sapply(1,bout)
  b <- summary(fit2)@coef3[6:7, 1]
  se <- summary(fit2)@coef3[6:7, 2]
  p <- summary(fit2)@coef3[6:7, 4]
  statusout[2:3, i+1] <- sapply(1:2,bout)
}
write.csv(statusout, "edu2_output_city.csv")

#### robustness check - all####
rout <- data.frame(var=c('Floor area per capita', '# Floors (Ref: Single-family house)', 
                         '  Very low rise', '  Low rise', '  Lower mid-rise', '  Higher mid-rise', '  High rise',
                         '# Units per floor', 'Urban'), b_def=0, b_c01=0, b_c12=0, b_c23=0)
sr_code <- c("'0'=0", "'0'=1", "'1'=2", "'2'=3")
lev <- c(1, 0, 0, 0)
for (i in 1:4){
  q <- lev[i]
  tr <- eval(parse(text=paste0('mutate(t, symp = recode(symp, ', sr_code[i], '))')))
  fit1 <- vglm(symp ~ I(areapc/10) + income + edu + edu2 + age, data=tr, 
               cumulative(parallel = T, reverse = T))
  fit2 <- vglm(symp ~ I(areapc/10) + as.factor(floor) + unit + income + edu + edu2 + age, data=tr, 
               cumulative(parallel = T, reverse = T))
  fit3 <- vglm(symp ~ I(areapc/10) + as.factor(floor) + unit + city + income + edu + edu2 + age, data=tr, 
               cumulative(parallel = T, reverse = T))
  b <- summary(fit1)@coef3[4 + q, 1]
  se <- summary(fit1)@coef3[4 + q, 2]
  p <- summary(fit1)@coef3[4 + q, 4]
  rout[1, i+1] <- sapply(1,bout)
  b <- summary(fit2)@coef3[5:10 + q, 1]
  se <- summary(fit2)@coef3[5:10 + q, 2]
  p <- summary(fit2)@coef3[5:10 + q, 4]
  rout[3:8, i+1] <- sapply(1:6,bout)
  b <- summary(fit3)@coef3[11 + q, 1]
  se <- summary(fit3)@coef3[11 + q, 2]
  p <- summary(fit3)@coef3[11 + q, 4]
  rout[9, i+1] <- sapply(1,bout)
}
write.csv(rout, "robust_output.csv")

#### robustness check - city####
rout <- data.frame(var=c('Floor area per capita', '# Floors (Ref: Single-family house)', 
                         '  Very low rise', '  Low rise', '  Lower mid-rise', '  Higher mid-rise', '  High rise',
                         '# Units per floor'), b_def=0, b_c01=0, b_c12=0, b_c23=0)
sr_code <- c("'0'=0", "'0'=1", "'1'=2", "'2'=3")
lev <- c(1, 0, 0, 0)
for (i in 1:4){
  q <- lev[i]
  tr <- eval(parse(text=paste0('mutate(t, symp = recode(symp, ', sr_code[i], '))')))
  fit1 <- vglm(symp ~ I(areapc/10) + income + edu + edu2 + age, data=filter(tr,city==1), 
               cumulative(parallel = T, reverse = T))
  fit2 <- vglm(symp ~ I(areapc/10) + as.factor(floor) + unit + income + edu + edu2 + age, 
               data=filter(tr,city==1), 
               cumulative(parallel = T, reverse = T))
  b <- summary(fit1)@coef3[4 + q, 1]
  se <- summary(fit1)@coef3[4 + q, 2]
  p <- summary(fit1)@coef3[4 + q, 4]
  rout[1, i+1] <- bout(1)
  b <- summary(fit2)@coef3[5:10 + q, 1]
  se <- summary(fit2)@coef3[5:10 + q, 2]
  p <- summary(fit2)@coef3[5:10 + q, 4]
  rout[3:8, i+1] <- sapply(1:6, bout)
}
write.csv(rout, "robust_output_city.csv")



#### graph test for proportional ordinal ####
library(Hmisc)
sf <- function(y) {
  c('Y>=0' = qlogis(mean(y >= 0)),
    'Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)))
}
t_po <- t %>% mutate(floor = as.factor(floor), 
                     unit = recode(unit, '1'=1, '2'=1, '3'=2, '4'=2, '5'=3, '6'=3, '7'=4, '8'=4, '9'=5, '10'=5),
                     areapc = case_when(areapc<=10 ~ 1, areapc>10 & areapc<=20 ~ 2,
                                        areapc>20 & areapc<=30 ~ 3, areapc>30 ~ 4),
                     income = recode(income, '1'=1, '2'=1, '3'=2, '4'=2, '5'=3, '6'=3, '7'=4, '8'=4),
                     edu2 = ifelse(edu2==-3, edu, edu2),
                     edu = recode(edu, '1'=1, '2'=1, '3'=2, '4'=3, '5'=4, '6'=5, '7'=5),
                     edu2 = recode(edu2, '1'=1, '2'=1, '3'=2, '4'=3, '5'=4, '6'=5, '7'=5),
                     age = recode(age, '1'=1, '2'=2, '3'=3, '4'=4, '5'=5, '6'=5, '7'=5),
                     unit = as.factor(unit),
                     areapc = as.factor(areapc),
                     income = as.factor(income),
                     edu = as.factor(edu),
                     edu2 = as.factor(edu2),
                     age = as.factor(age))
sg <- with(t_po, summary(symp ~ city + floor + unit + areapc + income + 
                        edu + edu2 + age, fun=sf))
pot <- data.frame(var=0, level=0, symp=0, logit=0, logit_u=0, logit_l=0)
vars <- c('city', 'floor', 'unit', 'areapc', 'income', 'edu', 'edu2', 'age')
for (i in 1:4){
  for (j in 1:8){
    b <- eval(parse(text = paste("glm(I(symp >= i) ~ ", vars[j], ", family='binomial', data = t_po)")))
    #b <- glm(I(symp >= i) ~ city, family="binomial", data = s)
    c <- summary(b)$coefficients[,1]
    n <- length(c)
    logi <- c + c[1]
    logi[1] <- logi[1] - c[1]
    se <- sqrt(diag(vcov(b)) + vcov(b)[1,1] + 2 * vcov(b)[1,])
    se[1] <- summary(b)$coefficients[1,2]
    seg_pot <- data.frame(var=rep(vars[j], n), level=1:n, symp=rep(i, n),
                          logit=logi, 
                          logit_u=logi+1.96*se,
                          logit_l=logi-1.96*se)
    pot <- rbind(pot, seg_pot)
  }
}
pot <- pot[-1, ]
pot <- pot %>% 
  mutate(level = case_when(var=='city'&level==1~'1rural', var=='city'&level==2~'2urban',
         var=='floor'&level==1~'1singlefamilyhouse',var=='floor'&level==2~'2lowrise',
         var=='floor'&level==3~'3lowermidrise',var=='floor'&level==4~'4midrise',
         var=='floor'&level==5~'5highermidrise',var=='floor'&level==6~'6highrise',
         var=='unit'&level==1~'1-2',var=='unit'&level==2~'3-4',
         var=='unit'&level==3~'5-6',var=='unit'&level==4~'7-8',var=='unit'&level==5~'9-10',
         var=='areapc'&level==1~'1<=10sqm',var=='areapc'&level==2~'210-20sqm',
         var=='areapc'&level==3~'320-30sqm',var=='areapc'&level==4~'4>30sqm',
         var=='income'&level==1~'1<=3wrmb',var=='income'&level==2~'23-15wrmb',
         var=='income'&level==3~'315-50wrmb',var=='income'&level==4~'4>50wrmb',
         var=='edu'&level==1~'1juniorschool&lower',var=='edu'&level==2~'2highschool',
         var=='edu'&level==3~'3juniorcollege',var=='edu'&level==4~'4bachelor',
         var=='edu'&level==5~'5master&higher',
         var=='edu2'&level==1~'1juniorschool&lower',var=='edu2'&level==2~'2highschool',
         var=='edu2'&level==3~'3juniorcollege',var=='edu2'&level==4~'4bachelor',
         var=='edu2'&level==5~'5master&higher',
         var=='age'&level==1~'1<18', var=='age'&level==2~'218-29',
         var=='age'&level==3~'330-39', var=='age'&level==4~'440-49',
         var=='age'&level==5~'5>=50'))
write.csv(pot, "forsupfig2.csv")


