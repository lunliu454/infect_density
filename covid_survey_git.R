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
         district = as.numeric(density))

# univariate correlation               
cor.test(t$district, t$symp, method="spearman")
cor.test(t$city, t$symp, method="spearman")
cor.test(t$floor, t$symp, method="spearman")
cor.test(t$unit, t$symp, method="spearman")
cor.test(t$areapc, t$symp, method="spearman")

# multivariate model
# district
fit1 <- vglm(symp ~ city + as.factor(floor) + unit + I(areapc/10) + income + edu + edu2 + age, data=t, 
   cumulative(parallel = ~ -1 + city + as.factor(floor) + unit + I(areapc/10) + income, reverse = T))

# neighborhood
fit2 <- vglm(symp ~ as.factor(floor) + unit + I(areapc/10) + income + edu + edu2 + age, data=t, 
   cumulative(parallel = ~ -1 + as.factor(floor) + unit + I(areapc/10) + income, reverse = T))

# household
fit3 <- vglm(symp ~ I(areapc/10) + income + edu + edu2 + age, data=t, 
             cumulative(parallel = ~ -1 + I(areapc/10) + income, reverse = T))
summary(fit1)

#### model result output ####
olr_table <- data.frame(var=c('Urban', '# Floors (Ref: Single-family house)', 
                              '  Very low rise', '  Low rise', '  Lower mid-rise', '  Higher mid-rise', '  High rise',
                              '# Units per floor', 'Floor area per capita', 'Income', 
                              'Education', '  Asymptomatic', '  No fever', '  Low fever', '  High fever',
                              'Education of household', '  Asymptomatic', '  No fever', '  Low fever', '  High fever',
                              'Age', '  Asymptomatic', '  No fever', '  Low fever', '  High fever',
                              'Threshold', '  Asymptomatic', '  No fever', '  Low fever', '  High fever'), 
                        b1=0, se1=0, b2=0, se2=0, b3=0, se3=0)
bout <- function(x){
  bx <- format(round(b[x], 2))
  #sex <- format(round(se[x], 4), scientific = F)
  if(p[x] > 0.05){
    #return(paste0(bx, '\n(', sex, ')'))
    return(bx)
  }
  if(p[x] <= 0.05 & p[x] > 0.01){
    #return(paste0(bx, '*\n(', sex, ')'))
    return(paste0(bx, '*'))
  }
  if(p[x] <= 0.01 & p[x] > 0.001){
    #return(paste0(bx, '**\n(', sex, ')'))
    return(paste0(bx, '**'))
  }
  if(p[x] <= 0.001){
    #return(paste0(bx, '***\n(', sex, ')'))
    return(paste0(bx, '***'))
  }
}
b <- summary(fit1)@coef3[c(5:25, 1:4), 1]
se <- summary(fit1)@coef3[c(5:25, 1:4), 2]
p  <- summary(fit1)@coef3[c(5:25, 1:4), 4]
olr_table$b1[c(1, 3:10, 12:15, 17:20, 22:25, 27:30)] <- sapply(1:25, bout)
olr_table$se1[c(1, 3:10, 12:15, 17:20, 22:25, 27:30)] <- format(round(se, 4), scientific = F)
b <- summary(fit2)@coef3[c(5:24, 1:4), 1]
se <- summary(fit2)@coef3[c(5:24, 1:4), 2]
p  <- summary(fit2)@coef3[c(5:24, 1:4), 4]
olr_table$b2[c(3:10, 12:15, 17:20, 22:25, 27:30)] <- sapply(1:24, bout)
olr_table$se2[c(3:10, 12:15, 17:20, 22:25, 27:30)] <- format(round(se, 4), scientific = F)
b <- summary(fit3)@coef3[c(5:18, 1:4), 1]
se <- summary(fit3)@coef3[c(5:18, 1:4), 2]
p  <- summary(fit3)@coef3[c(5:18, 1:4), 4]
olr_table$b3[c(9:10, 12:15, 17:20, 22:25, 27:30)] <- sapply(1:18, bout)
olr_table$se3[c(9:10, 12:15, 17:20, 22:25, 27:30)] <- format(round(se, 4), scientific = F)
write.csv(olr_table, 'olr_table.csv')



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



#### by age ####
s <- t %>% mutate(age = dplyr::recode(age, '1'='u18', '2'='1829', '3'='3039',
                                      '4'='4049', '5'='50+', '6'='50+', '7'='50+'))
agegroup <- c('u18', '1829', '3039', '4049', '50+')
ageout <- data.frame(var=c('Floor area per capita', '# Floors (Ref: Single-family house)', 
                           '  Very low rise', '  Low rise', '  Lower mid-rise', '  Higher mid-rise', '  High rise',
                           '# Units per floor', 'Urban'), b_u18=0, b_1829=0, b_3039=0, b_4049=0, b_50=0)
for (i in 1:5){
  fit1 <- vglm(symp ~ I(areapc/10) + income + edu + edu2, 
               data=filter(s, age==agegroup[i]), 
               cumulative(parallel = ~ -1 + city + as.factor(floor) + unit + I(areapc/10) + income,
                          reverse = T))
  fit2 <- vglm(symp ~ I(areapc/10) + as.factor(floor) + unit + income + edu + edu2, 
               data=filter(s, age==agegroup[i]), 
               cumulative(parallel = ~ -1 + city + as.factor(floor) + unit + I(areapc/10) + income,
                          reverse = T))
  fit3 <- vglm(symp ~ I(areapc/10) + as.factor(floor) + unit + city + income + edu + edu2, 
               data=filter(s, age==agegroup[i]), 
               cumulative(parallel = ~ -1 + city + as.factor(floor) + unit + I(areapc/10) + income,
                          reverse = T))
  b <- summary(fit1)@coef3[5, 1]
  se <- summary(fit1)@coef3[5, 2]
  ageout[1, i+1] <- paste0(format(round(b, 2)), '\n(', format(round(se, 4), scientific=F), ')')
  b <- summary(fit2)@coef3[6:11, 1]
  se <- summary(fit2)@coef3[6:11, 2]
  ageout[3:8, i+1] <- paste0(format(round(b, 2)), '\n(', format(round(se, 4), scientific=F), ')')
  b <- summary(fit3)@coef3[12, 1]
  se <- summary(fit3)@coef3[12, 2]
  ageout[9, i+1] <- paste0(format(round(b, 2)), '\n(', format(round(se, 4), scientific=F), ')')
}
write.csv(ageout, "age_output.csv")








#### robustness check ####
rout <- data.frame(var=c('Floor area per capita', '# Floors (Ref: Single-family house)', 
                         '  Very low rise', '  Low rise', '  Lower mid-rise', '  Higher mid-rise', '  High rise',
                         '# Units per floor', 'Urban'), b_def=0, b_c01=0, b_c12=0, b_c23=0)
sr_code <- c("'0'=0", "'0'=1", "'1'=2", "'2'=3")
lev <- c(1, 0, 0, 0)
for (i in 1:4){
  q <- lev[i]
  tr <- eval(parse(text=paste0('mutate(t, symp = recode(symp, ', sr_code[i], '))')))
  fit1 <- vglm(symp ~ I(areapc/10) + income + edu + edu2 + age, data=tr, 
               cumulative(parallel = ~ -1 + city + as.factor(floor) + unit + I(areapc/10) + income,
                          reverse = T))
  fit2 <- vglm(symp ~ I(areapc/10) + as.factor(floor) + unit + income + edu + edu2 + age, data=tr, 
               cumulative(parallel = ~ -1 + city + as.factor(floor) + unit + I(areapc/10) + income,
                          reverse = T))
  fit3 <- vglm(symp ~ I(areapc/10) + as.factor(floor) + unit + city + income + edu + edu2 + age, data=tr, 
               cumulative(parallel = ~ -1 + city + as.factor(floor) + unit + I(areapc/10) + income,
                          reverse = T))
  b <- summary(fit1)@coef3[4 + q, 1]
  se <- summary(fit1)@coef3[4 + q, 2]
  rout[1, i+1] <- paste0(format(round(b, 2)), '\n(', format(round(se, 4), scientific=F), ')')
  b <- summary(fit2)@coef3[5:10 + q, 1]
  se <- summary(fit2)@coef3[5:10 + q, 2]
  rout[3:8, i+1] <- paste0(format(round(b, 2)), '\n(', format(round(se, 4), scientific=F), ')')
  b <- summary(fit3)@coef3[11 + q, 1]
  se <- summary(fit3)@coef3[11 + q, 2]
  rout[9, i+1] <- paste0(format(round(b, 2)), '\n(', format(round(se, 4), scientific=F), ')')
}
write.csv(rout, "robust_output.csv")



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


#### for plot ####
s <- t %>% dplyr::select(symp, district, 
                         city, floor, unit, areapc) %>%
  mutate(symp = dplyr::recode(symp, '0'="uninfected", '1'="asymptomatic", '2'="no fever", 
                              '3'="low fever", '4'='high fever'),
         city = dplyr::recode(city, '0'='rural', '1'='urban'),
         floor = dplyr::recode(floor, '6'='high rise', '5'='higher mid-rise', '4'='lower mid-rise', 
                               '3'='low rise', '2'='very low rise', '1'='single family house'))
write.csv(s, 'forfig1.csv')  

s <- t %>% mutate(age = dplyr::recode(age, '1'='u18', '2'='1829', '3'='3039',
                                      '4'='4049', '5'='50+', '6'='50+', '7'='50+'))
aop <- data.frame(age = rep(c('<18', '18-29', '30-39', '40-49', '>=50'), each=8), 
    var = rep(c('urban', 'verylowrise', 'lowrise', 'lowermidrise', 'highermidrise',
                'highrise', 'units', 'areapc'), 5), est = 0, est_l = 0, est_u = 0)
ag <- c('u18', '1829', '3039', '4049', '50+')
for (i in 1:5){
  fit1 <- vglm(symp ~ I(areapc/10) + income + edu + edu2, data=filter(s, age==ag[i]), 
       cumulative(parallel = ~ -1 + I(areapc/10) + income, reverse = T))
  b <- summary(fit1)@coef3[5, 1]
  se <- summary(fit1)@coef3[5, 2]
  aop[8*i,3] <- exp(b)-1
  aop[8*i,4] <- exp(b - se * 1.96) - 1
  aop[8*i,5] <- exp(b + se * 1.96) - 1
  fit2 <- vglm(symp ~ I(areapc/10) + as.factor(floor) + unit + income + edu + edu2, data=filter(s, age==ag[i]), 
      cumulative(parallel = ~ -1 + as.factor(floor) + unit + I(areapc/10) + income, reverse = T))
  b <- summary(fit2)@coef3[6:11, 1]
  se <- summary(fit2)@coef3[6:11, 2]
  aop[(8*i-6):(8*i-1),3] <- exp(b)-1
  aop[(8*i-6):(8*i-1),4] <- exp(b - se * 1.96) - 1
  aop[(8*i-6):(8*i-1),5] <- exp(b + se * 1.96) - 1
  fit3 <- vglm(symp ~ I(areapc/10) + as.factor(floor) + unit + city + income + edu + edu2, 
  data=filter(s, age==ag[i]), cumulative(parallel = ~ -1 + city + as.factor(floor) + unit + I(areapc/10) + income,reverse = T))
  b <- summary(fit3)@coef3[12, 1]
  se <- summary(fit3)@coef3[12, 2]
  aop[(8*i-7),3] <- exp(b)-1
  aop[(8*i-7),4] <- exp(b - se * 1.96) - 1
  aop[(8*i-7),5] <- exp(b + se * 1.96) - 1
}
write.csv(aop, 'forfig2.csv') 
  
  
