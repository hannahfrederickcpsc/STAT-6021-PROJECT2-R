###Exploratory Analysis###

library(ggplot2)

data_white<-read.csv('wineQualityWhites.csv',header=TRUE)
data_white['color']='white'

data_red<-read.csv('wineQualityReds.csv',header=TRUE)
data_red['color']='red'
colnames(data_red)

data<-merge(data_red,data_white,by=c("fixed.acidity","volatile.acidity",
                                     "citric.acid","residual.sugar","chlorides",
                                     "free.sulfur.dioxide","total.sulfur.dioxide",
                                     "density","pH","sulphates","alcohol",
                                     "quality","color"),all=T)

data<-subset(data, select=c("fixed.acidity","volatile.acidity","citric.acid",
                            "residual.sugar","chlorides","free.sulfur.dioxide",
                            "total.sulfur.dioxide", "density","pH","sulphates",
                            "alcohol","quality","color"))

head(data)
table(data$quality)
table(data$color)
summary(data)

################
##Quality White vs Red
###############
ggplot(data, aes(x=as.numeric(as.character(quality)), color=color, fill=color)) +
  geom_bar(position="dodge", alpha=.5)+
  scale_x_continuous(breaks=seq(2,10,1), lim=c(2,10)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,2250)) +
  xlab('Quality') +
  ylab('Quantity')

ggplot(data, aes(x=as.numeric(as.character(quality)), color=color, fill=color)) +
  geom_bar(position="dodge", alpha=.5)+
  scale_x_continuous(breaks=seq(2,10,1), lim=c(2,10)) +
  scale_y_sqrt(expand = c(0,0),limits = c(0,2250)) + #sqrt used to better see smaller 9 bar
  xlab('Quality') +
  ylab('Quantity')

qplot(x=color, y=quality, data=data, geom="boxplot", color=color)

qplot(data=data, quality, binwidth=1, color=color, geom="density") +
  scale_x_continuous(breaks=seq(3, 9, 1))
#########################
## both appear to be normally distributed but with different peaks

#################
#Individual Data Visualization
#################

#Alcohol
ggplot(data, aes(x=alcohol, color=color, fill=color)) +
  geom_density(position="dodge", alpha=.5)+
  xlab('Alcohol Content')+
  ylab('')

#Density
ggplot(data, aes(x=density, color=color, fill=color)) +
  geom_density(position="dodge", alpha=.5)+
  scale_x_continuous(breaks=seq(.98,1.01,0.005),lim=c(.9855,1.005))+
  xlab('Wine Density')+
  ylab('')

#Volitile Acidity
ggplot(data, aes(x=volatile.acidity, color=color, fill=color)) +
  geom_density(position="dodge", alpha=.5)+
  scale_x_continuous(breaks=seq(.07,1.6,0.1),lim=c(.07,1.6))+
  xlab('Volatile Acidity')+
  ylab('')

#Chlorides
ggplot(data, aes(x=chlorides, color=color, fill=color)) +
  geom_density(position="dodge", alpha=.5)+
  scale_x_continuous(breaks=seq(0,.7,0.05),lim=c(0,.15))+
  xlab('Chlorides')+
  ylab('')

#############
##Box plots for skewed data/outlier visualization
############

#Residual Sugar
qplot(data=data,residual.sugar, geom="boxplot")

#Free Sulfur Dioxide
qplot(data=data,free.sulfur.dioxide, geom="boxplot")

#Toal Sulfur Dioxide
qplot(data=data,total.sulfur.dioxide, geom="boxplot")

#Sulphates
qplot(data=data,sulphates, geom="boxplot")

#Individual Charts of all variables against quality
qplot(data=data,x=fixed.acidity, y=quality)
qplot(data=data,x=volatile.acidity, y=quality)
qplot(data=data,x=citric.acid, y=quality)
qplot(data=data,x=residual.sugar, y=quality)
qplot(data=data,x=chlorides, y=quality)
qplot(data=data,x=free.sulfur.dioxide, y=quality)
qplot(data=data,x=total.sulfur.dioxide, y=quality)
qplot(data=data,x=density, y=quality)
qplot(data=data,x=pH, y=quality)
qplot(data=data,x=sulphates, y=quality)
qplot(data=data,x=alcohol, y=quality)
##No clear pattern, this leads up to believe that it is the right balance of 
##variables that gives a good quality score 

#########################
#Compare objective parameters of wines
#########################
# density vs. alcohol plot
ggplot(data=data, aes_string(x="density", y="alcohol", color="color")) +
  geom_point(alpha=1/5, position = position_jitter(h=0), size=2) +
  geom_smooth(method='lm')+ 
  coord_cartesian(xlim=c(min(data$density),1.005), ylim=c(7.5,15))

# density vs. fixed.acidity plot
ggplot(data=data, aes_string(x="density", y="fixed.acidity", color="color")) +
  geom_point(alpha=1/5, position = position_jitter(h=0), size=2) +
  geom_smooth(method='lm')+ 
  coord_cartesian(xlim=c(min(data$density),1.005))

# residual.sugar vs. total.sulfur.dioxide
ggplot(data=data, aes_string(x="residual.sugar", y="total.sulfur.dioxide", color="color")) +
  geom_point(alpha=1/5, position = position_jitter(h=0), size=2) +
  geom_smooth(method='lm')+ 
  scale_x_log10() +
  coord_cartesian(xlim=c(min(data$residual.sugar),30),
                  ylim=c(min(data$total.sulfur.dioxide), 350))

# free.sulfur.dioxide vs. total.sulfur.dioxide
ggplot(data=data, aes_string(x="free.sulfur.dioxide", y="total.sulfur.dioxide",
                             color="color")) +
  geom_point(alpha=1/5, position = position_jitter(h=0), size=2) +
  geom_smooth(method='lm')+ 
  scale_x_log10() +
  coord_cartesian(xlim=c(min(data$free.sulfur.dioxide),30),
                  ylim=c(min(data$total.sulfur.dioxide), 350))

# residual.sugar vs. density
ggplot(data=data, aes_string(x="residual.sugar", y="density", color="color")) +
  geom_point(alpha=1/5, position = position_jitter(h=0), size=2) +
  geom_smooth(method='lm')+ 
  coord_cartesian(xlim=c(min(data$residual.sugar),25),
                  ylim=c(min(data$density), 1.005))

# residual.sugar vs. alcohol
ggplot(data=data, aes_string(x="residual.sugar", y="alcohol", color="color")) +
  geom_point(alpha=1/5, position = position_jitter(h=0), size=2) +
  geom_smooth(method='lm')+ 
  coord_cartesian(xlim=c(min(data$residual.sugar),20),
                  ylim=c(min(data$alcohol), 14))+ theme(legend.position="top")

# chlorides vs. density
ggplot(data=data, aes_string(x="chlorides", y="density", color="color")) +
  geom_point(alpha=1/5, position = position_jitter(h=0), size=2) +
  geom_smooth(method='lm')+ 
  scale_x_log10() + 
  coord_cartesian(ylim=c(min(data$density), 1.005))

# chlorides vs. sulphates
ggplot(data=data, aes_string(x="chlorides", y="sulphates", color="color")) +
  geom_point(alpha=1/5, position = position_jitter(h=0), size=2) +
  geom_smooth(method='lm')+ 
  scale_x_log10() + 
  coord_cartesian(ylim=c(min(data$sulphates), 1))

# Quality vs. Alcohol
ggplot(data=data, aes(y=alcohol, x=quality)) +
  geom_point(alpha=1/4, position=position_jitter(h=0), size=4) +
  geom_smooth(method='lm') +
  facet_wrap(~ color)

######################
#Significantly Different Parameters for Red and White Wine
#####################

#Fixed Acidity vs. Color
qplot(data=data, x=color, y=quality, geom="boxplot")


#Fixed Acidity vs. Color
qplot(data=data, x=color, y=fixed.acidity, geom="boxplot")

#Volatile Acidity vs. Color
qplot(data=data, x=color, y=volatile.acidity, geom="boxplot")

#Residual Sugar vs. Color
qplot(data=data, x=color, y=residual.sugar, geom="boxplot")

#Total Sulfur Dioxide vs. Color
qplot(data=data,x=color, y=total.sulfur.dioxide, geom="boxplot")

#################
##Exploring Wine Parameters and Quality
#################

#Chlorides and Sulphates
data$quality <- as.factor(data$quality) #converting 'quality' vector into factor variable
ggplot(data=data,aes(x=chlorides, y=sulphates, color=quality)) +
  facet_wrap(~color) +
  geom_point(size=3, alpha=1/4) +
  scale_color_identity(guide='legend') +
  ylim(min(data$sulphates), quantile(data$sulphates, 0.95)) +
  xlim(min(data$chlorides), quantile(data$chlorides, 0.95))

#Fixed Acidity and Volatile Acidity
ggplot(data=data,aes(x=fixed.acidity,y=volatile.acidity,color=quality))+
  facet_wrap(~color) +
  geom_point(size=3, alpha=1/4) +
  scale_color_identity(guide='legend') +
  ylim(min(data$volatile.acidity),quantile(data$volatile.acidity, 0.99)) +
  xlim(min(data$fixed.acidity),quantile(data$fixed.acidity, 0.99))

#Free Sulfur Dioxide and Total Sulfur Dioxide
ggplot(data=data,aes(x=free.sulfur.dioxide,y=total.sulfur.dioxide,color=quality)) +
  facet_wrap(~color) +
  geom_point(size=3, alpha=1/4) +
  scale_color_identity(guide='legend') +
  ylim(min(data$total.sulfur.dioxide),quantile(data$total.sulfur.dioxide, 0.95)) +
  xlim(min(data$free.sulfur.dioxide),quantile(data$free.sulfur.dioxide, 0.95))

#PH and Alcohol
ggplot(data = data,aes(x=pH, y=alcohol, color=quality)) +
  facet_wrap(~color) +
  geom_point(size=3, alpha=1/4) +
  scale_color_identity(guide='legend') +
  ylim(min(data$alcohol), quantile(data$alcohol, 0.95)) +
  xlim(min(data$pH), quantile(data$pH, 0.95))

#Citric Acid and Alcohol
ggplot(data = data, aes(x=citric.acid, y=alcohol, color=quality)) +
  facet_wrap(~color) +
  geom_point(size=3, alpha=1/4) +
  scale_color_identity(guide='legend') +
  ylim(min(data$alcohol), quantile(data$alcohol, 0.95)) +
  xlim(min(data$citric.acid), quantile(data$citric.acid, 0.95))

#Density vs. Alcohol correlation by Color
ggplot(data=data,aes(x=density, y=alcohol, color=color)) +
  geom_point(alpha=1/6, position=position_jitter(h=0), size=3) +
  geom_smooth(method='lm') +
  coord_cartesian(xlim=c(min(data$density),1.005), ylim=c(8,15)) +
  xlab('Density') +
  ylab('Alcohol') +
  ggtitle('Density vs. Alcohol correlation by Color')

ggplot(data=data,aes(x=density, y=alcohol, color=factor(quality))) +
  geom_point(alpha=1/2, position=position_jitter(h=0), size=2) +
  coord_cartesian(xlim=c(min(data$density),1.005), ylim=c(8,15)) +
  scale_color_brewer(type='qual') +
  xlab('Density') +
  ylab('Alcohol') +
  ggtitle('Density vs. Alcohol correlation by Quality')

ggplot(data=data,aes(x=density, y=alcohol) )+
  facet_wrap( ~ quality) +
  geom_boxplot() +
  xlab('Density') +
  ylab('Alcohol') +
  ggtitle('Density vs. Alcohol correlation by Quality')

#Quality vs. Alcohol
ggplot(data=data, aes(y=alcohol, x=quality)) +
  geom_boxplot() +
  geom_smooth(method = 'lm') +
  facet_wrap(~ color) +
  xlab('Quality') +
  ylab('Alcohol') +
  ggtitle('How Alcohol Level Affects Wine Quality')
##0.4 is not a high correlation level so we cannot use alcohol as a parameter 
##for quality prediction

###Unequal Red and White Together###

data_white <- read.csv('wineQualityWhites.csv',header = TRUE)
data_white['color']='white'

data_red <- read.csv('wineQualityReds.csv',header = TRUE)
data_red['color']='red'

data <- merge(data_red,data_white,by=c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide", "density","pH","sulphates","alcohol","quality","color"),all=T)
binary_quality <- ifelse(data$quality < 7,"bad","good")
data <- cbind(data, binary_quality)
data$binary_quality <-factor(data$binary_quality)
data2 <- subset(data, select = c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide", "density","pH","sulphates","alcohol","binary_quality","color"))

library(tidyverse)
sample_quality <- data2 %>% filter(binary_quality == 'bad')
sample_good <- data2 %>% filter(binary_quality == 'good')
set.seed(199)
sample2 <-sample.int(nrow(sample_quality), floor(nrow(sample_good)), replace = F)
data_bad_quality<-sample_quality[sample2, ]

data3 <- merge(sample_good,data_bad_quality,by=c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide", "density","pH","sulphates","alcohol","binary_quality","color"),all=T)
data3 <- subset(data3, select = c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide", "density","pH","sulphates","alcohol","binary_quality","color"))

head(data3)
attach(data3)

set.seed(199)

##choose the observations to be in the training. I am splitting the dataset into halves
sample<-sample.int(nrow(data3), floor(.50*nrow(data3)), replace = F)
train<-data3[sample, ]
test<-data3[-sample, ]

#fit the full model
result<-glm(binary_quality ~ ., family = "binomial", data=train)
summary(result)

#confirm the model with all predictors is useful
1-pchisq(result$null.deviance-result$deviance,12)

library(ROCR)
preds<-predict(result,newdata=test, type="response")

##produce the numbers associated with classification table
rates<-prediction(preds, test$binary_quality)

##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for Quality of Wine")
lines(x = c(0,1), y = c(0,1), col="red")

#determine the AUC
auc <- performance(rates, measure = "auc")
auc@y.values[[1]] #0.8177859

#look at the confusion matrix with threshold=0.5
table(test$binary_quality, preds>0.5)
#false positive rate = 177/(449+177) = 0.2827
#false negative rate = 155/(155+496) = 0.2380952
#we want a lower false positive rate

#use automated search procedures to find a reduced model
##intercept only model
regnull <- glm(binary_quality~1,family = "binomial",data=data3)
##model with all predictors
regfull <- glm(binary_quality~.,family = "binomial", data=data3)
##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward") 
#best forward predictors: alcohol, volatile.acidity, sulphates, residual.sugar, chlorides, color, free.sulfur.dioxide, total.sulfur.dioxide, pH, fixed.acidity, density
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
#best backward predictors: fixed.acidity, volatile.acidity, residual.sugar, chlorides, free.sulfur.dioxide, total.sulfur.dioxide, density, pH, sulphates, alcohol, color
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")
#best stepwise predictors: alcohol, volatile.acidity, sulphates, residual.sugar, chlorides, color, free.sulfur.dioxide, total.sulfur.dioxide, pH, fixed.acidity, density
#all three regressions are the same

reduced<-glm(binary_quality ~ alcohol + volatile.acidity + sulphates + residual.sugar + chlorides + citric.acid + color + pH + fixed.acidity + density, family = "binomial", data=train)
summary(reduced)

#confirm the model with all predictors is useful
1-pchisq(reduced$null.deviance-reduced$deviance,10)

library(ROCR)
preds2<-predict(reduced,newdata=test, type="response")

##produce the numbers associated with classification table
rates2<-prediction(preds2, test$binary_quality)

##store the true positive and false postive rates
roc_result2<-performance(rates2,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result2, main="ROC Curve for Quality of Wine")
lines(x = c(0,1), y = c(0,1), col="red")

#determine the AUC
auc <- performance(rates2, measure = "auc")
auc@y.values[[1]] #0.8179478

#look at the confusion matrix with threshold=0.5
table(test$binary_quality, preds>0.5)
#false positive rate = 177/(449+177) = 0.2827
#false negative rate = 155/(155+496) = 0.2380952
#we want a lower false positive rate

#fail to reject the null hypothesis, so favor the reduced model
1-pchisq(reduced$deviance-result$deviance,2) #0.4237744

###Equal Red and White Together###

data_white <- read.csv('wineQualityWhites.csv',header = TRUE)
data_white['color']='white'

data_red <- read.csv('wineQualityReds.csv',header = TRUE)
data_red['color']='red'

set.seed(199)
data_white_sample <-sample.int(nrow(data_white), floor(nrow(data_red)), replace = F)
data_white_sample

data_white_use<-data_white[data_white_sample, ]

data <- merge(data_red,data_white_use,by=c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide", "density","pH","sulphates","alcohol","quality","color"),all=T)
binary_quality <- ifelse(data$quality < 7,"bad","good")
data <- cbind(data, binary_quality)
data$binary_quality <-factor(data$binary_quality)
data2 <- subset(data, select = c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide", "density","pH","sulphates","alcohol","binary_quality","color"))

library(tidyverse)
sample_quality <- data2 %>% filter(binary_quality == 'bad')
sample_good <- data2 %>% filter(binary_quality == 'good')
set.seed(199)
sample2 <-sample.int(nrow(sample_quality), floor(nrow(sample_good)), replace = F)
data_bad_quality<-sample_quality[sample2, ]

data3 <- merge(sample_good,data_bad_quality,by=c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide", "density","pH","sulphates","alcohol","binary_quality","color"),all=T)
data3 <- subset(data3, select = c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide", "density","pH","sulphates","alcohol","binary_quality","color"))

table(data3$color)

##choose the observations to be in the training. I am splitting the dataset into halves
sample<-sample.int(nrow(data3), floor(.50*nrow(data3)), replace = F)
train<-data3[sample, ]
test<-data3[-sample, ]

#fit the full model
result<-glm(binary_quality ~ ., family = "binomial", data=train)
summary(result)

#confirm the model with all predictors is useful
1-pchisq(result$null.deviance-result$deviance,12)

result$null.deviance-result$deviance

library(ROCR)
preds<-predict(result,newdata=test, type="response")

##produce the numbers associated with classification table
rates<-prediction(preds, test$binary_quality)

##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for Quality of Wine")
lines(x = c(0,1), y = c(0,1), col="red")

#determine the AUC
auc <- performance(rates, measure = "auc")
auc@y.values[[1]] #0.8463924

#look at the confusion matrix with threshold=0.5
table(test$binary_quality, preds>0.5)
#false positive rate = 64/(221+64) = 0.2245614
#false negative rate = 62/(222+62) = 0.2183099
#we want a lower false positive rate

#use automated search procedures to find a reduced model
##intercept only model
regnull <- glm(binary_quality~1,family = "binomial",data=data3)
##model with all predictors
regfull <- glm(binary_quality~.,family = "binomial", data=data3)
##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
#best forward predictors: alcohol, volatile.acidity, sulphates, residual.sugar, chlorides, total.sulfur.dioxide, fixed.acidity, density, pH, free.sulfur.dioxide
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
#best backward predictors: fixed.acidity, volatile.acidity, residual.sugar, chlorides, free.sulfur.dioxide, total.sulfur.dioxide, density, pH, sulphates, alcohol
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")
#best stepwise predictors: alcohol, volatile.acidity, sulphates, residual.sugar, chlorides, total.sulfur.dioxide, fixed.acidity, density, pH, free.sulfur.dioxide
#all three regressions are the same

reduced<-glm(binary_quality ~ alcohol + volatile.acidity + sulphates + residual.sugar + chlorides + total.sulfur.dioxide + fixed.acidity + density + pH + free.sulfur.dioxide, family = "binomial", data=train)
summary(reduced)

#confirm the model with all predictors is useful
1-pchisq(reduced$null.deviance-reduced$deviance,10)

reduced$null.deviance-reduced$deviance

library(ROCR)
preds2<-predict(reduced,newdata=test, type="response")

##produce the numbers associated with classification table
rates2<-prediction(preds2, test$binary_quality)

##store the true positive and false postive rates
roc_result2<-performance(rates2,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result2, main="ROC Curve for Quality of Wine")
lines(x = c(0,1), y = c(0,1), col="red")

#determine the AUC
auc <- performance(rates2, measure = "auc")
auc@y.values[[1]] #0.8473808

#look at the confusion matrix with threshold=0.5
table(test$binary_quality, preds>0.5)
#false positive rate = 64/(221+64) = 0.2245614
#false negative rate = 62/(222+62) = 0.2183099
#we want a lower false positive rate

#fail to reject the null hypothesis, so favor the reduced model
1-pchisq(reduced$deviance-result$deviance,2) #0.5591237

reduced$deviance-result$deviance

###Equal Red and White Separate###

data_white <- read.csv('wineQualityWhites.csv',header = TRUE)
data_white['color']='white'

data_red <- read.csv('wineQualityReds.csv',header = TRUE)
data_red['color']='red'


set.seed(199)
data_white_sample <-sample.int(nrow(data_white), floor(nrow(data_red)), replace = F)
data_white_sample

data_white_use<-data_white[data_white_sample, ]

data <- merge(data_red,data_white_use,by=c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide", "density","pH","sulphates","alcohol","quality","color"),all=T)
binary_quality <- ifelse(data$quality < 7,"bad","good")
data <- cbind(data, binary_quality)
data$binary_quality <-factor(data$binary_quality)
data2 <- subset(data, select = c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide", "density","pH","sulphates","alcohol","binary_quality","color"))

library(tidyverse)
sample_quality <- data2 %>% filter(binary_quality == 'bad')
sample_good <- data2 %>% filter(binary_quality == 'good')
set.seed(199)
sample2 <-sample.int(nrow(sample_quality), floor(nrow(sample_good)), replace = F)
data_bad_quality<-sample_quality[sample2, ]

data3 <- merge(sample_good,data_bad_quality,by=c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide", "density","pH","sulphates","alcohol","binary_quality","color"),all=T)
data3 <- subset(data3, select = c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide", "density","pH","sulphates","alcohol","binary_quality","color"))

head(data3)
attach(data3)

white_data <- data3[ which(data3$color=='white'), ]
white_data <- subset(white_data, select = c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide", "density","pH","sulphates","alcohol","binary_quality"))
red_data <- data3[ which(data3$color=='red'), ]
red_data <- subset(red_data, select = c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide", "density","pH","sulphates","alcohol","binary_quality"))

table(white_data$binary_quality)
table(red_data$binary_quality)

head(white_data)
head(red_data)
detach(data3)
#white only model
attach(white_data)
set.seed(199)

##choose the observations to be in the training. I am splitting the dataset into halves
sample<-sample.int(nrow(white_data), floor(.50*nrow(white_data)), replace = F)
train<-white_data[sample, ]
test<-white_data[-sample, ]

#fit the full model
result<-glm(binary_quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, family = "binomial", data=train)
summary(result)

#confirm the model with all predictors is useful
1-pchisq(result$null.deviance-result$deviance,11)

library(ROCR)
preds<-predict(result,newdata=test, type="response")

##produce the numbers associated with classification table
rates<-prediction(preds, test$binary_quality)

##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for Quality of White Wine")
lines(x = c(0,1), y = c(0,1), col="red")

#determine the AUC
auc <- performance(rates, measure = "auc")
auc@y.values[[1]] #0.7756067

#look at the confusion matrix with threshold=0.5
table(test$binary_quality, preds>0.5)
#false positive rate = 36/(94+36) = 0.2769231
#false negative rate = 48/(48+139) = 0.2566845
#we want a lower false positive rate

#use automated search procedures to find a reduced model
##intercept only model
regnull <- glm(binary_quality~1,family = "binomial",data=white_data)
##model with all predictors
regfull <- glm(binary_quality~.,family = "binomial", data=white_data)
##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
#best forward predictors: alcohol, chlorides, volatile.acidity, residual.sugar
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
#best backward predictors: fixed.acidity, volatile.acidity, residual.sugar, density, pH, sulphates
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")
#best stepwise predictors: alcohol, volatile.acidity, residual.sugar, chlorides
#not all three regressions are the same

reduced<-glm(binary_quality ~ chlorides + volatile.acidity + residual.sugar + alcohol, family = "binomial", data=train)
summary(reduced)

#confirm the model with all predictors is useful
1-pchisq(reduced$null.deviance-reduced$deviance,4)

library(ROCR)
preds2<-predict(reduced,newdata=test, type="response")

##produce the numbers associated with classification table
rates2<-prediction(preds2, test$binary_quality)

##store the true positive and false postive rates
roc_result2<-performance(rates2,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result2, main="ROC Curve for Quality of White Wine")
lines(x = c(0,1), y = c(0,1), col="red")

#determine the AUC
auc <- performance(rates2, measure = "auc")
auc@y.values[[1]] #0.779638

#look at the confusion matrix with threshold=0.5
table(test$binary_quality, preds>0.5)
#false positive rate = 36/(94+36) = 0.2769231
#false negative rate = 48/(48+139) = 0.2566845
#we want a lower false positive rate

#fail to reject the null hypothesis, so favor the reduced model
1-pchisq(reduced$deviance-result$deviance,7) #0.4210097
detach(white_data)

#red wine only model
attach(red_data)
set.seed(199)

##choose the observations to be in the training. I am splitting the dataset into halves
sample<-sample.int(nrow(red_data), floor(.50*nrow(red_data)), replace = F)
train<-red_data[sample, ]
test<-red_data[-sample, ]

#fit the full model
result<-glm(binary_quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, family = "binomial", data=train)
summary(result)

#confirm the model with all predictors is useful
1-pchisq(result$null.deviance-result$deviance,11)

library(ROCR)
preds<-predict(result,newdata=test, type="response")

##produce the numbers associated with classification table
rates<-prediction(preds, test$binary_quality)

##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for Quality of Red Wine")
lines(x = c(0,1), y = c(0,1), col="red")

#determine the AUC
auc <- performance(rates, measure = "auc")
auc@y.values[[1]] #0.8789957

#look at the confusion matrix with threshold=0.5
table(test$binary_quality, preds>0.5)
#false positive rate = 22/(115+22) = 0.1605839
#false negative rate = 20/(96+20) = 0.1724138

#use automated search procedures to find a reduced model
##intercept only model
regnull <- glm(binary_quality~1,family = "binomial",data=red_data)
##model with all predictors
regfull <- glm(binary_quality~.,family = "binomial", data=red_data)
##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
#best forward predictors: alcohol, volatile.acidity, sulphates, total.sulfur.dioxide, residual.sugar, chlorides, fixed.acidity, density
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
#best backward predictors: fixed.acidity, volatile.acidity, residual.sugar, chlorides, total.sulfur.dioxide, density, sulphates, alcohol
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")
#best stepwise predictors: alcohol, volatile.acidity, sulphates, total.sulfur.dioxide, chlorides, fixed.acidity, residual.sugar, density
#all three regressions are the same

reduced<-glm(binary_quality ~ alcohol + volatile.acidity + sulphates + total.sulfur.dioxide + chlorides + fixed.acidity + residual.sugar + density, family = "binomial", data=train)
summary(reduced)

#confirm the model with all predictors is useful
1-pchisq(reduced$null.deviance-reduced$deviance,8)

library(ROCR)
preds2<-predict(reduced,newdata=test, type="response")

##produce the numbers associated with classification table
rates2<-prediction(preds2, test$binary_quality)

##store the true positive and false postive rates
roc_result2<-performance(rates2,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result2, main="ROC Curve for Quality of Red Wine")
lines(x = c(0,1), y = c(0,1), col="red")

#determine the AUC
auc <- performance(rates2, measure = "auc")
auc@y.values[[1]] #0.882079

#look at the confusion matrix with threshold=0.5
table(test$binary_quality, preds>0.5)
#false positive rate = 22/(115+22) = 0.1605839
#false negative rate = 20/(96+20) = 0.1724138
#we want a lower false positive rate

#fail to reject the null hypothesis, so favor the reduced model
1-pchisq(reduced$deviance-result$deviance,3) #0.7620423
