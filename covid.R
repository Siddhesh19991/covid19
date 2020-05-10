covid_data <- read.csv("~/Downloads/COVID19_line_list_data.csv")

 library(Hmisc)

describe(covid_data)

covid_data$death<-as.integer(covid_data$death!=0)

#death rate

deathrate<-sum(covid_data$death)/nrow(covid_data)

#age

alive<-subset(covid_data,covid_data$death==0)
death<-subset(covid_data,covid_data$death==1)

mean_alive<-mean(alive$age,na.rm = TRUE)
mean_death<-mean(death$age,na.rm = TRUE)

#according to this people who how died are older than people who recovered


#doing a t-test to check if there is any statistical difference betweeen the two

ttest<-t.test(alive$age,death$age,conf.level = 0.95)

#from the test we can see that the p value<0.05 , so we reject the null hypothesis.
#therefore we can say that older people are more likely to die from the virus

#gender

female<-subset(covid_data,covid_data$gender=="female")
male<-subset(covid_data,covid_data$gender=="male")

maledeath<-mean(male$death)#8.46%
femaledeath<-mean(female$death)#3.66%

t.test(male$death,female$death,conf.level = 0.95)

#since the p-value is less than 0.05 we can reject the null hypothesis that man are more likley to suffer from the virus.
#and therefore say that this is statistically significant.

