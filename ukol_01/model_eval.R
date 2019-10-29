getwd()
setwd("C:/Adam/School_FJFI/magisterske/3/DAS/DAS-ukoly/ukol_01")

library(lattice)
library(MASS)
library(car)
library(ggplot2)
library(leaps)
library(ISLR)
library(colorspace)

df <- read.table("empdata.csv",header=TRUE,sep=",")
colnames(df)
df[,c("over18")] <- list(NULL)
colnames(df)


#df$job_involvement <- as.factor(df$job_involvement)
#df$job_level <- as.factor(df$job_level)
#df$num_companies_worked <- as.factor(df$num_companies_worked)
#df$performance_rating <- as.factor(df$performance_rating)
#df$environment_satisfaction <- as.factor(df$environment_satisfaction)
#df$standard_hours <- as.factor(df$standard_hours)
#df$stock_option_level  <- as.factor(df$stock_option_level)
#df$work_life_balance <- as.factor(df$work_life_balance)



model1 <- glm(formula=attrition ~ ., 
             data=df, family=binomial(link = "logit"))
summary(model1)

model2 <- stepAIC(model1, k=2) ### AIC
summary(model2)

model3  <- stepAIC(model1, k=log(dim(df)[1])) ### BIC
summary(model3)

##BIC vybira:
# age, business_travel, department, distance_from_home, environment_satisfaction
# job)involvment, job)level, job_satisfaction, marital_status, num_companies_worked
# over_time, relationship_satisfaction, work_life_balance, years_in_current_role
# years_since_last_promotion

model4 <- glm(formula=attrition ~ 
                age + business_travel + department + distance_from_home 
                + environment_satisfaction + job_involvement + job_level  
                + job_satisfaction + marital_status + num_companies_worked
                + over_time + relationship_satisfaction + work_life_balance 
                + years_in_current_role + years_since_last_promotion, data=df, family=binomial(link = "logit"))
summary(model4)

model5 <- glm(formula=attrition ~ 
                environment_satisfaction + job_involvement + job_level  
              + job_satisfaction, 
              data=df, family=binomial(link = "logit"))
summary(model5)

model6 <- glm(formula=attrition ~ age + years_at_company + education, data=df, family=binomial(link = "logit"))
summary(model6)



























