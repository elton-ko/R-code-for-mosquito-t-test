#Sets working directory
setwd("")

#Function to take the inverse of logit to backtransform means and SE
inv.logit<-function(x) 1/(1+1/(exp(x)))

#Reads in Data
UVvB<-read.csv("Blue 715 vs continuous stats.csv")
#Creates response variable, 2 column object with treatment and control
resp<-with(UVvB,cbind(Treatment, Control))

#Model with intercept, intercept here indicates the mean value in the 
#logit space is not 0, and the logit of 50% is 0. 
#So if the intercept is significant the proportion is not 50%
model.1<-glm(resp~1, quasibinomial)
#Model without an intercept
model.2<-glm(resp~0, quasibinomial)
#Significance test of intercept comparing the two models, report this
#F-value and p-value
anova(model.1,model.2, test="F")

#model summary
summary(model.1)

#Assumption tests, just checking normality
par(mfrow=c(2,2))
plot(model.1)

#extract the treatment proportion (still a logit) from model
treat<-model.1$coefficients
#extract the SE of treatment proportion (still a logit) from model
se<-summary(model.1)$coefficients[,2]
#creates a vector with treatment proportion and the upper/lower SE bar values 
t.props<-c(treat, treat-se, treat+se)
#Back-transformes the logit values to proportions
t.props<-inv.logit(t.props)
#Creates table from back-transformed data and calculates 
#control values from treatment values (control=1-treatment)
data.frame(	"Proportion"=c(t.props[1], 1-t.props[1]), 
						"Upper SE"=c(t.props[2], 1-t.props[2]), 
						"Lower SE"=c(t.props[3], 1-t.props[3]), 
						row.names=c("treatment", "control"))
#