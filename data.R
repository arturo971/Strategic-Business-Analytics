#data analysis
#Arturo Garcia
#ITESM

setwd("/Users/arturo/Dropbox/Coursera/Foundations of strategic business analytics/week4/capstone")
if (!("dates" %in% ls())){
        dates<-read.csv("Speed Dating Data.csv",header = T)
}
str(dates)
library(gmodels)
dates$income<-as.numeric(dates$income)
#Only16.5% of the dates are a match.
CrossTable(dates$match,prop.chisq= T)

CrossTable(dates$dec,dates$gender)
pie(table(dates$match),c("Mismatch","Match"),main = "Match Or Not",col = c("red","green"),cex=2,cex.main=2.5)
women<-subset(dates,gender==0)
men<-subset(dates,gender==1)

attributes_men<-t(as.data.frame(lapply(men[,70:75], mean,na.rm=T)))
attributes_women<-t(as.data.frame(lapply(women[,70:75], mean,na.rm=T)))

datescompleteformula<-dates[complete.cases(dates$dec_o,dates$attr_o,dates$sinc_o,dates$intel_o,dates$fun_o,dates$amb_o,dates$shar_o),]
#general what attributes are looking for
formula<-glm(dec_o~attr_o+sinc_o+intel_o+fun_o+amb_o+shar_o,data = dates,family=binomial(logit))
summary(formula)

datescompleteformula$regresion<-formula$fitted.values
Match<-.7
sum((datescompleteformula$regresion>=Match)&(datescompleteformula$dec_o==1))/sum(datescompleteformula$dec_o==1)
mean((datescompleteformula$regresion>=Match)==(datescompleteformula$dec_o==1))



#Formula woman
datescompleteformula<-men[complete.cases(men$dec_o,men$attr_o,men$sinc_o,men$intel_o,men$fun_o,men$amb_o,men$shar_o),]
formula<-glm(dec_o~attr_o+sinc_o+intel_o+fun_o+amb_o+shar_o,data = men,family=binomial(logit))
datescompleteformula$regresion<-formula$fitted.values
Match<-.7
sum((datescompleteformula$regresion>=Match)&(datescompleteformula$dec_o==1))/sum(datescompleteformula$dec_o==1)
mean((datescompleteformula$regresion>=Match)==(datescompleteformula$dec_o==1))

#Formula men
datescompleteformula<-women[complete.cases(women$dec_o,women$attr_o,women$sinc_o,women$intel_o,women$fun_o,women$amb_o,women$shar_o),]
formula<-glm(dec_o~attr_o+sinc_o+intel_o+fun_o+amb_o+shar_o,data = women,family=binomial(logit))
datescompleteformula$regresion<-formula$fitted.values
Match<-.7
sum((datescompleteformula$regresion<=Match)&(datescompleteformula$dec_o==0))/sum(datescompleteformula$dec_o==0)
mean((datescompleteformula$regresion>=Match)==(datescompleteformula$dec_o==1))





#What men are looking for
formula<-glm(dec_o~attr_o+sinc_o+intel_o+fun_o+amb_o+shar_o,data = women,family=binomial(logit))
summary(formula)

#What women are looking for
formula<-glm(dec_o~attr_o+sinc_o+intel_o+fun_o+amb_o+shar_o,data = men,family=binomial(logit))
summary(formula)





formula<-glm(dec_o~int_corr+age+race+round+income+attr+sinc+intel+fun+amb+shar,data = dates,family=binomial(logit))