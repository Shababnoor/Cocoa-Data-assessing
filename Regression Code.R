#Set working directory and open Cocoa.csv file
setwd("D:\\ShababR&Python\\STA-334_July\\Lecture-5")
Cocoa<-read.csv("Cocoa.csv")

#Examining the data
str(Cocoa)
names(Cocoa)

#Recode variable
library(car)
Cocoa$YEAR<-recode(Cocoa$YEAR, "77='1977';78='1978';79='1979';80='1980';81='1981';82='1982';83='1983';84='1984';85='1985';86='1986';87='1987';88='1988';89='1989';90='1990';91='1991';92='1992';93='1993';94='1994'")
Cocoa$MONTH<-recode(Cocoa$MONTH, "1='January'; 9='September'; 10='October'; 11='November'; 12='December'")
Cocoa$flu<-recode(Cocoa$flu, "1='Start';2='Finishing';3='Maximum';4='Over'")
Cocoa$can<-recode(Cocoa$can, "1='Good'; 2='Fairly Good'; 3='Average'; 4='Below Average'; 5='Bad'")
Cocoa$flw<-recode(Cocoa$flw, "1='Pod1'; 2='Pod2'; 3='Pod3'; 4='Pod4'; 5='Pod5';6='Pod6'")

as.factor(Cocoa$flu)
as.factor(Cocoa$can)
as.factor(Cocoa$flw)

#Check normality and constant variance assumption and fit a regression model
Reg<-lm(hv~maxt+mint+midt+pcp+smi+lmi+can+flu+flw+blk+wilt+loss+ty+sml+lrg,data=Cocoa)
par(mfrow=c(2,2))
plot(Reg)

#Create another data frame for performing multicolinearity
Cocoa1 <-Cocoa[,c(4,5,6,7,10,11,12,13,14,15,16,17,18,19)]

#Check multicolinearity
corr(Cocoa1)

#Again check normality and constant variance assumption and fit a regression model
Reg1<-lm(hv~mint+pcp+smi+can+flu+flw+blk+wilt+loss+ty+sml+lrg,data=Cocoa)
par(mfrow=c(2,2))
plot(Reg1)


#Using box-cox transformation
library(MASS)
b=boxcox(hv~mint+pcp+smi+can+flu+flw+blk+wilt+loss+ty+sml+lrg,data=Cocoa)
lambda<-b$x
likelihood<-b$y
bc=cbind(lambda, likelihood)
bc[order(-likelihood),]

#Fit a new regression line with transformed variable
Reg2<-lm((hv^.25)~mint+pcp+smi+can+flu+flw+blk+wilt+loss+ty+sml+lrg,data=Cocoa)
par(mfrow=c(2,2))
plot(Reg2)

#Append transform variable in the dataset
hv<-Cocoa$hv
hvnew<-hv^.25
cbind(Cocoa,hvnew)
shapiro.test(hvnew)

#Detecting Outliers
boxplot(hvnew)

#Backward Elimanation 
fullmodel <- lm((hv^.25)~mint+loss+sml+lrg+pcp+smi+can+flu+flw+blk+wilt+loss+ty,data=na.omit(Cocoa))
step(fullmodel, direction = "backward", trace=FALSE ) 

#Run a model
Reg3<-lm((hv^.25)~mint+sml+lrg+pcp+flw+blk+ty,data=na.omit(Cocoa))
summary(Reg3)
anova(Reg3)

#Run a final model
Reg4<-lm((hv^.25)~lrg+pcp+flw+blk,data=na.omit(Cocoa))
summary(Reg4)
anova(Reg4)


#Checking whether residuals are uncorrelated or not in the final model
library(lmtest)
bgtest(lm((hv^.25)~lrg+pcp+flw+blk,data=na.omit(Cocoa)), order = 1, order.by = NULL, type = c("Chisq", "F"))
#Checking whether heteroscedasticity is present or not in the final model
bptest(lm((hv^.25)~lrg+pcp+flw+blk,data=na.omit(Cocoa)), varformula = NULL, studentize = TRUE)


