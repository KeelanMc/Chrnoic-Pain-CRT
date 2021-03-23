library(Hmisc)
#'hc114_','hc115_','depsoc','depmat','depsev','ac012_'

cpaindev = phbr54[c('mergeid','ph088_',"ph010d1",'initact','initialage','initialyear',
                    'country','gender','pasthstay','Loweducation','cciw_w4',
                    'fdistress','dn010_','ep018_','ep027_','ep028_','ac016_','ac012_'
                    ,initialillvar,futureillvar,variablemixed)]

#Isolating people who developed the pain
cpaindev = cpaindev[cpaindev$ph010d1=='Not selected'&!is.na(cpaindev$ph088_),]
cpaindev = cpaindev[!is.na(cpaindev$cciw_w4),]
population = sum(cpaindev$cciw_w4,na.rm = TRUE)
cpaindev$initialage =as.numeric(levels(cpaindev$initialage)[cpaindev$initialage])
cpaindev$bmi = factor(cpaindev$bmi,levels = c("Normal","Underweight" ,"Overweight","Obese"  ))
cpaindev$parent = cpaindev$nchild!='None'


#Removing na's from waiting list question
#cpaindev$hc115_[cpaindev$hc115_ %in% c("Don't know",'Refusal')]=NA
# cpaindev = cpaindev[!is.na(cpaindev$hc115_),]
# cpaindev$hc115_=droplevels(cpaindev$hc115_)

#Removing na's from isolation question
cpaindev$ac016_[cpaindev$ac016_ %in% c("Don't know",'Refusal')]=NA
cpaindev = cpaindev[!is.na(cpaindev$ac016_),]
cpaindev$ac016_=droplevels(cpaindev$ac016_)
cpaindev$ac016_ = factor(cpaindev$ac016_,levels = c("Never","Rarely","Sometimes","Often"))

cpaindev$Isolation = ifelse(cpaindev$ac016_=='Often',1,0)

#cpaindev$hc115_ = factor(cpaindev$hc115_, levels= c('No','Yes'))
#cpaindev$hc114_ = factor(cpaindev$hc114_, levels= c('No','Yes'))

#Removing na's from life satisfaction question
cpaindev$ac012_[cpaindev$ac012_ %in% c("Don't know",'Refusal')]=NA
cpaindev = cpaindev[!is.na(cpaindev$ac012_),]
cpaindev$ac012_ = droplevels(cpaindev$ac012_)
cpaindev$ac012_ = as.numeric(levels(cpaindev$ac012_)[cpaindev$ac012_])

names(cpaindev)[names(cpaindev) == 'ac012_'] <- 'Life_satisfaction'
names(cpaindev)[names(cpaindev) == 'eurod'] <- 'Depression'
names(cpaindev)[names(cpaindev) == 'ph006d8'] <- 'Arthritis'
names(cpaindev)[names(cpaindev) == 'ph006d12initial'] <- 'Parkinsons'
names(cpaindev)[names(cpaindev) == 'ph006d1initial'] <- 'Heart_Problems'

names(cpaindev)[names(cpaindev) == 'mh007_'] <- 'Sleep_Problems'
names(cpaindev)[names(cpaindev) == 'ep027_'] <- 'PhysicalLabour'
names(cpaindev)[names(cpaindev) == 'sphus'] <- 'Self_reported_health'


cpaindev$PhysicalLabour[cpaindev$PhysicalLabour %in% c("Don't know",'Refusal')]=NA
cpaindev$ep018_[cpaindev$ep018_ %in% c("Don't know",'Refusal')]=NA
cpaindevtest = cpaindev[!is.na(cpaindev$PhysicalLabour),]
cpaindevtest$PhysicalLabour=droplevels(cpaindevtest$PhysicalLabour)
cpaindevtest$PhysicalLabour = factor(cpaindevtest$PhysicalLabour,levels = c("No",levels(cpaindevtest$PhysicalLabour),"Yes"))
cpaindevtest$PhysicalLabour[cpaindevtest$PhysicalLabour %in% c("Agree","Strongly agree")] = 'Yes'
cpaindevtest$PhysicalLabour[cpaindevtest$PhysicalLabour %in% c("Disagree","Strongly disagree")] = 'No'
cpaindevtest$PhysicalLabour=droplevels(cpaindevtest$PhysicalLabour)


cpaindev$fdistress2 = cpaindev$fdistress %in% c('With great difficulty','With some difficulty')






andrewlogit <- glm(ph088_ ~ initialage + country +gender+ Depression+bmi
                   +Loweducation+Self_reported_health+Sleep_Problems+phactiv+Arthritis + Parkinsons+Heart_Problems
                 ,data = cpaindev, family = "binomial")

summary(andrewlogit)
AIC(andrewlogit)
BIC(andrewlogit)
andrewlogit <- glm(ph088_ ~ initialage + country +gender+ Depression+bmi+mobility+parent
                   +Loweducation+Self_reported_health+Sleep_Problems+phactiv+Arthritis + Parkinsons+Heart_Problems
                   ,data = cpaindev, family = "binomial")

Keelanlogit <- glm(ph088_ ~ initialage + country +gender+ Depression+bmi+mobility+parent
                   +Loweducation+Self-reported_health+Sleep_Problems+phactiv+Arthritis + Parkinsons+Heart_Problems+
                     Life_satisfaction+PhysicalLabour+Isolation
                   ,data = cpaindev, family = "binomial")

BIC(Keelanlogit)-BIC(andrewlogit)


#Extracting the coefficients for each country for comparison 
countrycoeff  = as.data.frame(andrewlogit$coefficients[3:14])
rownames(countrycoeff) = sub("country","",rownames(countrycoeff))
countrycoeff['Austria',]= 0

countrycoeff=cbind(Country = rownames(countrycoeff), countrycoeff)
rownames(countrycoeff) <- 1:nrow(countrycoeff)
