library(foreign)
### Loading in datasets 
newdatasets = c("mh5","hh5",'dn5',"gv_isced5",'ex5','gv_health5')
#newdatasets = c('hc5')
for (set in newdatasets){
  filename = paste("sharew",substr(set,nchar(set),nchar(set)),"_rel7-1-0_",
                   substr(set,1,nchar(set)-1),".sav",sep="")
  suppressWarnings(assign(set,read.spss(filename, to.data.frame=TRUE)))
}

weights_5= gv_weights5[c('mergeid','cciw_w5')]
migrant= dn5[c("mergeid","dn004_")]
sleep_trouble = mh5[c('mergeid','mh007_')]
empdesc = ep5[c('mergeid','ep005_','ep016_','ep027_','ep028_','ep301_','ep302_','ep018_')]
vardataset = imputations5[,c('mergeid','sphus','mstat','isced','chronic','mobility','adl','nchild','bmi','eurod','fdistress')]
social = ac5[c(which(colnames(ac5)=='mergeid'),which(colnames(ac5)=='ac012_'):which(colnames(ac5)=='ac038_'))]


#wave5data  =wave5data[wave5data$country!='Germany',]
wave5data= merge(ph5,br5,by='mergeid',suffixes = c('ph','br'), all= TRUE)
wave5data = merge(x=wave5data,y=weights_5,by='mergeid', all= TRUE)
wave5data  = merge(x=wave5data ,y=migrant,by='mergeid',all = TRUE)
wave5data  = merge(x=wave5data , y=mh5,by='mergeid', all=TRUE)
wave5data  = merge(x=wave5data ,y=empdesc,by='mergeid',all = TRUE)
wave5data  = merge(x=wave5data ,y=social,by='mergeid',all = TRUE)
wave5data  = merge(x=wave5data ,y=ex5[c('mergeid','ex026_')],by='mergeid',all = TRUE)
wave5data  = merge(wave5data ,vardataset,by='mergeid',all = TRUE)
wave5data  = merge(wave5data ,cv_r5,by='mergeid',suffixes = c('','cv'),all.x = TRUE)
wave5data  = merge(wave5data ,hc5,by='mergeid',suffixes = c('','hc'),all = TRUE)
wave5data  = merge(wave5data ,gv_health5,by='mergeid',suffixes = c('','gv_h'),all = TRUE)

wave5data$mobility[wave5data$mobility>0&!is.na(wave5data$mobility)]='1 or more'
levels(wave5data$mobility) = c('0','1 or more')

levels(wave5data$ep027_)= c(levels(wave5data$ep027),'Not working')
wave5data$ep027_[!is.na(wave5data$ep005_) & wave5data$ep005_!='Employed or self-employed (including working for family business)'&
                is.na(wave5data$ep027_)] = 'Not working'

wave5data$bmi = ifelse(is.na(wave5data$mobility),as.numeric(NaN),
                    ifelse(wave5data$bmi<18.5,'Underweight',
                           ifelse(wave5data$bmi<25,'Normal',
                                  ifelse(wave5data$bmi<30,'Overweight','Obese'))))

wave5data$bmi = factor(wave5data$bmi,levels = c('Underweight','Normal','Overweight','Obese','NaN'))


wave5data$eurod[wave5data$eurod=='Not depressed']='0'
wave5data$eurod[wave5data$eurod=='Very depressed']='12'
wave5data$eurod = factor(ifelse(wave5data$eurod>=4,'Depression','No Depression'))
wave5data$eurod = droplevels(wave5data$eurod)

wave5data$initialage[wave5data$age_int  %in% c("Don't know","Refusal")]=NA
wave5data=wave5data[!is.na(wave5data$cciw_w5),]
wave5data['age_category']=cut(as.numeric(levels(wave5data$initialage)[wave5data$initialage]),c(50,60,67,75,120),right=FALSE)

wave5data['ncillness'] =cut(as.numeric(levels(wave5data$chronic)[wave5data$chronic]),c(0,1,2,4,20),labels = c(0,1,"2 or 3","more than 3"),right=FALSE)



wave5data[wave5data$sphus %in% c("Excellent","Very good"),"sphus"]= "Very good/excellent"
wave5data[wave5data$sphus %in% c("Poor","Fair","Good"),"sphus"]= "Less than very good"


wave5data$dn004_[wave5data$dn004_ %in% c("Don't know","Refusal")]=NA


wave5data$mstat[wave5data$mstat %in% c('Married, living with spouse','Registered partnership')] = 'Married/registered'
wave5data$mstat[wave5data$mstat %in% c('Married, not living with spouse','Divorced')] = 'Seperated/Divorced'
wave5data$mstat[wave5data$mstat == 'Never married'] = 'Single'

wave5data$mstat = factor(wave5data$mstat,levels = c('Married/registered','Seperated/Divorced','Single','Widowed'))


wave5data$nchild[wave5data$nchild>3]='More than 3'
wave5data$nchild[wave5data$nchild==1]='1'
wave5data$nchild[wave5data$nchild==2|wave5data$nchild==3]='2 or 3'
wave5data$nchild[wave5data$nchild==0]='None'
wave5data$nchild = factor(wave5data$nchild,levels = c('None','1','2 or 3','More than 3'))


wave5data$isced[wave5data$isced  %in% c('None','Isced-97 code 1','Isced-97 code 2')] = "Primary or lower"
wave5data$isced[wave5data$isced %in% c('Isced-97 code 3')] = "Secondary"
wave5data$isced[wave5data$isced %in% c('Isced-97 code 4','Isced-97 code 5','Isced-97 code 6')] = "Post-secondary"

wave5data['Loweducation'] = ifelse(wave5data$isced=='Primary or lower',1,0)
wave5data['Loweducation'] = as.factor(wave5data$Loweducation)
levels(wave5data['Loweducation']) = c(0,1)


wave5data$adl[wave5data$adl>0]='1 or more'
wave5data$adl = as.factor(wave5data$adl)
wave5data$adl = droplevels(wave5data$adl)


levels(wave5data$ep005_) = c(levels(wave5data$ep005_), 'Employed')
wave5data$ep005_[wave5data$ep005_ %in% c("Employed or self-employed (including working for family business)")] = "Employed"
wave5data$ep005_ = factor(wave5data$ep005_,levels = c('Employed','Retired','Unemployed','Permanently sick or disabled',"Homemaker",'Other'))
wave5data$ep005_ = droplevels(wave5data$ep005_)

wave5data$hc114_[wave5data$hc114_ %in% c("Don't know","Refusal")]=NA
wave5data$hc114_ =droplevels(wave5data$hc114_)
wave5data=droplevels(wave5data)
illvar = colnames(wave5data[c(which(colnames(wave5data)=="ph006d1"):
                                   which(colnames(wave5data)=="ph006dot"))])

wave5data = wave5data[complete.cases(wave5data[c('hc014_')]),]
wave5data$hstay = as.numeric(levels(wave5data$hc014_)[wave5data$hc014_])
wave5data$age_int=as.numeric(levels(wave5data$age_int)[wave5data$age_int])
wave5data$ph088_ =factor(wave5data$ph088_,levels=c('No','Yes'))

dplyr::count_(d, vars = c('Non_MSK_pain','Chronic_MSK_Pain',))

names(wave5data)[names(wave5data) == 'ph006d1'] <- 'Heart_trouble'
names(wave5data)[names(wave5data) == 'ph006d2'] <- 'Blood_Pressure'
names(wave5data)[names(wave5data) == 'ph006d3'] <- 'Cholesterol'
names(wave5data)[names(wave5data) == 'ph006d4'] <- 'Stroke'
names(wave5data)[names(wave5data) == 'ph006d5'] <- 'Diabetes'
names(wave5data)[names(wave5data) == 'ph006d10'] <- 'Cancer'
names(wave5data)[names(wave5data) == 'ph006d11'] <- 'Ulcer'
names(wave5data)[names(wave5data) == 'ph006d13'] <- 'Cataracts'
names(wave5data)[names(wave5data) == 'ph006d14'] <- 'Hip_fracture'
names(wave5data)[names(wave5data) == 'ph006d15'] <- 'Other_fracture'
names(wave5data)[names(wave5data) == 'ph006d16'] <- 'Alzeimhers'
names(wave5data)[names(wave5data) == 'ph006d18'] <- 'Psychiatric_problems'
names(wave5data)[names(wave5data) == 'ph006d18'] <- 'Psychiatric_problems'
names(wave5data)[names(wave5data) == 'ph006d19'] <- 'Rheumatoid_Arthritis'
names(wave5data)[names(wave5data) == 'ph006d20'] <- 'Osteoarthritis'
names(wave5data)[names(wave5data) == 'ph006d12'] <- 'Parkinsons'
names(wave5data)[names(wave5data) == 'ph088_'] <- 'Chronic_MSK_Pain'
names(wave5data)[names(wave5data) == 'eurod'] <- 'Depression'

dplyr::count_(wave5data, vars = c('No_Pain','Non_MSK_pain','Chronic_MSK_Pain','Non_chronic_MSK'))

wave5data = wave5data[!is.na(wave5data$Chronic_MSK_Pain),]
wave5data$Non_MSK_pain = (!is.na(wave5data$ph084_) & wave5data$ph084_=='Yes') &
  wave5data$ph087d1=='Not selected'&wave5data$ph087d2=='Not selected'&
  wave5data$ph087d3=='Not selected'&wave5data$ph087d4=='Not selected'
wave5data$Non_chronic_MSK = (!is.na(wave5data$ph084_) & wave5data$ph084_=='Yes') &(
  wave5data$ph087d1=='Selected'|wave5data$ph087d2=='Selected'|
    wave5data$ph087d3=='Selected'|wave5data$ph087d4=='Selected')&
  wave5data$Chronic_MSK_Pain=='No'
wave5data$No_Pain = (!is.na(wave5data$ph084_) & wave5data$ph084_=='No') 


wave5data$Paintype = ifelse(wave5data$Chronic_MSK_Pain=='Yes','Chronic_MSK_Pain',
                            ifelse(wave5data$No_Pain,'A_No_Pain',
                                   ifelse(wave5data$Non_chronic_MSK,'Non_chronic_MSK',
                                          ifelse(wave5data$Non_MSK_pain,'Non_MSK_pain',NA))))
library(pscl)
model4 <- zeroinfl(formula = hstay ~  gender + country +age_int+Paintype
                   | gender + country +age_int+Paintype+
                     Parkinsons+Osteoarthritis+Rheumatoid_Arthritis+Psychiatric_problems+Alzeimhers
                      +Other_fracture+Hip_fracture+Cataracts+Ulcer+Cancer+Diabetes+Stroke+Cholesterol
                      +Blood_Pressure+Heart_trouble+ph006dot+Depression,
                     data = wave5data, dist = "negbin")
summary(model4)
summary(wave5data$No_Pain)
model5 <- zeroinfl(formula = hstay ~  gender + country +age_int+Paintype
                   | gender + country +age_int +Paintype,
                   data = wave5data, dist = "negbin")
summary(model5)

library(plyr)
probhstay_country = ddply(wave5data, .(country), summarise, Sumx1 = sum(hstay!=0),Sumx2 = sum(hstay==0))
probhstay_country$odd = probhstay_country$Sumx1/probhstay_country$Sumx2
