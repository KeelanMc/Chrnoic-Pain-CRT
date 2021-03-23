####################################################################################################
###
### File:    PainDevdata.R
### Purpose: Load and structure data required for analysis
### Authors: Keelan McMahon
### Date:    6/12/20
###
####################################################################################################
library(foreign)
### Loading in datasets 
datasets = c("ph5","ph4","br5","br4","cv_r4","cv_r5","mh4","ac4",
             "hc4","hc5","xt5","xt6","xt7","dn7","gv_weights4","mh4","dn4",
             "gv_isced4","gv_isced1","gv_isced2","gv_health4","dn1","dn2","ep4","ep5","technical_variables2",
             "gv_weights5","gv_deprivation5","ac4","ac5","ex4","hh4")
if (file.exists('gv_imputations5_first.csv')){
  imputations4 = read.csv('gv_imputations4_first.csv')
  imputations5 = read.csv('gv_imputations5_first.csv')
} else{
  datasets = c(datasets,"gv_imputations4","gv_imputations5")
}
for (set in datasets){
  filename = paste("sharew",substr(set,nchar(set),nchar(set)),"_rel7-1-0_",
                   substr(set,1,nchar(set)-1),".sav",sep="")
  assign(set,read.spss(filename, to.data.frame=TRUE))
}
if (file.exists('gv_imputations5_first.csv') ==FALSE){
  imputations5 = gv_imputations5[gv_imputations5$implicat==1,]
  write.csv(imputations5,'gv_imputations5_first.csv')
  imputations4 = gv_imputations4[gv_imputations4$implicat==1,]
  write.csv(imputations4,'gv_imputations4_first.csv')
}


#isolating variables of interest
individual_weights= gv_weights4[c('mergeid','cciw_w4')]
individual_demographics = dn4[c("mergeid","dn004_")]
sleep = mh4[c('mergeid','mh007_')]
empdesc = ep4[c('mergeid','ep005_','ep016_','ep027_','ep028_','ep301_','ep302_','ep018_')]
social = ac4[c('mergeid',colnames(ac4[,20:28]),'ac016_')]


#Extracting education values based on previous studies values of isced1997_r from previous baseline interviews
elvl = gv_isced4[c('mergeid','isced1997_r')]
eduw1r = gv_isced1[c('mergeid','isced1997_r')]
names(eduw1r)[2] = 'isced1997_r_w1'

eduw2r =gv_isced2[c('mergeid','isced1997_r')]
names(eduw2r)[2] = 'isced1997_r_w2'
tecvar2= technical_variables2[c("mergeid","mn101_")]
wave2merged = merge(eduw2r,tecvar2,by = 'mergeid',all.x = TRUE)
wave2fin = wave2merged[wave2merged$mn101_ == 'Baseline questionnaire', c('mergeid','isced1997_r_w2')]

elvl14 = merge(elvl,eduw1r,by = 'mergeid',all.x=TRUE)
elvl124 = merge(elvl14,wave2fin,by = 'mergeid',all.x=TRUE)

elvl124[is.na(elvl124$isced1997_r)|elvl124$isced1997_r %in% c("Don't know","Refusal","Other","Still in school"),'isced1997_r']= 
  elvl124[is.na(elvl124$isced1997_r)|elvl124$isced1997_r %in% c("Don't know","Refusal","Other","Still in school"),'isced1997_r_w2']

elvl124[is.na(elvl124$isced1997_r)|elvl124$isced1997_r %in% c("Don't know","Refusal","Other","Still in school"),'isced1997_r']= 
  elvl124[is.na(elvl124$isced1997_r)|elvl124$isced1997_r %in% c("Don't know","Refusal","Other","Still in school"),'isced1997_r_w1']

edufin = elvl124[c('mergeid','isced1997_r')]

wave1fin = dn1[c("mergeid","dn004_")]
names(wave1fin)[2] = 'dn004_w1'

wave2fin= dn2[c("mergeid","dn004_")]
names(wave2fin)[2] = 'dn004_w2'

individual_demographics = dn4[c("mergeid","dn004_","dn010_")]
indem42 = merge(individual_demographics,wave2fin,by='mergeid',all.x = TRUE)
indem421 = merge(indem42,wave1fin,by='mergeid',all.x=TRUE)

indem421[is.na(indem421$dn004_)|indem421$dn004_ %in% c("Don't know","Refusal","Not answered"),'dn004_']= 
  indem421[is.na(indem421$dn004_)|indem421$dn004_ %in% c("Don't know","Refusal","Not answered"),'dn004_w2']

indem421[is.na(indem421$dn004_)|indem421$dn004_ %in% c("Don't know","Refusal","Not answered"),'dn004_']= 
  indem421[is.na(indem421$dn004_)|indem421$dn004_ %in% c("Don't know","Refusal","Not answered"),'dn004_w1']
individual_demographics = indem421[c("mergeid","dn004_","dn010_")]

#cleaning datasets treating refusal and unsure as NA
healthcaresets = list(hc4 =hc4,hc5=hc5)
healthcaresets = lapply(healthcaresets,
                        function(x){
                          levels(x$hc014_) =union("0",levels(x$hc014_))
                          x$hc014_[x$hc012_=='No']=0
                          x$hc014_[x$hc014_ %in% c("Don't know","Refusal")]=NA
                          x=droplevels(x)
                          return(x)
                        })
hc4 = healthcaresets$hc4
hc5 = healthcaresets$hc5

#Cleaning ph dataset, filling in incorrect na values, based on entries in other columns
phdatasets = list(ph5=ph5)
phdatasets = lapply(phdatasets,
                    function(x){
                      levels(x$ph085_) = c(levels(x$ph085_), 'No pain')
                      x$ph085_[x$ph084_=='No']='No pain'
                      x[['npainloc']]= rowSums(x[c(which(colnames(x)=="ph087d1"):
                                                     which(colnames(x)=="ph087d6"))]=='Selected')
                      x[['painsites']] = ifelse(x$ph085_=='No pain','None',
                                                ifelse(x$npainloc>1|x$ph087d7=='Selected','Multi',
                                                       ifelse(x$npainloc==1&x$ph087d7=='Not selected','Single',NA)))
                      
                      Partofbodypain = c("ph087d1","ph087d2","ph087d3","ph087d4","ph087d5","ph087d6","ph087d7")
                      for (part in Partofbodypain){
                        x[[part]][x$ph084_=='No']='Not selected'
                      }
                      x$ph088_[x$ph084_=='No']='No'
                      
                      x$ph088_[is.na(x$ph088_)&
                                 x$ph087d1=='Not selected'&x$ph087d2=='Not selected'&
                                 x$ph087d3=='Not selected'&x$ph087d4=='Not selected']='No'
                      x$ph088_[x$ph088_ %in% c("Don't know","Not answered","Refusal")]=NA
                      
                      illnessvar = colnames(x[c(which(colnames(x)=="ph006d1"):
                                                  which(colnames(x)=="ph006d20"),
                                                which(colnames(x)=="ph006dot"),
                                                which(colnames(x)=="ph085_"),
                                                which(colnames(x)=="ph088_"),
                                                which(colnames(x)=="ph003_")
                                                )])
                      for (illness in  illnessvar){
                        x[[illness]][x[[illness]] %in% c("Don't know","Not answered","Refusal")]=NA
                        x[[illness]]=droplevels(factor(x[[illness]]))
                      }
                      levels(x$ph003_) = c(levels(x$ph003_),0,1)
                      x$ph003_ = ifelse(x$ph003_=='Poor',1,0)
                      x$ph003_ = factor(x$ph003_)
                      x['weight'] = as.numeric(levels(x$ph012_))[x$ph012_]
                      x['height'] =  as.numeric(levels(x$ph013_))[x$ph013_]
                      x$weight[x['weight']<20]=NA
                      x$height[x['height']<20]=NA
                      x['mybmi'] = 10000*x$weight/(x$height)^2
                      x['Obese'] = ifelse(x$mybmi<30,0,1)
                      x['Obese'] = factor(x$Obese)
                      levels(x$Obese) = c(0,1)
                      x = droplevels(x)
                      
                      x$ph085_ =factor(x$ph085_,levels = c('No pain','Mild','Moderate','Severe'))
                      return(x)
                    })
ph5 = phdatasets$ph5


#Filling in na for all missing data responses
illnessvar = colnames(ph4[c(which(colnames(ph4)=="ph006d1"):
                            which(colnames(ph4)=="ph006d17"),
                          which(colnames(ph4)=="ph006dot"),
                          which(colnames(ph4)=="ph010d1"))])
for (illness in  illnessvar){
  ph4[[illness]][ph4[[illness]] %in% c("Don't know","Not answered","Refusal")]=NA
  ph4[[illness]]=droplevels(factor(ph4[[illness]]))
}

#Creating BMI variable
ph4['weight'] = as.numeric(levels(ph4$ph012_))[ph4$ph012_]
ph4['height'] =  as.numeric(levels(ph4$ph013_))[ph4$ph013_]
ph4$weight[ph4['weight']<20]=NA
ph4$height[ph4['height']<20]=NA
ph4['mybmi'] = 10000*ph4$weight/(ph4$height)^2
ph4['Obese'] = ifelse(ph4$mybmi<30,0,1)
ph4['Obese'] = factor(ph4$Obese)
levels(ph4$Obese) = c(0,1)

#creating a binary variable for poor self-reported health
ph4$ph003_[ph4$ph003_ %in% c("Don't know","Refusal")] = NA
levels(ph4$ph003_) = c(levels(ph4$ph003_),0,1)
ph4$ph003_ = ifelse(ph4$ph003_=='Poor',1,0)
ph4$ph003_ = factor(ph4$ph003_)
ph4 = droplevels(ph4)



#creating a binary variable for self-reported sleeping trouble
mh4$mh007_[mh4$mh007_ %in% c("Don't know","Refusal")] = NA
levels(mh4$mh007_) = c(levels(mh4$mh007_),0,1)
mh4$mh007_ = ifelse(mh4$mh007_=='Trouble with sleep or recent change in pattern',1,0)
mh4$mh007_ = factor(mh4$mh007_)
mh4 = droplevels(mh4)

#creating a binary variable for self-reported activity levels
gv_health4$phactiv[gv_health4$phactiv %in% c("Don't know","Refusal")] = NA
levels(gv_health4$phactiv) = c(levels(gv_health4$phactiv),0,1)
gv_health4$phactiv[gv_health4$phactiv =='Other' ] = 0
gv_health4$phactiv[gv_health4$phactiv =='Never vigorous nor moderate physical activity' ] = 1

#filling and adding nas to br datasets
brdatasets = list(br4=br4,br5=br5)
brdatasets = lapply(brdatasets,
                    function(x){
                      x$br023_[x$br021_=='No']='Not at all in the last 3 months'
                      x$br023_[x$br010_=="Not at all in the last 3 months"] = 'Not at all in the last 3 months'
                      x$br002_[x$br001_=='No']='No'
                      x$br002_[x$br002_ %in% c("Don't know","Refusal")] = NA
                      levels(x$br002_) = c(levels(x$br002_),0,1)
                      x$br002_[x$br002_ =='No' ] = 0
                      x$br002_[x$br002_ =='Yes' ] = 1
                      x$br023_[x$br023_ %in% c("Don't know","Refusal")] = NA
                      levels(x$br023_) = c(levels(x$br023_),0,1)
                      x$br023_ = ifelse(x$br023_=='Not at all in the last 3 months',0,1)
                      x$br023_ = factor(x$br023_)
                      x = droplevels(x)
                      x$br015_[x$br015_ %in% c("Don't know","Not answered","Refusal")]=NA
                      x$br015_ = droplevels(x$br015_)
                      return(x)
                    })
br4 = brdatasets$br4
br5 = brdatasets$br5

##Merging data from two waves and removing participants not in both

ph54 = merge(ph5,ph4,by='mergeid',suffixes = c('future','initial'),all=TRUE)
chronicpain= ph54[c('mergeid','ph010d1','ph088_')]
chronicpain = chronicpain[complete.cases(chronicpain),]
levels(chronicpain$ph010d1)=c(levels(chronicpain$ph010d1),'Yes','No')
chronicpain$ph010d1[chronicpain$ph010d1=='Selected']='Yes'
chronicpain$ph010d1[chronicpain$ph010d1=='Not selected']='No'
chronicpain$ph010d1 = droplevels(chronicpain$ph010d1)

#Using imputed values
vardataset = imputations4[,c('mergeid','sphus','mstat','isced','chronic','mobility','adl','nchild','bmi','eurod','fdistress')]
ac4small = ac4[c(which(colnames(ac4)=='mergeid'),which(colnames(ac4)=="ac012_"):which(colnames(ac4)=="ac038_"))]

#Merginng dataets containing variables of interet
phbr4 = merge(ph4,br4,by='mergeid',suffixes = c('ph','br'), all= TRUE)
phbr4 = merge(x=phbr4,y=individual_weights,by='mergeid', all= TRUE)
phbr4 = merge(x=phbr4,y=individual_demographics,by='mergeid',all = TRUE)
phbr4 = merge(x=phbr4,y=edufin,by='mergeid',all = TRUE)
phbr4 = merge(x=phbr4, y=mh4,by='mergeid', all=TRUE)
phbr4 = merge(x=phbr4,y=empdesc,by='mergeid',all = TRUE)
phbr4 = merge(x=phbr4,y=ac4small,by='mergeid',all = TRUE)
phbr4 = merge(x=phbr4,y=ex4[c('mergeid','ex026_')],by='mergeid',all = TRUE)
phbr4 = merge(phbr4,vardataset,by='mergeid',all = TRUE)
phbr4 = merge(phbr4,cv_r4,by='mergeid',suffixes = c('','cv'),all.x = TRUE)
phbr4 = merge(phbr4,hc4,by='mergeid',suffixes = c('','hc'),all = TRUE)
phbr4 = merge(phbr4,gv_health4,by='mergeid',suffixes = c('','gv_h'),all = TRUE)
phbr4 = merge(phbr4,hc5,by='mergeid',suffixes = c('initial','future'),all = TRUE)
phbr54 = merge(phbr4,ph5,by='mergeid',suffixes = c('initial','future'),all = TRUE)
phbr54 = merge(phbr54,gv_weights5[c('mergeid','cciw_w5')],by='mergeid',all=TRUE)
phbr54['initact'] = phbr54$br015_
phbr54['initialage'] =phbr54$age_int
phbr54['initialyear'] = 2011
phbr54['country']=phbr54$countrycv
phbr54['pasthstay'] = phbr54$hc014_initial
phbr54['futurehstay'] = phbr54$hc014_future


#Creating variables used in Cimas et al

phbr54$mobility[phbr54$mobility>0&!is.na(phbr54$mobility)]='1 or more'
levels(phbr54$mobility) = c('0','1 or more')

levels(phbr54$ep027_)= c(levels(phbr54$ep027),'Not working')
phbr54$ep027_[!is.na(phbr54$ep005_) & phbr54$ep005_!='Employed or self-employed (including working for family business)'&
             is.na(phbr54$ep027_)] = 'Not working'


phbr54$bmi = ifelse(is.na(phbr54$mobility),as.numeric(NaN),
                    ifelse(phbr54$bmi<18.5,'Underweight',
                  ifelse(phbr54$bmi<25,'Normal',
                         ifelse(phbr54$bmi<30,'Overweight','Obese'))))
phbr54$bmi = factor(phbr54$bmi,levels = c('Underweight','Normal','Overweight','Obese','NaN'))

# Creating depression variable based on Castro-Costa et al 2007

phbr54$eurod[phbr54$eurod=='Not depressed']='0'
phbr54$eurod[phbr54$eurod=='Very depressed']='12'
phbr54$eurod = factor(ifelse(phbr54$eurod>=4,'Depression','No Depression'))
phbr54$eurod = droplevels(phbr54$eurod)

phbr54$initialage[phbr54$initialage  %in% c("Don't know","Refusal")]=NA
phbr54=phbr54[!is.na(phbr54$cciw_w4),]
phbr54['age_category']=cut(as.numeric(levels(phbr54$initialage)[phbr54$initialage]),c(50,60,67,75,120),right=FALSE)

phbr54['ncillness'] =cut(as.numeric(levels(phbr54$chronic)[phbr54$chronic]),c(0,1,2,4,20),labels = c(0,1,"2 or 3","more than 3"),right=FALSE)



phbr54[phbr54$sphus %in% c("Excellent","Very good"),"sphus"]= "Very good/excellent"
phbr54[phbr54$sphus %in% c("Poor","Fair","Good"),"sphus"]= "Less than very good"


phbr54$dn004_[phbr54$dn004_ %in% c("Don't know","Refusal")]=NA


phbr54$mstat[phbr54$mstat %in% c('Married, living with spouse','Registered partnership')] = 'Married/registered'
phbr54$mstat[phbr54$mstat %in% c('Married, not living with spouse','Divorced')] = 'Seperated/Divorced'
phbr54$mstat[phbr54$mstat == 'Never married'] = 'Single'

phbr54$mstat = factor(phbr54$mstat,levels = c('Married/registered','Seperated/Divorced','Single','Widowed'))


phbr54$nchild[phbr54$nchild>3]='More than 3'
phbr54$nchild[phbr54$nchild==1]='1'
phbr54$nchild[phbr54$nchild==2|phbr54$nchild==3]='2 or 3'
phbr54$nchild[phbr54$nchild==0]='None'
phbr54$nchild = factor(phbr54$nchild,levels = c('None','1','2 or 3','More than 3'))


phbr54$isced[phbr54$isced  %in% c('None','Isced-97 code 1','Isced-97 code 2')] = "Primary or lower"
phbr54$isced[phbr54$isced %in% c('Isced-97 code 3')] = "Secondary"
phbr54$isced[phbr54$isced %in% c('Isced-97 code 4','Isced-97 code 5','Isced-97 code 6')] = "Post-secondary"

phbr54['Loweducation'] = ifelse(phbr54$isced=='Primary or lower',1,0)
phbr54['Loweducation'] = as.factor(phbr54$Loweducation)
levels(phbr54['Loweducation']) = c(0,1)


phbr54$adl[phbr54$adl>0]='1 or more'
phbr54$adl = as.factor(phbr54$adl)
phbr54$adl = droplevels(phbr54$adl)


levels(phbr54$ep005_) = c(levels(phbr54$ep005_), 'Employed')
phbr54$ep005_[phbr54$ep005_ %in% c("Employed or self-employed (including working for family business)")] = "Employed"
phbr54$ep005_ = factor(phbr54$ep005_,levels = c('Employed','Retired','Unemployed','Permanently sick or disabled',"Homemaker",'Other'))
phbr54$ep005_ = droplevels(phbr54$ep005_)

phbr54$hc114_[phbr54$hc114_ %in% c("Don't know","Refusal")]=NA
phbr54$hc114_ =droplevels(phbr54$hc114_)
phbr54=droplevels(phbr54)

initialillvar = colnames(phbr54[c(which(colnames(phbr54)=="ph006d1initial"):
                            which(colnames(phbr54)=="ph006dotinitial"))])
futureillvar = colnames(phbr54[c(which(colnames(phbr54)=="ph006d1future"):
                                     which(colnames(phbr54)=="ph006dotfuture"))])

variablemixed = c('age_category','ncillness','mobility',
                  'sphus','eurod','mh007_',
                  'bmi','country','dn004_','mstat',
                  'nchild','isced','ep005_','ep028_',
                  'adl','cciw_w4','br002_','br023_','phactiv',
                  'Obeseinitial','ex026_','ep027_')

#cobining chronic illnesses with significant ovelap
phbr54$fracturefuture = ifelse(phbr54$ph006d14future=='Selected'|phbr54$ph006d15future=='Selected','Selected','Not selected')
phbr54$alzparkfuture = ifelse(phbr54$ph006d16future=='Selected'|phbr54$ph006d12future=='Selected','Selected','Not selected')
futureillvar = c(futureillvar,'fracturefuture','alzparkfuture')

cpaindev = phbr54[c('mergeid','ph088_',"ph010d1",'initact','initialage','initialyear',
                  'country','gender','pasthstay','futurehstay','Loweducation','cciw_w4',
                  'hc114_','hc115_',initialillvar,futureillvar,variablemixed)]

var =c('mergeid','ph088_',"ph010d1",'initact','initialage','initialyear',
  'country','gender','pasthstay','Loweducation','cciw_w4',
  initialillvar,futureillvar,variablemixed)
variablemixed[!(variablemixed %in% colnames(phbr54))]





