impvar =phbr54[c('mergeid','ph088_',"ph010d1",'initialage','initialyear',
                 'country','gender','pasthstay','futurehstay','Loweducation',
                 initialillvar,futureillvar,variablemixed)]
impvar = merge(impvar,ep5[c('mergeid',"ep005_")],by='mergeid',all.x = TRUE,suffixes = c('','future'))
impvar = merge(impvar,gv_weights4[c('mergeid','cciw_w4')],by='mergeid',suffixes  =c('','_cv'))

impvar = impvar[!is.na(impvar$ep005_),]
impvar = impvar[!is.na(impvar$ph010d1),]
impvar = impvar[!is.na(impvar$ep005_future),]
impvar = impvar[!is.na(impvar$cciw_w4),]


levels(impvar$ep005_future) = c(levels(impvar$ep005_future), 'Employed')
impvar$ep005_future[impvar$ep005_future %in% c("Employed or self-employed (including working for family business)")] = "Employed"
impvar$ep005_future = factor(impvar$ep005_future,levels = c('Employed','Retired','Unemployed','Permanently sick or disabled',"Homemaker",'Other'))
impvar$ep005_future = droplevels(impvar$ep005_future)
impvar$futureemployed = impvar$ep005_future=='Employed'
impvar$futurretired = impvar$ep005_future=='Retired'


impvar['ncillnessfuture'] = rowSums(impvar[c(which(colnames(impvar)=="ph006d1future"):
                                               which(colnames(impvar)=="ph006d20"),
                                             which(colnames(impvar)=="ph006dotfuture"))]=='Selected')
impvar['ncillnessinitial'] = rowSums(impvar[c(which(colnames(impvar)=="ph006d1initial"):
                                                which(colnames(impvar)=="ph006d6initial"),
                                              which(colnames(impvar)=="ph006d8"),
                                              which(colnames(impvar)=="ph006d10initial"):
                                                which(colnames(impvar)=="ph006d16initial"),
                                              which(colnames(impvar)=="ph006dotinitial"))]=='Selected')


illnessfreeallage = impvar[!is.na(impvar$ncillnessfuture)&impvar$ncillnessfuture==0,]
illnessfreeallage = illnessfreeallage[!is.na(illnessfreeallage$ncillnessinitial)&illnessfreeallage$ncillnessinitial==0,]
illnessfreeallage$initialage = droplevels(illnessfreeallage$initialage)
illnessfreeallage$initialage= as.numeric(levels(illnessfreeallage$initialage))[illnessfreeallage$initialage]
originallyworking = illnessfreeallage[illnessfreeallage$ep005_=='Employed',]
illnessfree = originallyworking[originallyworking$initialage<60,]


workingwithpain = illnessfree[illnessfree$ph010d1=='Selected',]
workingpainfree = illnessfree[illnessfree$ph010d1=='Not selected',]


illnessfree$futureemployed = as.numeric(illnessfree$futureemployed)
illnessfree$futureret = as.numeric(illnessfree$futurretired)
illnessfree$futuredisabled = as.numeric(illnessfree$ep005_future=='Permanently sick or disabled')
illnessfree$futureUnemp = as.numeric(illnessfree$ep005_future=='Unemployed')

retlogit <- glm(futureret ~ initialage + country +gender+Loweducation+ph010d1, data = illnessfree, family = "binomial")

summary(retlogit)
sum(workingwithpain$futureemployed==FALSE)

pretend = workingwithpain
pretend$ph010d1 = 'Not selected'

workingwithpain['retprobpain'] = predict(retlogit,workingwithpain,type='response')-predict(retlogit,pretend,type='response')
popaddearlyret = sum(workingwithpain$retprobpain*workingwithpain$cciw_w4)
sampleearlyret = sum(workingwithpain$retprobpain)

originallyworking$country = droplevels(originallyworking$country)
countrylist = levels(originallyworking$country)

retdf = data.frame(Country =character(),N_in_sample= integer(),
                   Sample_additional_early_retirements=double(),
                   Total_additional_early_retirements= double())


i=1
for (country in countrylist){
  countryretdata = workingwithpain[workingwithpain$country==country,]
  retdf[i,1] = country
  retdf[i,2] = nrow(countryretdata)
  retdf[i,3] = sum(countryretdata$retprobpain)
  retdf[i,4] = sum(countryretdata$retprobpain*countryretdata$cciw_w4)
  i =i+1
}
Population = read.csv('Population.csv',header = TRUE)

retdf = merge(retdf,Population,by='Country')


poptotal = impvar
poptotal$age = droplevels(poptotal$initialage)
poptotal$age =as.numeric(levels(poptotal$age)[poptotal$age])



allret = poptotal[!is.na(poptotal$ep005_future)& poptotal$ep005_future=='Retired' &!is.na(poptotal$ep005_)&poptotal$ep005_=='Employed',] 

poptotal = poptotal[poptotal$age<60,]

earlyret = poptotal[!is.na(poptotal$ep005_future)& poptotal$ep005_future=='Retired' &!is.na(poptotal$ep005_)&poptotal$ep005_=='Employed',] 

samptotal = poptotal[!is.na(poptotal$ph010d1)&poptotal$ph010d1=='Selected'&
                       !is.na(poptotal$ep005_)&poptotal$ep005_=='Employed'&
                       !is.na(poptotal$ncillnessinitial)&poptotal$ncillnessinitial==0&
                       !is.na(poptotal$ncillnessfuture)&poptotal$ncillnessfuture==0,]
samprettotal = samptotal[!is.na(samptotal$futureemployed)&samptotal$ep005_future=='Retired',]
poptotalc = aggregate(cciw_w4 ~ country, data=poptotal, sum)
poptotalsum = sum(poptotal$cciw_w4)
samptotalc = aggregate(cciw_w4 ~ country, data=samptotal, sum)
samptotalsum = sum(samptotal$cciw_w4)
allretc = aggregate(cciw_w4 ~ country, data=allret, sum)
allretsum = sum(allret$cciw_w4)
samprettotalc =  aggregate(cciw_w4 ~ country, data=samprettotal, sum)
samprettotalsum = sum(samprettotal$cciw_w4)
earlyretc = aggregate(cciw_w4 ~ country, data=earlyret, sum)
earlyretsum = sum(earlyret$cciw_w4)

sum(retdf$Sample_additional_early_retirements)
retdf = merge(retdf,poptotalc,by.x='Country',by.y='country')
retdf = merge(retdf,samptotalc,by.x='Country',by.y='country',suffixes = c('_pop','_sam'))
retdf = merge(retdf,allretc,by.x='Country',by.y='country',suffixes = c('','_ret'))
retdf = merge(retdf,samprettotalc,by.x='Country',by.y='country',all.x =TRUE,suffixes = c('','_sampret'))
retdf = merge(retdf,earlyretc,by.x='Country',by.y='country',suffixes = c('','_earlyret'))



retdf['propofallret']=retdf$Total_additional_early_retirements/retdf$cciw_w4
retdf['propofsampret']=retdf$Total_additional_early_retirements/retdf$cciw_w4_sampret
retdf['propofearret']=retdf$Total_additional_early_retirements/retdf$cciw_w4_earlyret

popaddearlyret/samprettotalsum
popaddearlyret/earlyretsum
popaddearlyret/allretsum
retdf
earlyretsum/allretsum



graphvalues =  poptotal[!is.na(poptotal$ep005_future),]
graphvalues = graphvalues[!is.na(graphvalues$ncillnessinitial)&graphvalues$ncillnessinitial==0,]
#graphvalues = graphvalues[!is.na(graphvalues$ncillnessfuture)&graphvalues$ncillnessfuture==0,]



agedf = data.frame(Age =character(),N_at_age= integer(),Retirement_level=double(),
                   Pain_level = character())
graphvalues$initialage = droplevels(graphvalues$initialage)
splitdata = split(graphvalues,graphvalues$initialage)

i=1
for (levl in levels(graphvalues$initialage)){
  csdata = splitdata[[levl]]
  mean = mean(as.numeric(csdata$ep005_=='Retired'))*100
  agedf[i,1]=levl
  agedf[i,2]=nrow(csdata)
  agedf[i,3]=mean
  agedf[i,4]= 'Total'
  for (painlvl in levels(csdata$ph010d1)){
    i=i+1
    edudata= split(csdata,csdata$ph010d1)[[painlvl]]
    edumean = mean(as.numeric(edudata$ep005_=='Retired'))*100
    agedf[i,1]=levl
    agedf[i,2]= nrow(edudata)
    agedf[i,3]= edumean
    agedf[i,4]= painlvl
  }
  i=i+1
}


test = impvar[impvar$initialage==60&impvar$ncillnessinitial==0&!is.na(impvar$ph010d1)&impvar$ph010d1=='Not selected',]
nrow(test[test$ep005_=='Retired',])/nrow(test)


agedf = agedf[agedf$Pain_level%in% c('Selected','Not selected'),]
agedf$Pain_level =factor(agedf$Pain_level,levels=c('Selected','Not selected'))

colnames(agedf) = c('Age','Number of Observations','Retired %','Chronic Pain')

ggplot()+
  geom_point(agedf[agedf$`Chronic Pain`=='Not selected',], mapping = aes(Age,`Retired %`,group=`Chronic Pain`,col=`Chronic Pain`,size= `Number of Observations`))+
  geom_point(agedf[agedf$`Chronic Pain`=='Selected',], mapping = aes(Age,`Retired %`,group=`Chronic Pain`,col=`Chronic Pain`,size= `Number of Observations`))







agedf = data.frame(Age =character(),N_at_age= integer(),Retirement_level=double(),
                   Pain_level = character())
graphvalues = graphvalues[!is.na(graphvalues$ep005_future),]
i=1
for (levl in levels(graphvalues$initialage)){
  csdata = splitdata[[levl]]
  mean = mean(as.numeric(csdata$ep005_future=='Retired'))*100
  agedf[i,1]=levl
  agedf[i,2]=nrow(csdata)
  agedf[i,3]=mean
  agedf[i,4]= 'Total'
  for (painlvl in levels(csdata$ph088_)){
    i=i+1
    edudata= split(csdata,csdata$ph088_)[[painlvl]]
    edumean = mean(as.numeric(edudata$ep005_future=='Retired'))*100
    agedf[i,1]=levl
    agedf[i,2]= nrow(edudata)
    agedf[i,3]= edumean
    agedf[i,4]= painlvl
  }
  i=i+1
}

agedf=agedf[1:30,]
agedf = agedf[agedf$Pain_level%in% c('Yes','No'),]
agedf$Pain_level =factor(agedf$Pain_level,levels=c('Yes','No'))

colnames(agedf) = c('Age','Number of Observations','Retired %','Chronic Pain')

ggplot()+
  geom_point(agedf[agedf$`Chronic Pain`=='No',], mapping = aes(Age,`Retired %`,group=`Chronic Pain`,col=`Chronic Pain`,size= `Number of Observations`))+
  geom_point(agedf[agedf$`Chronic Pain`=='Yes',], mapping = aes(Age,`Retired %`,group=`Chronic Pain`,col=`Chronic Pain`,size= `Number of Observations`))



phep5 = merge(ph5,ep5,by='mergeid')
phep5 = phep5[!is.na(phep5$ph088_)&!is.na(phep5$ep005_),]
phep5 = merge(phep5,imputations5,by='mergeid')
phep5 = merge(phep5,gv_weights5,by='mergeid',suffixes = c('','weights'))
phep5 = phep5[!is.na(phep5$cciw_w5),]
#phep5=phep5[phep5$chronic==0,]

phep5$age = droplevels(phep5$age)
phep5$age = as.numeric(levels(phep5$age)[phep5$age])
phep5 = phep5[phep5$age<62&phep5$age>49,]
phep5$age = factor(phep5$age)
splitdata = split(phep5,phep5$age)
agedf = data.frame(Age =character(),N_at_age= integer(),Retirement_level=double(),
                   Pain_level = character(),LB95 = double(),UB95 = double())

i=1
for (levl in levels(phep5$age)){
  csdata = splitdata[[levl]]
  mean = weighted.mean(as.numeric(csdata$ep005_=='Retired'),csdata$cciw_w5)
  sd = sqrt(mean*(1-mean)*sum((csdata$cciw_w5/sum(csdata$cciw_w5))^2))
  agedf[i,1]=levl
  agedf[i,2]=nrow(csdata)
  agedf[i,3]=mean*100
  agedf[i,4]= 'Total'
  agedf[i,5]= round(mean-qnorm(0.025)*sd,3)*100
  agedf[i,6]= round(mean+qnorm(0.025)*sd,3)*100
  for (painlvl in levels(csdata$ph088_)){
    i=i+1
    edudata= split(csdata,csdata$ph088_)[[painlvl]]
    edumean = weighted.mean(as.numeric(edudata$ep005_=='Retired'),edudata$cciw_w5)
    edusd = sqrt(edumean*(1-edumean)*sum((edudata$cciw_w5/sum(edudata$cciw_w5))^2))
    agedf[i,1]=levl
    agedf[i,2]= nrow(edudata)
    agedf[i,3]= edumean*100
    agedf[i,4]= painlvl
    agedf[i,5]= round(edumean-qnorm(0.025)*edusd,3)*100
    agedf[i,6]= round(edumean+qnorm(0.025)*edusd,3)*100
  }
  i=i+1
}



agedf = agedf[agedf$Pain_level%in% c('Yes','No'),]
agedf$Pain_level =factor(agedf$Pain_level,levels=c('Yes','No'))

colnames(agedf) = c('Age','Number of Observations','Retired %','Chronic Pain','LBCI','UBCI')

ggplot()+
  geom_point(agedf[agedf$`Chronic Pain`=='No',], mapping = aes(Age,`Retired %`,group=`Chronic Pain`,col=`Chronic Pain`,size= `Number of Observations`))+
  geom_errorbar(aes(ymin=Lower_bound_95CI,ymax=Upper_bound_95CI,color='red',width=50))
geom_point(agedf[agedf$`Chronic Pain`=='Yes',], mapping = aes(Age,`Retired %`,group=`Chronic Pain`,col=`Chronic Pain`,size= `Number of Observations`))+
  
  
  genderedmean = weighted.mean(as.numeric(edudata$ep005_=='Retired'),edudata$cciw_w5)
sqrt(gendermean*(1-gendermean)*sum((edudata$cciw_w5/sum(edudata$cciw_w5))^2))
