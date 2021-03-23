library(dplyr)
library(tidyr)
library(forestplot)
library(pdftools)
library(staplr)

Paindev = data.frame(Variable=character(),category =character(),NTotal = integer(),Chronic_Pain_Development_rate=double(),Lower_bound_95CI=double(),
                     Upper_bound_95CI=double(), NMale = integer(),Male_CPD= double(), Lower_bound_95CI=double(),
                     Upper_bound_95CI=double(),NFemale =integer(),Female_CPD=double(), Lower_bound_95CI=double(),
                     Upper_bound_95CI=double())


variablemixed = c('age_category','ncillness','mobility',
                  'sphus','eurod','mh007_',
                  'bmi','country','dn004_','mstat',
                  'nchild','isced','ep005_',
                  'adl','initact','br002_','br023_','phactiv',
                  'Obeseinitial','hc114_')
cpaindev = phbr54[c('mergeid','ph088_',"ph010d1",'initact','initialage','initialyear',
                    'country','gender','pasthstay','futurehstay','Loweducation','cciw_w4',
                    initialillvar,futureillvar,variablemixed)]


cpaindev  =cpaindev[!is.na(cpaindev$cciw_w4),]
cpaindev = cpaindev[cpaindev$ph010d1=='Not selected'&!is.na(cpaindev$ph088_),]
cpaindev = droplevels(cpaindev)
i=1
for(svar in c("total",variablemixed)){
  for (levl in levels(cpaindev[[svar]])){
    csdata = split(cpaindev,cpaindev[[svar]])[[levl]]
    Paindev[i,1] = svar
    Paindev[i,2] = levl
    Paindev[i,3] = nrow(csdata)
    mean = weighted.mean(as.numeric(csdata$ph088_=='Yes'),csdata$cciw_w4)
    sd = sqrt(mean*(1-mean)*sum((csdata$cciw_w4/sum(csdata$cciw_w4))^2))
    Paindev[i,4]= round(mean,3)*100
    Paindev[i,5]= round(mean+qnorm(0.025)*sd,3)*100
    Paindev[i,6]= round(mean+qnorm(0.975)*sd,3)*100
    j=1
    for (gender in levels(csdata$gender)){
      genderdata= split(csdata,csdata$gender)[[gender]]
      gendermean = weighted.mean(as.numeric(genderdata$ph088_=='Yes'),genderdata$cciw_w4)
      gendersd = sqrt(gendermean*(1-gendermean)*sum((genderdata$cciw_w4/sum(genderdata$cciw_w4))^2))
      Paindev[i,(6+j)]= nrow(genderdata)
      Paindev[i,(7+j)]= round(gendermean,3)*100
      Paindev[i,(8+j)]= round(gendermean+qnorm(0.025)*gendersd,3)*100
      Paindev[i,(9+j)]= round(gendermean+qnorm(0.975)*gendersd,3)*100
      j=j+4
    }
    i= i+1
  }
}

Paindev[,4:14][Paindev[,4:14]<0]=0




variabledesc = list("age_category"="Age Category","ncillness"="Number of Chronic Illnesses","mobility"="Mobility Limitations",
                    "sphus"="Self Reported Health Status","eurod"="Depression","mh007_"="Sleeping Status","bmi"="Bmi","country"="Country","dn004_"="Born in Country of interview",
                    "mstat"="Marital Status","nchild"="Number of Children","isced"="Education Level",
                    "ep005_"="Employment Status","adl"="Limitations with Daily Activities","initact"="Activity Level",
                    "Obeseinitial"="Obese","phactiv"="Inactive","br002_"="Smoker","br023_"="binge_drinker")


for (variable in unique(Paindev$Variable)){
  pdf(paste(variable,'.pdf',sep=''))
  labeltext=c("N_obs", "Chronic_Pain_Prevalence", "Lower_bound_95CI", "Upper_bound_95CI")
  variabledf = Paindev[Paindev$Variable==variable,]
  mean=data.matrix(variabledf[c("Chronic_Pain_Development_rate","Male_CPD","Female_CPD")])
  lower=data.matrix(variabledf[c('Lower_bound_95CI',"Lower_bound_95CI.1" ,"Lower_bound_95CI.2")])
  upper=data.matrix(variabledf[c('Upper_bound_95CI',"Upper_bound_95CI.1" ,"Upper_bound_95CI.2")])
  forestplot(labeltext=variabledf$category, mean=mean,lower=lower,upper=upper,#clip=c(20,55),
             title=paste("Chronic MSK development by Gender and",variabledesc[[variable]]),xlab="% Chronic Pain development",ylab="Age Category",
             col=fpColors(box=c("yellow", "blue","red")),zero=35.7,legend = c("Total", "Men","Women"),
             legend_args = fpLegend(pos = list(x=.85, y=0.75),gp=gpar(col="#CCCCCC", fill="#F9F9F9")))
  dev.off()
}


pdf_combine(paste(unique(Paindev$Variable),'.pdf',sep=''), output = "joined.pdf")

pdf_subset("joined.pdf",
           pages = seq(2,32,2), output = "CMSKPainDev.pdf")


Paindev = Paindev %>%
  unite(confidence_interval, c(Lower_bound_95CI,Upper_bound_95CI ), sep = "-", remove = TRUE)%>%
  unite(confidence_interval_men, c(Lower_bound_95CI.1,Upper_bound_95CI.1 ), sep = "-", remove = TRUE) %>%
  unite(confidence_interval_women, c(Lower_bound_95CI.2,Upper_bound_95CI.2 ), sep = "-", remove = TRUE)
