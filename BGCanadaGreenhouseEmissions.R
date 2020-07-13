# Date: 12/07/2020
# Author: Bahareh Golfar
# Version 1 

# Description: Analysis of Greenhouse Emissions of Sectors of Government of Canada
#              based on published data for 2010 to 2018
#              https://open.canada.ca/data/en/dataset?keywords=energy&page=2


# Questions:
# 1. which sectors generate most of the emissions (visualizing using ggplot2)
# 2. What is the trend of annual emission in each sector (Linear, polynomial and SVR regression)
# 3. Predict the emissions of each sector in 2019 and 2020


# Data Preprocessing 

rm(list=ls())
# Importing the dataset
data = read.csv('BG20182019Greenhouse.csv')
dataset=data[159:1107,] #Eliminating rows for 2005 because we don't have any data between 2005 and 2010

# Encoding categorical data
dataset$GHG.source = factor(dataset$GHG.source,
                            levels = c('facilities', 'fleet'),
                            labels = c(1, 2))


# Visualising emissions
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Year, y = dataset$Total..emissions),
             colour = 'green') +
  #geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
  #         colour = 'blue') +
  ggtitle('Plot 1: Total Greenhouse Emissions') +
  xlab('Fiscal Year') +
  ylab('Total Emissions')

AgricultureEmissions=0
BorderServicesEmissions=0
CanadaRevenueEmissions=0
EmploymentSocialEmissions=0
EnvironmentEmissions=0
FisheriesEmissions=0
HealthCanadaEmissions=0
ImmigrationEmissions=0
IndigenousAffairsEmissions=0
InnovationEmissions=0
NationalDefenceEmissions=0
NaturalResourcesEmissions=0
ParksEmissions=0
PublicServicesEmissions=0
TransportCanadaEmissions=0

for (i in 1:949) {
  if (dataset$Federal.organization[i]=='Agriculture and Agri-Food Canada'){
    AgricultureEmissions[i]=dataset$Emissions..t.[i]} 
  #else {AgricultureEmissions[i]=''}
  
  if (dataset$Federal.organization[i]=='Canada Border Services Agency'){
    BorderServicesEmissions[i]=dataset$Emissions..t.[i]} 
  
  if (dataset$Federal.organization[i]=='Canada Revenue Agency'){
    CanadaRevenueEmissions[i]=dataset$Emissions..t.[i]} 
  
  if (dataset$Federal.organization[i]=='Employment and Social Development Canada'){
    EmploymentSocialEmissions[i]=dataset$Emissions..t.[i]}
  
  if (dataset$Federal.organization[i]=='Environment and Climate Change Canada'){
    EnvironmentEmissions[i]=dataset$Emissions..t.[i]}
  
  if (dataset$Federal.organization[i]=='Fisheries and Oceans Canada'){
    FisheriesEmissions[i]=dataset$Emissions..t.[i]}
  
  if (dataset$Federal.organization[i]=='Health Canada'){
    HealthCanadaEmissions[i]=dataset$Emissions..t.[i]}
  
  if (dataset$Federal.organization[i]=='Immigration, Refugees and Citizenship Canada'){
    ImmigrationEmissions[i]=dataset$Emissions..t.[i]}
  
  if (dataset$Federal.organization[i]=='Indigenous and Northern Affairs Canada'){
    IndigenousAffairsEmissions[i]=dataset$Emissions..t.[i]}
  
  if (dataset$Federal.organization[i]=='Innovation, Science and Economic Development Canada'){
    InnovationEmissions[i]=dataset$Emissions..t.[i]}
  
  if (dataset$Federal.organization[i]=='National Defence'){
    NationalDefenceEmissions[i]=dataset$Emissions..t.[i]}
  
  if (dataset$Federal.organization[i]=='Natural Resources Canada'){
    NaturalResourcesEmissions[i]=dataset$Emissions..t.[i]}
  
  if (dataset$Federal.organization[i]=='Parks Canada'){
    ParksEmissions[i]=dataset$Emissions..t.[i]}
  
  if (dataset$Federal.organization[i]=='Public Services and Procurement Canada'){
    PublicServicesEmissions[i]=dataset$Emissions..t.[i]}
  
  if (dataset$Federal.organization[i]=='Transport Canada'){
    TransportCanadaEmissions[i]=dataset$Emissions..t.[i]}
  
}


AgricultureTotalSum=sum(AgricultureEmissions, na.rm = TRUE)
BorderServicesTotalSum=sum(BorderServicesEmissions, na.rm = TRUE)
CanadaRevenueTotalSum=sum(CanadaRevenueEmissions, na.rm = TRUE)
EmploymentSocialTotalSum=sum(EmploymentSocialEmissions, na.rm = TRUE)
EnvironmentTotalSum=sum(EnvironmentEmissions, na.rm = TRUE)
FisheriesTotalSum=sum(FisheriesEmissions, na.rm = TRUE)
HealthCanadaTotalSum=sum(HealthCanadaEmissions, na.rm = TRUE)
ImmigrationTotalSum=sum(ImmigrationEmissions, na.rm = TRUE)
IndigenousAffairsTotalSum=sum(IndigenousAffairsEmissions, na.rm = TRUE)
InnovationTotalSum=sum(InnovationEmissions, na.rm = TRUE)
NationalDefenceTotalSum=sum(NationalDefenceEmissions, na.rm = TRUE)
NaturalResourcesTotalSum=sum(NaturalResourcesEmissions, na.rm = TRUE)
ParksTotalSum=sum(ParksEmissions, na.rm = TRUE)
PublicServicesTotalSum=sum(PublicServicesEmissions, na.rm = TRUE)
TransportCanadaTotalSum=sum(TransportCanadaEmissions, na.rm = TRUE)

Sectors=c("Agri","Border", "CRA","Employment",
          "Environment", "Fisheries", "Health", "Immigration",
          "Indigenous", "Innovation", "ND",
          "NR", "Parks", "PS","Transport")

TotalEmissions=c(AgricultureTotalSum,BorderServicesTotalSum,CanadaRevenueTotalSum,
                 EmploymentSocialTotalSum,EnvironmentTotalSum,FisheriesTotalSum,
                 HealthCanadaTotalSum,ImmigrationTotalSum,IndigenousAffairsTotalSum,
                 InnovationTotalSum,NationalDefenceTotalSum,NaturalResourcesTotalSum,
                 ParksTotalSum,PublicServicesTotalSum,TransportCanadaTotalSum)

Contribution=data.frame(Sectors,TotalEmissions)

library(ggplot2)
s<-ggplot(data=Contribution, aes(x=Sectors , y=TotalEmissions)) + 
  geom_bar(stat = "identity")+
  ggtitle('Plot 2: Each Sector Contribution') 

s

AgricultureEmissions2010=0
AgricultureEmissions2011=0
AgricultureEmissions2012=0
AgricultureEmissions2013=0
AgricultureEmissions2014=0
AgricultureEmissions2015=0
AgricultureEmissions2016=0
AgricultureEmissions2017=0
AgricultureEmissions2018=0


for (j in 1:949) {
  if (dataset$Year[j]==2010 && is.numeric(AgricultureEmissions[j])){
    AgricultureEmissions2010[j]=AgricultureEmissions[j]} 
  
  if (dataset$Year[j]==2011 && is.numeric(AgricultureEmissions[j])){
    AgricultureEmissions2011[j]=AgricultureEmissions[j]} 
  
  if (dataset$Year[j]==2012 && is.numeric(AgricultureEmissions[j])){
    AgricultureEmissions2012[j]=AgricultureEmissions[j]} 
  
  if (dataset$Year[j]==2013 && is.numeric(AgricultureEmissions[j])){
    AgricultureEmissions2013[j]=AgricultureEmissions[j]} 
  
  if (dataset$Year[j]==2014 && is.numeric(AgricultureEmissions[j])){
    AgricultureEmissions2014[j]=AgricultureEmissions[j]} 
  
  if (dataset$Year[j]==2015 && is.numeric(AgricultureEmissions[j])){
    AgricultureEmissions2015[j]=AgricultureEmissions[j]} 
  
  if (dataset$Year[j]==2016 && is.numeric(AgricultureEmissions[j])){
    AgricultureEmissions2016[j]=AgricultureEmissions[j]} 
  
  if (dataset$Year[j]==2017 && is.numeric(AgricultureEmissions[j])){
    AgricultureEmissions2017[j]=AgricultureEmissions[j]} 
  
  if (dataset$Year[j]==2018 && is.numeric(AgricultureEmissions[j])){
    AgricultureEmissions2018[j]=AgricultureEmissions[j]} 
}

Agriculture2010Sum=sum(AgricultureEmissions2010, na.rm = TRUE)
Agriculture2011Sum=sum(AgricultureEmissions2011, na.rm = TRUE)
Agriculture2012Sum=sum(AgricultureEmissions2012, na.rm = TRUE)
Agriculture2013Sum=sum(AgricultureEmissions2013, na.rm = TRUE)
Agriculture2014Sum=sum(AgricultureEmissions2014, na.rm = TRUE)
Agriculture2015Sum=sum(AgricultureEmissions2015, na.rm = TRUE)
Agriculture2016Sum=sum(AgricultureEmissions2016, na.rm = TRUE)
Agriculture2017Sum=sum(AgricultureEmissions2017, na.rm = TRUE)
Agriculture2018Sum=sum(AgricultureEmissions2018, na.rm = TRUE)

FiscalYear=c(2010,2011,2012,2013,2014,2015,2016,2017,2018)
AnnualAgricultureContribution=c(Agriculture2010Sum,Agriculture2011Sum,Agriculture2012Sum,Agriculture2013Sum,
                                Agriculture2014Sum,Agriculture2015Sum,Agriculture2016Sum,
                                Agriculture2017Sum,Agriculture2018Sum)

# Visualising each sector emissions

# Agriculture sector Annual Emissions
library(ggplot2)
ggplot() +
  geom_point(aes(x = FiscalYear, y = AnnualAgricultureContribution),
             colour = 'blue') +
  ggtitle('Plot3: Agriculture Greenhouse Emissions') +
  xlab('Fiscal Year') +
  ylab('Total Annual Agriculture Emissions')


BorderServicesEmissions2010=0
BorderServicesEmissions2011=0
BorderServicesEmissions2012=0
BorderServicesEmissions2013=0
BorderServicesEmissions2014=0
BorderServicesEmissions2015=0
BorderServicesEmissions2016=0
BorderServicesEmissions2017=0
BorderServicesEmissions2018=0

for (j in 1:949) {
  if (dataset$Year[j]==2010 && is.numeric(BorderServicesEmissions[j])){
    BorderServicesEmissions2010[j]=BorderServicesEmissions[j]} 
  
  if (dataset$Year[j]==2011 && is.numeric(BorderServicesEmissions[j])){
    BorderServicesEmissions2011[j]=BorderServicesEmissions[j]} 
  
  if (dataset$Year[j]==2012 && is.numeric(BorderServicesEmissions[j])){
    BorderServicesEmissions2012[j]=BorderServicesEmissions[j]} 
  
  if (dataset$Year[j]==2013 && is.numeric(BorderServicesEmissions[j])){
    BorderServicesEmissions2013[j]=BorderServicesEmissions[j]} 
  
  if (dataset$Year[j]==2014 && is.numeric(BorderServicesEmissions[j])){
    BorderServicesEmissions2014[j]=BorderServicesEmissions[j]} 
  
  if (dataset$Year[j]==2015 && is.numeric(BorderServicesEmissions[j])){
    BorderServicesEmissions2015[j]=BorderServicesEmissions[j]} 
  
  if (dataset$Year[j]==2016 && is.numeric(BorderServicesEmissions[j])){
    BorderServicesEmissions2016[j]=BorderServicesEmissions[j]} 
  
  if (dataset$Year[j]==2017 && is.numeric(BorderServicesEmissions[j])){
    BorderServicesEmissions2017[j]=BorderServicesEmissions[j]} 
  
  if (dataset$Year[j]==2018 && is.numeric(BorderServicesEmissions[j])){
    BorderServicesEmissions2018[j]=BorderServicesEmissions[j]} 
  
}

BorderServices2010Sum=sum(BorderServicesEmissions2010, na.rm = TRUE)
BorderServices2011Sum=sum(BorderServicesEmissions2011, na.rm = TRUE)
BorderServices2012Sum=sum(BorderServicesEmissions2012, na.rm = TRUE)
BorderServices2013Sum=sum(BorderServicesEmissions2013, na.rm = TRUE)
BorderServices2014Sum=sum(BorderServicesEmissions2014, na.rm = TRUE)
BorderServices2015Sum=sum(BorderServicesEmissions2015, na.rm = TRUE)
BorderServices2016Sum=sum(BorderServicesEmissions2016, na.rm = TRUE)
BorderServices2017Sum=sum(BorderServicesEmissions2017, na.rm = TRUE)
BorderServices2018Sum=sum(BorderServicesEmissions2018, na.rm = TRUE)

AnnualBorderServicesContribution=c(BorderServices2010Sum,BorderServices2011Sum,BorderServices2012Sum,
                                   BorderServices2013Sum,BorderServices2014Sum,BorderServices2015Sum,
                                   BorderServices2016Sum,BorderServices2017Sum,BorderServices2018Sum)


# Border Services sector Annual Emissions
library(ggplot2)
ggplot() +
  geom_point(aes(x = FiscalYear, y = AnnualBorderServicesContribution),
             colour = 'blue') +
  #geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
  #         colour = 'blue') +
  ggtitle('Plot 4: Border Services Greenhouse Emissions') +
  xlab('Fiscal Year') +
  ylab('Total Annual Border Services Emissions')


TransportCanadaEmissions2010=0
TransportCanadaEmissions2011=0
TransportCanadaEmissions2012=0
TransportCanadaEmissions2013=0
TransportCanadaEmissions2014=0
TransportCanadaEmissions2015=0
TransportCanadaEmissions2016=0
TransportCanadaEmissions2017=0
TransportCanadaEmissions2018=0


for (j in 1:949) {
  if (dataset$Year[j]==2010 && is.numeric(TransportCanadaEmissions[j])){
    TransportCanadaEmissions2010[j]=TransportCanadaEmissions[j]} 
  
  if (dataset$Year[j]==2011 && is.numeric(TransportCanadaEmissions[j])){
    TransportCanadaEmissions2011[j]=TransportCanadaEmissions[j]}
  
  if (dataset$Year[j]==2012 && is.numeric(TransportCanadaEmissions[j])){
    TransportCanadaEmissions2012[j]=TransportCanadaEmissions[j]}
  
  if (dataset$Year[j]==2013 && is.numeric(TransportCanadaEmissions[j])){
    TransportCanadaEmissions2013[j]=TransportCanadaEmissions[j]}
  
  if (dataset$Year[j]==2014 && is.numeric(TransportCanadaEmissions[j])){
    TransportCanadaEmissions2014[j]=TransportCanadaEmissions[j]}
  
  if (dataset$Year[j]==2015 && is.numeric(TransportCanadaEmissions[j])){
    TransportCanadaEmissions2015[j]=TransportCanadaEmissions[j]}
  
  if (dataset$Year[j]==2016 && is.numeric(TransportCanadaEmissions[j])){
    TransportCanadaEmissions2016[j]=TransportCanadaEmissions[j]}
  
  if (dataset$Year[j]==2017 && is.numeric(TransportCanadaEmissions[j])){
    TransportCanadaEmissions2017[j]=TransportCanadaEmissions[j]}
  
  if (dataset$Year[j]==2018 && is.numeric(TransportCanadaEmissions[j])){
    TransportCanadaEmissions2018[j]=TransportCanadaEmissions[j]}
  
}


TransportCanada2010Sum=sum( TransportCanadaEmissions2010, na.rm = TRUE)
TransportCanada2011Sum=sum( TransportCanadaEmissions2011, na.rm = TRUE)
TransportCanada2012Sum=sum( TransportCanadaEmissions2012, na.rm = TRUE)
TransportCanada2013Sum=sum( TransportCanadaEmissions2013, na.rm = TRUE)
TransportCanada2014Sum=sum( TransportCanadaEmissions2014, na.rm = TRUE)
TransportCanada2015Sum=sum( TransportCanadaEmissions2015, na.rm = TRUE)
TransportCanada2016Sum=sum( TransportCanadaEmissions2016, na.rm = TRUE)
TransportCanada2017Sum=sum( TransportCanadaEmissions2017, na.rm = TRUE)
TransportCanada2018Sum=sum( TransportCanadaEmissions2018, na.rm = TRUE)

AnnualTransportCanadaContribution=c(TransportCanada2010Sum,TransportCanada2011Sum,TransportCanada2012Sum,
                                    TransportCanada2013Sum,TransportCanada2014Sum,TransportCanada2015Sum,
                                    TransportCanada2016Sum,TransportCanada2017Sum,TransportCanada2018Sum)


# Transport Canada sector Annual Emissions
library(ggplot2)
ggplot() +
  geom_point(aes(x = FiscalYear, y = AnnualTransportCanadaContribution),
             colour = 'blue') +
  #geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
  #         colour = 'blue') +
  ggtitle('plot 5: Transport Canada Greenhouse Emissions') +
  xlab('Fiscal Year') +
  ylab('Total Annual Transport Canada Emissions')

AgriResults=data.frame(FiscalYear,AnnualAgricultureContribution)


# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = AnnualAgricultureContribution ~ FiscalYear,
               data = dataset)

Agritest_set = read.csv('Agritestset.csv')

y_pred = predict(regressor, newdata = Agritest_set)


# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = AgriResults$FiscalYear, y = AgriResults$AnnualAgricultureContribution),
             colour = 'red') +
  geom_line(aes(x = AgriResults$FiscalYear, y = predict(regressor, newdata = AgriResults)),
            colour = 'blue') +
  ggtitle('Plot 6: Linear Regression of Agriculture Contribution vs year') +
  xlab('Year') +
  ylab('Annual Agriculture Greenhouse Contribution')


TransportResults=data.frame(FiscalYear,AnnualTransportCanadaContribution)

# Fitting Polynomial Regression to the dataset
scaledFY=c(10,11,12,13,14,15,16,17,18)
TransportResults3=data.frame(scaledFY,AnnualTransportCanadaContribution)

poly_reg = lm(formula = AnnualTransportCanadaContribution ~ poly(scaledFY, 4, raw=TRUE),
              data = TransportResults3)

# Visualising the Polynomial Regression results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = TransportResults3$scaledFY, y = TransportResults$AnnualTransportCanadaContribution),
             colour = 'red') +
  geom_line(aes(x = TransportResults3$scaledFY, y = predict(poly_reg, newdata = TransportResults3)),
            colour = 'blue') +
  ggtitle('plot 7: 4th order polynomial regression of Transport Canada Greenhouse Contribution (NOT A GOOD FIT)') +
  xlab('Year') +
  ylab('Transport Canada Annual Contribution')


library(e1071)
regressorSVM = svm(formula = AnnualTransportCanadaContribution ~ scaledFY,
                   data = TransportResults3,
                   type = 'eps-regression',
                   kernel = 'radial')

# Visualising the SVR results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = TransportResults3$scaledFY, y = TransportResults$AnnualTransportCanadaContribution),
             colour = 'red') +
  geom_line(aes(x = TransportResults3$scaledFY, y = predict(regressorSVM, newdata = TransportResults3)),
            colour = 'blue') +
  ggtitle('Plot 8: SVR regression of Transport Canada Greenhouse Contribution') +
  xlab('Year') +
  ylab('Transport Canada Annual Contribution')

Transpotest_set = read.csv('Transpotestset.csv')

y_pred2 = predict(poly_reg, newdata = Transpotest_set)

y_pred3 = predict(regressorSVM, newdata = Transpotest_set)

print("Agriculture emissions prediction for 2019 and 2020")
show(y_pred)

print("Transport sector emissions prediction for 2019 and 2020")
show(y_pred3)

