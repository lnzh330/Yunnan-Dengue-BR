#Extract Markov jumps
library(readxl)
library(dplyr)
library(openxlsx)

dataD1<-read.csv('D3jumptimes.txt',sep = '\t')

#From Yunnan
datafryu<-filter(dataD1,from=='Yunnan')
datafryu$to<-as.factor(datafryu$to)
datayutoba<-filter(datafryu,to =='Bangladesh')
datayutobh<-filter(datafryu,to =='Bhutan')
datayutoca<-filter(datafryu,to =='Cambodia')
datayutoid<-filter(datafryu,to =='India')
datayutoin<-filter(datafryu,to =='Indonesia')
datayutola<-filter(datafryu,to =='Laos')
datayutosi<-filter(datafryu,to =='Singapore')
datayutosr<-filter(datafryu,to =='SriLanka')
datayutota<-filter(datafryu,to =='Thailand')
datayutovi<-filter(datafryu,to =='VietNam')
datayutoma<-filter(datafryu,to =='Malaysia')
datayutomy<-filter(datafryu,to =='Myanmar')
datayutoco<-filter(datafryu,to =='China-other')
datayutoet<-filter(datafryu,to =='EastTimor')
datayutoph<-filter(datafryu,to =='Philippines')
datayutopa<-filter(datafryu,to =='Pakistan')

#To Yunnan
datatoyu<-filter(dataD1,to=='Yunnan')
datafrba<-filter(datatoyu,from =='Bangladesh')
datafrbh<-filter(datatoyu,from =='Bhutan')
datafrca<-filter(datatoyu,from =='Cambodia')
datafrid<-filter(datatoyu,from =='India')
datafrin<-filter(datatoyu,from =='Indonesia')
datafrla<-filter(datatoyu,from =='Laos')
datafrsi<-filter(datatoyu,from =='Singapore')
datafrsr<-filter(datatoyu,from =='SriLanka')
datafrta<-filter(datatoyu,from =='Thailand')
datafrvi<-filter(datatoyu,from =='VietNam')
datafrma<-filter(datatoyu,from =='Malaysia')
datafrmy<-filter(datatoyu,from =='Myanmar')
datafrco<-filter(datatoyu,from =='China-other')
datafret<-filter(datatoyu,from =='EastTimor')
datafrph<-filter(datatoyu,from =='Philippines')
datafrpa<-filter(datatoyu,from =='Pakistan')

#write excel
sheetsfr<-list('toBangladeshi'=datayutoba,'toBhutan'=datayutobh,'toCambodia'=datayutoca,'toChinaother'=datayutoco,'toIndia'=datayutoid,
               'toIndiesia'=datayutoin, 'toLaos'=datayutola,'toSingapore'=datayutosi,'toSrilanka'=datayutosr,'toThailand'=datayutota,
               'toVietNam'=datayutovi,'toMalaysia'=datayutoma,'toMyanmar'=datayutomy,'toEastTimor'=datayutoet,'toPhilippines'=datayutoph,
               'toPakistan'=datayutopa)
write.xlsx(sheetsfr,"From Yunnan-D3.xlsx")

sheetsto<-list('frBangladeshi'=datafrba,'frBhutan'=datafrbh,'frCambodia'=datafrca,'frChinaother'=datafrco,'frIndia'=datafrid,
               'frIndiesia'=datafrin, 'frLaos'=datafrla,'frSingapore'=datafrsi,'frSrilanka'=datafrsr,'frThailand'=datafrta,
               'frVietNam'=datafrvi,'frMalaysia'=datafrma,'frMyanmar'=datafrmy,'frEastTimor'=datafret,'frPhilippines'=datafrph,
               'frPakistan'=datafrpa)
write.xlsx(sheetsto,"To Yunnan-D3.xlsx")

