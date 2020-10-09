library(readxl)
library(readr)
library(lattice)
library(rpivotTable)
library(ggplot2)
library(dplyr)
library(corrgram)
library(corrplot)
library(Hmisc)
library(psych)
library(car)
library(mice)
library(DMwR)

setwd("C:/greatlakes/mra/prjct")
mktBasket= read_xlsx("mba.xlsx")
print(mktBasket)
str(mktBasket)



mktBasket$`Bill Number`=as.factor(mktBasket$`Bill Number`)#convert the Invoice_No from 'int' to 'fctr'
mktBasket$`Item Desc`=as.factor(mktBasket$`Item Desc`)
str(mktBasket)



mktBasket.Agg=split(mktBasket$`Item Desc`,mktBasket$`Bill Number`)
head(mktBasket.Agg)


mktBasket.Agg2=list()
for(i in 1:length(mktBasket.Agg)){
  mktBasket.Agg2[[i]]=unique(mktBasket.Agg[[i]])
}
head(mktBasket.Agg2)





library(arules)# 'arules' has a specific data structure called Transactions
Txns=as(mktBasket.Agg2,"transactions")
summary(Txns)
inspect(Txns[10])#here we are looking at the 10th transactions


freq=itemFrequency(Txns)#gives us the frequency of each items
freq=freq[order(-freq)]#gives the frequencies in descending order
freq["MOROCCAN MINT TEA"]#gives the frequency of Bread
barplot(freq[1:20])#first 20 items barplot

itemFrequencyPlot(Txns,topN=20)



arules=apriori(data=Txns)
inspect(sort(arules,by='lift'))



arules2=apriori(data=Txns,
                parameter=list(support=0.0001,confidence=0.60,maxlen=6)
)
inspect(sort(arules2,by='lift'))

arules2d
arules2d = arules2
arules2d = sort(arules2,by='lift')
arules2d
arules2d = as.data.frame(arules2d)
View(arules2d)
write.csv(arules2,"Combos.csv")
inspect(arules2)


library(RColorBrewer)#we are using this library to plot the colour as a gradient rather than a single colour
library(arulesViz)
plot(arules2,control=list(col=brewer.pal(11,"Spectral")))#the plot function now works on the basis of the arulesViz package
#ideally, here we are looking for something on the top right corner with a deep red
```

subrules2=head(sort(arules2,by="support"),20)
inspect(subrules2)
plot(subrules2,method="graph")


plot(subrules2, method = "grouped")

rules_df=as(arules2,"data.frame")#here we are converting arules2 in to a data frame
#Rule: {A}=>{B}
#Probability(A)-LHS Support
rules_df$LHSSupport=rules_df$support/rules_df$confidence#gives us the probability of A on the LHS
#Probability(B)-RHS Support
rules_df$RHSSupport=rules_df$confidence/rules_df$lift#gives us the probability of B on the RHS
print(rules_df)#here we can finally use the print command rather than inspect as it has been converted in to a data frame
write.table(rules_df,file="MBA_output.csv",sep=",",append=FALSE,row.names = FALSE)