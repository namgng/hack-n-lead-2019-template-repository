# HACKNLEAD 2019
#-------------------------------------------------------#
# We created a fake database which is structured and simulated in Excel with 3 senarios (found the results on "Results" worksheet.
 (1) without donation(income0)
 (2) donation of 0.2(income1)
 (3) donation of 0.5(income2) from the customers 
Looking at the violon graph with 3 senarios, we could see the distribution of worker'shares by countries: China and India.
By using donation from the customer, the average observed salary of worker increases even higher than the reference salary (red line for China 
and blue line for India). 
#-------------------------------------------------------#

install.packages("stringr")
library(readxl)
library(ggplot2)
library(actuar)

#read the data from excel

market<- read_excel("HacknLead_data_SBNQ.xlsx", sheet = "Market Insight", col_names = FALSE)
transaction<- read_excel("HacknLead_data_SBNQ.xlsx", sheet = "Transaction_2019", col_names = FALSE)
product<- read_excel("HacknLead_data_SBNQ.xlsx", sheet = "Product_master", col_names = FALSE)
work<- read_excel("HacknLead_data_SBNQ.xlsx", sheet = "Market Insight", col_names = FALSE)


#Creating data frame
df[,1]<-data.frame(workID=transaction[-1,1])
df[,2]<-data.frame(country=transaction[-1,2])
df[,3]<-data.frame(product=transaction[-1,3])
df[,4]<-data.frame(subproduct=transaction[-1,4])
df[,5]<-data.frame(transactionmonth=transaction[-1,5])
df[,6]<-data.frame(number=transaction[-1,6])
df[,7]<-data.frame(income=transaction[-1,7])
df[,8]<-data.frame(incomep=transaction[-1,8])
df[,9]<-data.frame(incomepp=transaction[-1,9])

names(df)=c("workID","country","product","subproduct","subproduct","transaction","number","income0","income1", "income2")

dfcountry <- select(df, "country", "income0","income1","income2" )
#transform character to integer
dfcountry<- transform(dfcountry, income0 = as.numeric(income0))
dfcountry<- transform(dfcountry, income1 = as.numeric(income1))
dfcountry<- transform(dfcountry, income2 = as.numeric(income2))
sapply(dfcountry, mode)

#Reporting 
mu <- dfcountry %>% 
  group_by(country) %>%
  summarise(grp.mean = mean(income2))
mu

p1<-ggplot(dfcountry,aes(x=country, y=income0, fill=country))+ geom_violin(trim=FALSE)
p1<-p1+ geom_hline(yintercept = 2.6,color="red")+ geom_hline(yintercept = 1.6,color="blue")
p2<-ggplot(dfcountry,aes(x=country, y=income1, fill=country))+ geom_violin(trim=FALSE)
p2<-p2+ geom_hline(yintercept = 2.6,color="red")+ geom_hline(yintercept = 1.6,color="blue")
p3<-ggplot(dfcountry,aes(x=country, y=income2, fill=country))+ geom_violin(trim=FALSE)
p3<-p3+ geom_hline(yintercept = 2.6,color="red")+ geom_hline(yintercept = 1.6,color="blue")

#p3 + stat_summary(fun.y=mean, geom="point", shape=23, size=2)+ geom_line(aes(y=mean(income2)),color = "red", linetype = "dotted")


ggplot(filter(dfcountry, country %in% c("China", "India")),
       aes(x=country,
           y=income0,
           color=country))+
  geom_point()

grid.arrange(p1, p2, p3, top = "Senario Development by Donation")

par(mfrow=c(4,4))
h1<- hist(dfcountry$income0)
h2<- hist(dfcountry$income1)
h3<- hist(dfcountry$income2)

grid.arrange(hist(dfcountry$income0),
hist(dfcountry$income1),
hist(dfcountry$income2), top = "Senario Development by Donation")

