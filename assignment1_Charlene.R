# for assignment 1

# Chia-Ling (Charlene) Ni

library(readxl)
library(expss)
library(tidyverse)
library(dplyr)
library(ggplot2)

rm(list=ls())
ass1 = read_excel("C:/Users/NiNi/Desktop/MKTG2505_marketing analytics/assignment/Red Ventures Telecom Chapter 4.xlsx")
summary(ass1)
spec(ass1)
sum(is.na(ass1))
colSums(is.na(ass1))


# make all the plot title in the center
theme_update(plot.title = element_text(hjust = 0.5)) 



# Q1: How many total orders placed by different Browser? What's TOP3?
Order_Browser <- aggregate(ass1$`Total Orders`, by=list(ass1$`Browser Name`,ass1$`Total Orders`),FUN=length)

ggplot(Order_Browser, aes(x=reorder(Group.1,-x),y=x), label=x, height=350)+geom_col(fill='dark blue')+
  xlab('Browser Name')+ylab('Total Orders')+ggtitle('Total Orders by Browser')+geom_text(aes(label=x),color="black",size=3,vjust=-1)

# Users from the top 3 browsers Safari, Chrom, and Chrom Mobile have placed much more orders than others. The company should focus on these three browsers' website user experience and update if needed.



# Q2: How do distinct pages views affect order placed?
Page_Order = ass1 %>% group_by(`Distinct Page Views`,`Total Orders`) %>% summarise(count=n())
Page_Order = subset(Page_Order, Page_Order$`Total Orders`==1)
Page_Order$Perc = round(Page_Order$count / sum(Page_Order$count), digits=4)*100
Page_Order$`Distinct Page Views` = as.character(Page_Order$`Distinct Page Views`)
PageOrder_battery=data.frame(Page_Order, percentvar=c(100,100))
PageOrder_battery

ggplot(PageOrder_battery, aes(x=Distinct.Page.Views,y=percentvar))+geom_col(fill="light grey")+geom_col(aes(x=Distinct.Page.Views,y=Perc,fill=Distinct.Page.Views), fill='blue')+
  xlab('Distinct Page Views')+ylab('Total Orders Percentage')+ggtitle('Total Orders Percentage by Distinct Page Views')+geom_text(aes(label=Perc),color="black",size=5,vjust=1)

# The plot shows that more than 3 distinct page views would significantly decrease the intent to make real orders. So the company should design each website page clearly to direct customers.



# Q3: Do types of device affect order placed or monthly charge among different types?
Charge_Device = ass1 %>% group_by(`Order Monthly Charge`,`Device Type`) %>% summarise(count=n())
Charge_Device_order = Charge_Device[1:13,]

ggplot(Charge_Device_order, aes(x=`Device Type`,y=count,fill=`Order Monthly Charge`))+geom_col(position="stack",width=.7)+scale_fill_gradient(low="light blue", high="dark blue")+
  xlab('Device Type')+ylab('Total Orders')+ggtitle('Total Orders by Monthly Charges and Device Types')

# Device users from Desktop and Mobile place more orders than Tablet users. They company should pay more attention to provide better websites and operating systems for desktop and mobile devices.
