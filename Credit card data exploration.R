library(dplyr)
library(plotly)
library(lubridate)
library(eeptools)
library(readxl)
library(ggplot2)
library(stringr)
getwd()
setwd("D:\\GITHUB\\R\\Credit card data exploration")
Cust=read.csv("Customer Acquisition.csv")
Repay=read.csv("Repayment.csv")
Spend=read.csv("Spend.csv")


# 1.	In the above dataset,
# a.	Incase age is less than 18, replace it with mean of age values.

length(Cust$Age[is.na(Cust$Age)])
Cust$Age_new=ifelse(Cust$Age<18, mean(Cust$Age), Cust$Age)

# b.	In case spend amount is more than the limit, replace it with 50% of that customer's 
# limit.
# (customer's limit provided in acquisition table is the per transaction limit on his 
# card)

Cust_spend=left_join(x=Spend, y=Cust, by=c("Customer"="Customer"))
Cust_spend$Spend_amt=ifelse(Cust_spend$Amount>Cust_spend$Limit, 0.5*(Cust_spend$Limit),Cust_spend$Amount)
Cust_spend$Month=dmy(Cust_spend$Month)
# c.	Incase the repayment amount is more than the limit, 
# replace the repayment with the limit.

Cust_repay=left_join(x=Repay, y=Cust, by=c("Customer"="Customer")) %>%
  mutate(Repay_amt=ifelse(Amount>Limit, Limit, Amount))
Cust_repay$Month=dmy(Cust_repay$Month)
# 2.	From the above dataset create the following summaries:
#   a.	How many distinct customers exist?  

length(unique(Cust_spend$Customer))
message("Total number of Unique Customers=: " , length(unique(Cust_spend$Customer)))

#   b.	How many distinct categories exist?

length(unique(Cust_spend$Type))
message("Total number of Unique Categories/Type of products=: " ,length(unique(Cust_spend$Type)))

# c.	What is the average monthly spend by customers?

Cust_spend$Month_yr <- format(Cust_spend$Month, "%B-%Y")#%>% 
  # factor(ordered=T, levels=c("Jan-04","Jan-05","Jan-06","Feb-04","Feb-05",
  #                            "Feb-06","Mar-04","Mar-06","Apr-04","Apr-05",
  #                            "Apr-06","May-04","May-05","May-06","Jun-05",
  #                            "Jun-06","Jul-05","Jul-06","Aug-05","Aug-06",
  #                            "Sep-04","Sep-05","Sep-06","Oct-05","Oct-06",
  #                            "Nov-04","Nov-05","Nov-06","Dec-05","Dec-06"))
avg_spend=group_by(Cust_spend, Month_yr) %>%
  summarise(Avg_monthly_spend=mean(Amount))
str(avg_spend)
# d.	What is the average monthly repayment by customers?

Cust_repay$Month_yr <- format(Cust_repay$Month, "%B-%Y")
avg_repay=group_by(Cust_repay, Month_yr) %>%
  summarise(Avg_monthly_repay=mean(Amount))

# e.If the monthly rate of interest is 2.9%, what is the profit for the bank 
# for each month? (Profit is defined as interest earned on Monthly Profit. 
# Monthly Profit = Monthly repayment - Monthly spend. Interest is earned only
# on positive profits and not on negative amounts)  

df=cbind(avg_spend, Avg_monthly_repay=avg_repay$Avg_monthly_repay) %>%
  mutate(Monthly_profit=Avg_monthly_repay-Avg_monthly_spend) %>%
  mutate(Interest=ifelse(Monthly_profit>0, 0.029*Monthly_profit,"No profit"))

# f.What are the top 5 product types?

Top5_prod=group_by(Cust_spend, Type) %>%
  summarise(Total_spend=sum(Amount)) %>%
  arrange(desc(Total_spend)) %>%
  head(5)

#   g.	Which city is having maximum spend?

City_wise_spend=group_by(Cust_spend, City) %>%
  summarise(City_Wise_Spend=sum(Amount)) %>%
  arrange(desc(City_Wise_Spend))%>%
  head(1)

#  h.	Which age group is spending more money?

length(Cust_spend$Age[is.na(Cust_spend$Age)])

max(Cust_spend$Age)
min(Cust_spend$Age)
Cust_spend$Age_Group=ifelse(Cust_spend$Age %in% c(11:20), "11-20",ifelse(Cust_spend$Age %in% c(21:30), "21-30",
                                                         ifelse(Cust_spend$Age %in% c(31:40),"31-40",
                                                                ifelse(Cust_spend$Age %in% c(41:50),"41-50",
                                                                       ifelse(Cust_spend$Age %in% c(51:60), "51-60",
                                                                              ifelse(Cust_spend$Age %in% c(61:70), "61-70",
                                                                                     ifelse(Cust_spend$Age %in% c(71:80), "71-80","NA")))))))
Age_grp_spend=group_by(Cust_spend, Age_Group) %>%
  summarise(AgeGroup_wise_spend=sum(Amount)) %>%
  arrange(desc(AgeGroup_wise_spend)) %>%
  head(1)

#   i.	Who are the top 10 customers in terms of repayment?

Top10cust_repay=group_by(Cust_repay, Customer) %>%
  summarise(Repay_amt=sum(Repay_amt)) %>%
  arrange(desc(Repay_amt)) %>%
  head(10)

# 3.Calculate the city wise spend on each product on yearly basis. Also include a 
#   graphical representation for the same.


Cust_spend$Yr <- format(Cust_spend$Month, "%Y")
Yrly_citywise_spend=group_by(Cust_spend, City, Product,Yr) %>%
  summarise(Spend=sum(Amount))
g1=ggplot(Yrly_citywise_spend, aes(City,Spend)) +
  geom_bar(stat = "identity", aes(fill = Product), position = "dodge") +
  facet_wrap(~Yr, ncol = 3) +
  ggtitle("City and Product_wise Spend in 2004 v/s 2005 v/s 2006") +
  theme(axis.text.x=element_text(size=10, angle = -80, vjust=.1))
g1

# 4.	Create graphs for

# a.	Monthly comparison of total spends, city wise

Cust_spend$Month_new=months(Cust_spend$Month) %>%
  factor(levels=month.name)
City_wise_mspend=group_by(Cust_spend, City,Month_new) %>%
  summarise(Spend=sum(Amount))

g2=ggplot(City_wise_mspend, aes(Month_new,Spend))+
  geom_bar(stat = "identity", aes(fill =City ), position = "dodge") +
  ggtitle("Monthly comparison of total spends, city wise")+
  theme(axis.text.x=element_text(size=10, angle = -80, vjust=.1))+
  xlab("Month")
g2

 
# b.	Comparison of yearly spend on air tickets

Air_tkt_spend=filter(Cust_spend, Type=="AIR TICKET") %>%
  group_by(Yr) %>%
  summarise(Spend=sum(Amount))

g3=plot_ly(Air_tkt_spend, x=~Yr, y=~Spend, type="bar") %>%
  layout(title="Yearly Spend on Air-Tickets",
         yaxis=list(title="Spent amount"),
         xaxis=list(title="Year"))
g3

#OR
fig=ggplot(Air_tkt_spend, aes(Yr,Spend))+
  geom_bar(stat = "identity", colour="Blue", fill = "#FF6666")+
  ggtitle("Yearly Spend on Air-Tickets")+
  xlab("Year")
fig

# c.	Comparison of monthly spend for each product (look for any seasonality that exists in terms of spend)

Productwise_mspend=group_by(Cust_spend,Month_yr,Product) %>%
  summarise(Monthly_Productwise_Spend=sum(Amount))

g4=ggplot(Productwise_mspend, aes(Month_yr,Monthly_Productwise_Spend))+
  geom_bar(stat = "identity", colour="Blue", position = "dodge", fill="#FF6666")+
  labs(title = "Monthly Product_wise Spend", caption = "comment: As we move from January to December the Spend amount decreases irrespective of the Product-type. ")+ 
  facet_wrap(~Product)+
  theme(axis.text.x=element_text(size=10, angle = -80, vjust=.1))
g4


# 5.Write user defined R function to perform the following analysis:
#   You need to find top 10 customers for each city in terms of their 
#   repayment amount by different products and by different time periods 
#   i.e. year or month. The user should be able to specify the product 
#   (Gold/Silver/Platinum) and time period (yearly or monthly) and the function 
#   should automatically take these inputs while identifying the top 10 customers.
str(Cust_repay)
Final = function(data, Product, Time)
{
  if(Time == 'Monthly' | Time == 'monthly')
  {
    p = Cust_repay[Cust_repay$Product== Product,] %>%
      group_by(Customer,City, months(Month)) %>%
      summarise(TotalPayments = sum(Amount))
    
    top_10 = top_n(p,10)
    
  }
  
  else if (Time == 'Yearly' | Time == 'yearly')
  {
    p = Cust_repay[Cust_repay$Product== Product,] %>%
      group_by(Customer,City, year(Month)) %>%
      summarise(TotalPayments = sum(Amount))
    
    top_10 = top_n(p,10)
  }
  
  else{
    stop(paste("The Time period should be Yearly or Monthly, instead found", Time, "non-permissible"))
    
  }
  return(top_10)
}

View(Final(Cust_repay,Product="Gold", Time=""))

