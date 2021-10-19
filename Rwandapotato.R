# # 1. Importing important price data
# # Every time I go to the supermarket, my wallet weeps a little. But how expensive is food around the world?
# #  we'll explore time series of food prices in Rwanda from the United Nations Humanitarian Data Exchange Global Food Price Database.
# # Agriculture makes up over 30% of Rwanda's economy, and over 60% of its export earnings (CIA World Factbook), 
# # so the price of food is very important to the livelihood of many Rwandans.
# # 
# # The map below shows the layout of Rwanda; it is split into five administrative regions. 
# # The central area around the Capital city, Kigali, is one region, and the others are North, East, South, and West.
# # 
# # A map of the five administrative regions of Rwanda
# # 
# # In this notebook, we're going to import, manipulate, visualize and forecast Rwandan potato price data.
# # We'll also wrap our analysis into functions to make it easy to analyze prices of other foods.
# 
# United Nations Humanitarian Data Exchange Global Food Price Database


# Loading packages Tidyverse,forecast,Tseries,dygraphs
library(tidyverse)
library(forecast)
library(dygraphs)
library(tseries)
library(DataExplorer)
# The dataset contains food prices from 2010-present

glimpse(World_foodprices)


# create a duplicate dataset leaving orginal dataset 
df=World_foodprices


# Let filter based on Rwanda and Potato

potatodf_Rwanda=df %>% 
    filter(adm0_name=="Rwanda" &cm_name=="Potatoes (Irish) - Retail")

# Remove unwanted column for simpler analysis 

df1=potatodf_Rwanda[-c(1,2,3,8,9,14,18)]
df1=df1[-5]

glimpse(df1)


# Creating new colnames for easy maniplution 

colnames(df1)=c("Name","Mkt","Mkt_name","CM","Pt","PtName","Um","Month","Year","Price")
glimpse(df1)


# Let remove more columns 
Rwanda_potato=df1[-c(2,5)]


unique(Rwanda_potato$PtName)

# let combine month and Year into single date
library(lubridate)
rw=Rwanda_potato %>% 
  mutate(date=ymd(paste(Year,Month,"01"))) %>% 
  select(-Month,-Year)

rw$date=as.Date(rw$date)
rw

rwa=rw[-7]
rwa_ts=ts(rwa$Price,start=2010-07-01,frequency = 12)
dygraph(rwa_ts)


plot_bar(rwa)
plot_correlation(rwa)
plot_histogram(rwa)


dygraph(forecast(rwa_ts,h=16))
rwfor=forecast(rwa_ts,h=16)
rwfor
ggplot(data=rw,aes(x=date,y=Price))+geom_point()+geom_smooth(col="red")+geom_line()
  
