library(dplyr)
#Set working Directory:
setwd('G:/Sem 4/GLT/Assignment 2/Logistics Data and Data analysis')
#Read SalesOrders csv file

#------------------------------------------------------------------------------------------------------------------------------------------------------
#SALES ORDER FILE
#------------------------------------------------------------------------------------------------------------------------------------------------------


# QUESTION 1
#------------------------------------------------------------------------------------------------------------------------------------------------------
  weekly_sales_df<- read.csv('sales_order_weekly.csv')
  #Import Data from dataFrame to summarry table
  # Find Weekly sales by category
  item_week_summary<- weekly_sales_df %>% group_by(ITEM,WEEK) %>% summarise(WEEKLYSALES=sum(UNITS_SOLD),.groups = "drop")
  
  #Find Average Weekly Sales
  item_Week_average<-item_week_summary %>% group_by(ITEM)%>% summarise(WEEKLY_AVGSALES=mean(WEEKLYSALES))%>% arrange(desc(WEEKLY_AVGSALES))

  #Q1.a
    head(item_Week_average,n=1)
    #Answer:
    #    ITEM         WEEKLY_AVGSALES
    #  1 389739         186026
  
  #Q1.b
  summarise(item_Week_average, Sum_Of_Avg = sum(WEEKLY_AVGSALES))
    #Answer:
    #     Sum_Of_Avg
    # 1    912756.

# QUESTION 2
#------------------------------------------------------------------------------------------------------------------------------------------------------
  #Creating new dataframe for each week sales and grouping by Items and summarising to find total sales for each item in each week
  #Week1
  WEEK_1_SALES<- weekly_sales_df[weekly_sales_df$WEEK==1,]%>%group_by(ITEM)%>%summarise(WEEKL1_ITEM_SALES = sum(UNITS_SOLD))
  
  #Week1
  WEEK_2_SALES<- weekly_sales_df[weekly_sales_df$WEEK==2,]%>%group_by(ITEM)%>%summarise(WEEKL2_ITEM_SALES = sum(UNITS_SOLD))
  
  #Week1
  WEEK_3_SALES<- weekly_sales_df[weekly_sales_df$WEEK==3,]%>%group_by(ITEM)%>%summarise(WEEKL3_ITEM_SALES = sum(UNITS_SOLD))
  
  #Week1
  WEEK_4_SALES<- weekly_sales_df[weekly_sales_df$WEEK==4,]%>%group_by(ITEM)%>%summarise(WEEKL4_ITEM_SALES = sum(UNITS_SOLD))
  
  #Week1
  WEEK_5_SALES<- weekly_sales_df[weekly_sales_df$WEEK==5,]%>%group_by(ITEM)%>%summarise(WEEKL5_ITEM_SALES = sum(UNITS_SOLD))
  
  
  #Join all Tables
  Aggregate_Table<- full_join(WEEK_1_SALES,WEEK_2_SALES, by='ITEM')%>%
                     full_join(WEEK_3_SALES,by='ITEM')%>%
                        full_join(WEEK_4_SALES,by='ITEM')%>%
                          full_join(WEEK_5_SALES,by='ITEM')
  #Finding Weekly Total Sales
  Aggregate_Table<- Aggregate_Table%>% mutate(TOTALSALES=select(.,WEEKL1_ITEM_SALES:WEEKL5_ITEM_SALES)%>% rowSums(na.rm=TRUE))
  #Finding Weekly Average sales 
  Aggregate_Table$AVGSALES<- Aggregate_Table$TOTALSALES/5

  #Q2.a
    sum(Aggregate_Table$AVGSALES)
    #Answer
      #[1] 893359
      #There is a difference in Sum of averages across all ites from Method_1 and method_2                          


  #Q2.b
    # I believe that the difference in value is due to the inaccuracy in calculations in method from Q1. In method1
    #the possibility of an item not being sold in any of the weeks from WEEK1 to WEEK5 is being overlooked, because in the raw data
    #instead of entering zero as a value for no items sold in a week, there is no data of that Item in the respective week
    #leading to a skewed result. For example ITEM no 52209 has no values in 4 weeks, so method from Q1 calculated averageSales = 226/1=226
    #while method frOM Q2 calculated averagesales = 226/5 = 45.2.
    Aggregate_Table[Aggregate_Table$ITEM=='52209',]
    # ITEM WEEKL1_ITEM_SALES WEEKL2_ITEM_SALES WEEKL3_ITEM_SALES WEEKL4_ITEM_SALES WEEKL5_ITEM_SALES AVERAGE TOTALSALES
    # 52209         NA               226                NA                NA            NA            45.2        226


# QUESTION 3
#------------------------------------------------------------------------------------------------------------------------------------------------------  
  Item_Sales_By_Customer<-weekly_sales_df%>% group_by(ITEM,CUSTOMER_NUMBER) %>% 
                            summarise(Total_units=sum(UNITS_SOLD),.groups = 'drop')%>%
                              arrange(desc(Total_units))
  #Answer:
  #Highest sales 
  head(Item_Sales_By_Customer,n=1)
  #    ITEM         CUSTOMER_NUMBER  Total_units
  #   389739            5336         15760        
  

#------------------------------------------------------------------------------------------------------------------------------------------------------
#Inventory History File and Item Master File
#------------------------------------------------------------------------------------------------------------------------------------------------------

# QUESTION 4
#------------------------------------------------------------------------------------------------------------------------------------------------------
  inv_df<-read.csv('inventory_weekly_summary.csv')
  
  #Average Inventory Levels
  avg_inv_levels<-inv_df%>% group_by(ITEM) %>% summarise(AIL = mean(UNITS_ON_HAND))
    #Q4.a
    head(avg_inv_levels%>%arrange(desc(AIL)),n=1)
    #ANSWER
    #ITEM     AIL
    #1315560  12331
    
    #Q4.b
    sum(avg_inv_levels$AIL) #= 429775.8
    
  #Reading item_master.csv
  item_df<-read.csv('item_master.csv')

# QUESTION 5
#------------------------------------------------------------------------------------------------------------------------------------------------------
  
  item_df_with_inventory<- inner_join(item_df,avg_inv_levels, by='ITEM')%>%
                      mutate(AIL_VALUE = UNIT_INVENTORY_VALUE*AIL)
  #ANSWER
  sum(item_df_with_inventory$AIL_VALUE) #= $22,090,628
 

#------------------------------------------------------------------------------------------------------------------------------------------------------
#Bringing It All Together
#------------------------------------------------------------------------------------------------------------------------------------------------------
  
  item_total_sales<- weekly_sales_df%>% group_by(ITEM) %>% summarise(TOTAL_UNITS_SOLD = sum(UNITS_SOLD))
  
  #Creating master Items table
  item_df_all<- inner_join(item_total_sales,item_df_with_inventory, by ='ITEM')
    
  item_df_all<- item_df_all%>%mutate(REVENUE=TOTAL_UNITS_SOLD*UNIT_SELLING_PRICE)%>%

                                mutate(GROSS_MARGIN =REVENUE - AIL_VALUE)
  
# QUESTION 6
#------------------------------------------------------------------------------------------------------------------------------------------------------  
  #Answer
  #Gross margin for all items combined together
  sum(item_df_all$GROSS_MARGIN) #= $292,285,489

  
# QUESTION 7
#------------------------------------------------------------------------------------------------------------------------------------------------------  
  
  #Answer
  #Gross margin for Dairy category
  item_df_all[item_df_all$CATEGORY == 'Dairy',]%>%summarise(GM=sum(GROSS_MARGIN)) # = $1,321,049

  
# QUESTION 8
#------------------------------------------------------------------------------------------------------------------------------------------------------  

  # GMROI 
  sum(item_df_all$GROSS_MARGIN)/sum(item_df_all$AIL_VALUE) #=13.2312  
  