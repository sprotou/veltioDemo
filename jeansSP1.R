library(tidyverse)
setwd('C:/Users/sprot/Documents/GitHub/453')
jeansDataRaw = read_csv('RETAILDATA.csv')
#general EDA, correlations etc---------------------
cor(x = jeansDataRaw$`Shelf Spaces`, y= jeansDataRaw$`Items Sold`)

# jeansData = jeansDataRaw %>%
#   mutate(my_act_sales= parse_number(`Actual Sales`),
#          my_coup = parse_number(Coupons),
#          my_grp_conv_toimp = parse_number(`TV GRP`)*(1.2*10^6))

jeansData = jeansDataRaw %>%
  mutate_all(funs(parse_number))

jeansData$`Day of the Week`
  
jeansDataNum = jeansData %>%
  select_if(is.numeric) %>%
    select(-Date, -`Day of the Week`)
?cor
ourCor =  cor(jeansDataNum, use = 'everything') #any other way to handle NA? complete will drop the whole column even for 1 NA.
ourCor
corrplot::corrplot(ourCor) 

jeansData$`Day of the Week` = jeansDataRaw$`Day of the Week`
jeansData$Date = jeansDataRaw$Date

forShelves = jeansData %>%
  group_by(`Shelf Spaces`) %>%
    summarise(meanProfitPerShelf = mean(`Profit/ Shelf`))

ggplot(forShelves) + geom_bar(mapping = aes(x=forShelves$`Shelf Spaces`, y = forShelves$meanProfitPerShelf), stat='identity')

jeansData = jeansData %>%
  rename(profit_per_shelf = `Profit/ Shelf`,
         shelf_spaces = `Shelf Spaces`)

jeansData$profit_per_shelf

jeansData %>%
  filter(Markdown == 1) %>%
  select(profit_per_shelf) %>%
    lapply(mean, na.rm = T)

jeansData %>%
  filter(Discount == 1) %>%
  select(profit_per_shelf) %>%
  lapply(mean, na.rm = T)

jeansData %>%
  filter(Clearance == 1) %>%
  select(profit_per_shelf) %>%
  lapply(mean, na.rm = T)

jeansData %>%
  filter(Clearance == 0 & Markdown == 0 & Discount == 0) %>%
  select(profit_per_shelf) %>%
  lapply(mean, na.rm = T)

ggplot(jeansData) + geom_point(mapping = aes(x = jeansData$`Average Price`, y = jeansData$profit_per_shelf))

jeansData %>%
  filter(Type == 3) %>%
  select(profit_per_shelf) %>%
  lapply(mean, na.rm = T)

byDay = jeansData %>%
  group_by(`Day of the Week`) %>%
    summarise(meanProfit = mean(Profit),
              meanProfitPerShelf = mean(profit_per_shelf))

write_csv(x = byDay, path = 'byday.csv')

#keep in mind 15 has very few observations, so that is by no means realiable. However, there is a clear downward trend


#elasticity of shelves assigned------------------
#ideally we need an elasticity graph for this, as the relationship is not necassarily linear. At the minumum, we can use the correlation score for profit to shelves
#or we can just let a ML model handle this for us



#elasticity on price (discounts)----------------
#we need to quantify how many extra sales we can expect from a clearance / discount / etc
#or we can just let a ML model handle this for us

jeansData = jeansData %>%
  rename(avg_price = `Average Price`)

p2 =  jeansData %>%
  filter(Type == 2) %>%
    ggplot(.) + geom_point(aes(x = avg_price, y = Sales)) + geom_smooth(aes(x = avg_price, y = Sales),method=lm)
p1
p2
p3 
