# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)

# 2.0 Importing Files ----
bikes=readxl::read_excel("challenge_1/bikes.xlsx")
bikeshops=readxl::read_excel("challenge_1/bikeshops.xlsx")
order_lines=readxl::read_excel("challenge_1/orderlines.xlsx")

# 4.0 Joining Data ----
bike_orderlines_joined_tbl=left_join(order_lines, bikes, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops, by=c("customer.id"="bikeshop.id"))
# 5.0 Wrangling Data ----
bike_orderlines_joined_tbl %>% 
  separate(col=category, into=c("product_family","cat2","cat3"),sep=" - ")%>%
  select(-c("gender","url","...1"))%>%
  mutate(total_price=price*quantity) %>%
  select(-ends_with(".id"))%>%
  bind_cols(bike_orderlines_joined_tbl%>%select(order.id))%>%
  select(order.id,contains("order"),total_price,everything())%>%
  rename(bikeshop=name)%>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))->
  bike_orderlines_wrangled_tbl

bike_orderlines_wrangled_tbl
# 6.0 Business Insights ----
# 6.3 Sales by State
bike_orderlines_wrangled_tbl %>%
  separate(col=location, into=c("city","state"),sep=", ")%>%
  select(total_price, state)%>%
  group_by(state)%>%
  summarise(sales=sum(total_price))%>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                    decimal.mark = ",",
                                    prefix = "",
                                    suffix = " €"))->
  sales_by_state_tbl
# Visualization
sales_by_state_tbl%>%
  ggplot(aes(x=state, y=sales)) +
  geom_col()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_label(aes(label=sales_text))+
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "",
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by state",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )
# 6.4 Sales by State and City
bike_orderlines_wrangled_tbl %>%
  separate(col=location, into=c("city","state"),sep=", ")%>%
  select(total_price,state,order_date)%>%
  mutate(year=lubridate::year(order_date))%>%
  group_by(state,year)%>%
  summarize(sales=sum(total_price))%>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " €"))->
  sales_by_state_and_year_tbl
sales_by_state_and_year_tbl

# Visualization
sales_by_state_and_year_tbl %>%
    ggplot(aes(x=year, y=sales, fill=state))+
    geom_col()+
    facet_wrap(~state)+
    scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                      decimal.mark = ",",
                                                      prefix = "",
                                                      suffix = " €")) +
    labs(
      title = "Revenue by year and state category",
      fill = "State" # Changes the legend name
    )
