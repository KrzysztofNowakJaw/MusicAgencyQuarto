install.packages("hrbrthemes")
library(hrbrthemes)
library(geomtextpath)
library(tidyverse)
library(RPostgreSQL)
library(tidyverse)

theme_set(theme_light())

# Connect to your PostgreSQL database
con <- dbConnect(dbDriver("PostgreSQL"),
                 user = "postgres",
                 password = "DomowySQL",
                 dbname = "sqlda",
                 host = "::1",
                 port = 5432)


query <- "SELECT 

customer_id
,product_id
,sales_amount
,INITCAP(channel) as Channel
,sales_transaction_date AS Timestamp

FROM sales
"



Sales <- dbGetQuery(con,query)

SalesYear <- Sales |>
  mutate(timestamp = as.Date(timestamp),
         timestamp = floor_date(timestamp,unit = "month"),
         Year = year(timestamp))


Sales |> head()

Sales |>
  filter(channel == "Internet") |>
  mutate(timestamp = as.Date(timestamp),
         timestamp = floor_date(timestamp,unit = "month"),
         Label = if_else(row_number() == nrow(Sales),channel,"")) |>
  summarise(Sales = mean(sales_amount),.by = c(timestamp,channel)) |>
  ggplot(aes(x = timestamp,y = Sales)) +
  geom_line(label = 'internet') +
  geom_textpath(size = 5, vjust = 1, text_only = TRUE) +
  scale_x_date(date_breaks = "3 years")


B <- Sales |>
  filter(channel == "Internet") |>
  mutate(timestamp = as.Date(timestamp),
         timestamp = floor_date(timestamp,unit = "month")) |>
  summarise(Sales = mean(sales_amount),.by = c(timestamp,channel)) |>
  arrange(timestamp)

ChartData <- B |>
  mutate(Label = case_when(row_number() == nrow(B) ~ channel,.default = NULL))

p <- ChartData |>
  ggplot(aes(x = timestamp,y = Sales)) +
  geom_line() 

ChartData

p + geom_text(
  aes(x = timestamp,y = Sales,label = Label)) +
   hrbrthemes::theme_ft_rc()


p + geom_text(
  aes(x = timestamp,y = Sales,label = Label),vjust = -4) +
  hrbrthemes::theme_ft_rc()

SY <- SalesYear |>
  summarise(Sales = sum(sales_amount)/100,.by = c(Year,channel))

SY |> head()

SY |>
 ggplot(aes(x = channel,y = Sales)) +
  geom_col() +
  scale_y_continuous(n.breaks = 10,name = "Sales") 

