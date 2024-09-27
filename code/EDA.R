library(tidyverse)
library(gtExtras)
library(gt)
library(markdown)
library(ggtext)

Parties <- readRDS('Parties.RDS')
Clients <- readRDS('Clients.RDS')
Bands <- readRDS('Bands.RDS')


Bands |> head()
Parties |> head()
Clients |> head()

PartiesLocation <- Parties |>
  left_join(Clients)

CVSummary <- gtExtras::gt_plt_summary(Parties |> select(wartosckontraktu),title = "Contract values summary")

PartiesDates <- PartiesLocation |>
  mutate(Year = year(dzienrozpoczecia),
         Month = month(dzienrozpoczecia),
         miastoklienta = as.factor(miastoklienta),
         dayOfweek = weekdays(dzienrozpoczecia))

sum(PartiesLocation$wartosckontraktu)

PartiesTrend <- PartiesDates |>
  summarise(PartiesN = n(),.by = c(stanzamklienta,miastoklienta)) |>
  mutate(Flag = 
           case_when(
           dense_rank(desc(PartiesN)) == 1 ~ 'steelblue',.default = 'darkgray'))


CitySummary <- PartiesTrend |> 
  ggplot(aes(x = fct_reorder(miastoklienta, desc(PartiesN)), 
             y = PartiesN, fill = Flag)) +  # Map fill to the Flag column
  geom_col() + 
  scale_fill_identity() +  # Use the actual color values in Flag without mapping
  labs(x = "City", y = "Number of Parties", title = "Party Trend by City") +
  theme_minimal()

# Print the plot
CitySummary


PartiesValues <- gtExtras::gt_plt_summary(
  PartiesLocation |> select(wartosckontraktu,dzienrozpoczecia),title = "Summary of contracts value and parties distribiurion in time"
  )



DaysTrend <- PartiesDates |>
  summarise(PartiesN = n(),.by = c(dayOfweek))

DaysTrend  |>
  ggplot(aes(x = dayOfweek,y = PartiesN)) +
  geom_bar(stat = "identity")

BandsHeaders <- c("Band","Band City","Daily Rate","Music Style")
names(Bands) <- BandsHeaders

BandsExp <- Bands |>
  slice_max(n = 1,order_by = `Daily Rate`,by = `Music Style`) |>
  select(`Music Style`,Band,`Daily Rate`,`Band City`) |>
  arrange(`Music Style`)

BandsExp |>
  gt() |>
  tab_header(
    title = "Most expensive bands by genre") |>
  summary_rows(
    groups = FALSE,
    columns = vars(`Daily Rate`), # reference cols by name
    fns = list(
      avg = ~mean(.)
    )
  )

min(PartiesDates$dzienrozpoczecia)
max(PartiesDates$dzienrozpoczecia)

length(Parties$numerimprezy)

PartiesDates |>
  summarise(Parties = sum(1),.by = dzienrozpoczecia) |> 
  mutate(Cumulative = cumsum(Parties)) |> 
  ggplot(aes(x = dzienrozpoczecia,y = Cumulative)) +
  geom_area(alpha = 0.2,color = 'steelblue') +
  labs(x = "",y = 'Events organized',title = 'Events organized up to date') 
  
PD <- PartiesDates |>
  mutate(Index = 1,
         Cumulative = cumsum(Index)) |>
  select(numerimprezy,dzienrozpoczecia,Index,Cumulative) |>
  ggplot(aes(x = dzienrozpoczecia,y = Cumulative)) +
  geom_area(alpha = 0.1,color = 'steelblue') +
  labs(x = "",y = 'Events organized',title = 'Events organized up to date') 

PartiesDates |>
  complete(dzienrozpoczecia = seq.Date(from = min(PartiesDates$dzienrozpoczecia),
                                       to = max(PartiesDates$dzienrozpoczecia),by = 'day')) |>
  mutate(Index = case_when(!is.na(numerimprezy)~ 1,.default = 0)) |>
  summarise(Parties = sum(Index),.by = dzienrozpoczecia) |>
  select(dzienrozpoczecia,Parties) |>
  mutate(Cumulative = cumsum(Parties))

BandsExp |>
  gt() |>
  tab_header(
    title = "Most expensive bands by genre") |>
  fmt_currency(columns = 3) |>
  tab_source_note(source_note = "Data: Music Agency") |>
  tab_options(table.font.color = 'gray20',
              heading.subtitle.font.size = 12,
              heading.align = "center",
              table.border.top.color = "black",
              column_labels.border.bottom.color = "black",
              column_labels.border.bottom.width= px(3),
  )

NumberOfCities <- length(unique(PartiesLocation$miastoklienta))
PartiesLocation

BandsStats |>
  arrange(desc(`Average contract`)) |>
  gt() |>
  tab_header(
    title = "Most expensive bands") |>
  fmt_currency(columns = c("Total Contracts","Average contract",'Highest value')) |>
  gt_theme_nytimes()


PartiesDates |>
  complete(dzienrozpoczecia = seq.Date(from = min(PartiesDates$dzienrozpoczecia),
                                       to = max(PartiesDates$dzienrozpoczecia),by = 'day')) |>
  mutate(Index = case_when(!is.na(numerimprezy)~ 1,.default = 0)) |>
  summarise(Parties = sum(Index),.by = dzienrozpoczecia) |>
  select(dzienrozpoczecia,Parties) |>
  mutate(Cumulative = cumsum(Parties),
         Points = case_when(row_number() %% 10 == 0 ~ as.character(Cumulative),.default = ''))


BandsStats |>
  select(-c(Events)) |>
  arrange(desc(`Average contract`)) |>
  gt() |>
  tab_header(
    title = "Most expensive bands") |> 
  fmt_currency(columns = c("Total Contracts","Average contract",'Highest value')) |>
  tab_style(
    style = cell_text(color = "red"),
    locations = list(
      cells_body(
        columns = 2,
        rows = `Total Contracts` > 15000
      )))
         

Styles <- 
  Bands |>
  summarise(BandsPerCat = n(),.by = `Music Style`) |>
  arrange(desc(BandsPerCat)) |>
  mutate(Flag = case_when(dense_rank(desc(BandsPerCat)) == 1 ~ 'steelblue',.default = 'lightgray'))

Styles
           
           
StylesChart <- Styles |>
  ggplot(aes(x = BandsPerCat,y = reorder(`Music Style`,BandsPerCat),fill = Flag)) +
  geom_bar(stat = "identity") +
  ylab(label = "Music Styles") +
  labs(title = "Teams most often declare proficiency in <span style='color:steelblue;'>classical works</span>")+
  scale_fill_identity() +
  My_theme()

My_theme <- function() {
  theme(
    plot.title = element_markdown(),
    plot.title.position = "plot",
    panel.background = element_blank(),
    panel.grid.major = element_blank()
  )
}
max(PartiesDates$dzienrozpoczecia)
