install.packages('RPostgreSQL')
library(RPostgreSQL)
library(tidyverse)

theme_set(theme_light())

# Connect to your PostgreSQL database
con <- dbConnect(dbDriver("PostgreSQL"),
                 user = "postgres",
                 password = "DomowySQL",
                 dbname = "Agencja",
                 host = "::1",
                 port = 5432)


query <- "SELECT 

numerimprezy
,wartosckontraktu
,idklienta
,idagenta
,dzienrozpoczecia

FROM agencjaartystycznazmiana.imprezy AS AI
"

ClientsQ <- "SELECT 

idklienta
,miastoklienta
,stanzamklienta
,kodpocztowyklienta

FROM agencjaartystycznazmiana.klienci"

Artists <- "SELECT 

W.nazwascenicznawykonawcy AS Band
,miastowykonawcy AS BandCity
,stawkadziennawykonawcy AS DailyRate
,    CASE nazwastylu
        WHEN 'Muzyka kościelna' THEN 'Church Music'
        WHEN 'Rock & Roll' THEN 'Rock & Roll'
        WHEN 'Motown' THEN 'Motown'
        WHEN '40 hitów wszech czasów' THEN '40 All-Time Hits'
        WHEN 'Musicale' THEN 'Musicals'
        WHEN 'Country Rock' THEN 'Country Rock'
        WHEN 'Country' THEN 'Country'
        WHEN 'Salsa' THEN 'Salsa'
        WHEN 'Klasyczna' THEN 'Classical'
        WHEN 'Muzyka lat 70.' THEN '70s Music'
        WHEN 'Rhythm and Blues' THEN 'Rhythm and Blues'
        WHEN 'Folk' THEN 'Folk'
        WHEN 'Muzyka lat 60.' THEN '60s Music'
        WHEN 'Mieszanka' THEN 'Mixed'
        WHEN 'Standardy' THEN 'Standards'
        WHEN 'Współczesna' THEN 'Contemporary'
        WHEN 'Jazz' THEN 'Jazz'
        ELSE 'Unknown'
    END AS MusicStyle

FROM agencjaartystycznazmiana.wykonawcy AS W
LEFT JOIN agencjaartystycznazmiana.style_wykonawcow AS SW On W.idwykonawcy = SW.idwykonawcy
LEFT JOIN agencjaartystycznazmiana.style_muzyczne AS SM ON SM.idstylu = SW.idstylu"

ArtistsStats <- "
SELECT 

nazwascenicznawykonawcy
,count(numerimprezy) AS Parties
,Sum(wartosckontraktu) AS TotalValue
,Round(avg(wartosckontraktu),2) AS AverageContract
,max(wartosckontraktu) AS MaxContract
,
FROM agencjaartystycznazmiana.imprezy AS AI
LEFT JOIN agencjaartystycznazmiana.wykonawcy AS W ON AI.idwykonawcy = W.idwykonawcy
Group by AI.idwykonawcy,nazwascenicznawykonawcy "

Parties <- dbGetQuery(con,query)
Clients <- dbGetQuery(con,ClientsQ)
Bands <- dbGetQuery(con,Artists)
BandsStats <- dbGetQuery(con,ArtistsStats)
head(Parties)

saveRDS(Parties,'Parties.RDS')
saveRDS(Clients,'Clients.RDS')
saveRDS(Bands,'Bands.RDS')
saveRDS(BandsStats,'BandsStats.RDS')

