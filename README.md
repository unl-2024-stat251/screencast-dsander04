# Screencast

[Link to video](https://www.youtube.com/watch?v=Oy_HXVDABp8)



# Timestamps

3:30 - Tidyiing Data (tidyverse, pipe, select, filter, mutate, etc.)

5:00 and 24:15 - date times and lubridate features

19:00, 48:30, 59:50, 1:04:00 - ggplot, aesthetics, colors, labels, general plotting guides

52:00 - creating a function

57:00 - group_by and summarize statements



#Screencast Code

```{R}
scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

```

```{R}
library(tidyverse)
library(ggplot2)

```


DATE AIRED
```{R}
years <- scoobydoo %>% 
  select(index, date_aired, format) %>%
  mutate(date_aired = as.Date(date_aired)) %>%
  separate(date_aired, c("Year","Month","Day"), sep = "-") %>%
  mutate(year = as.numeric(year))

sixties <- years %>%
  filter(Year < 1970)

seventies <- years %>%
  filter(Year >= 1970 & Year <=1979)

eighties <- years %>%
  filter(Year > 1979 & Year <1991)

nineties <- years %>%
  filter(Year > 1989 & Year <2001)

twothousands <- years %>%
  filter(Year > 1999 & Year <2011)

twentytens <- years %>%
  filter(Year > 2009 & Year <2021)

twentytwenties <- years %>%
  filter(Year >2019)

decade_count <- data.frame(
  Decade = c("1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s"),
  Count = c(nrow(sixties), nrow(seventies), nrow(eighties), nrow(nineties), nrow(twothousands), nrow(twentytens), nrow(twentytwenties))
)

ggplot(decade_count, aes(x=Decade, y=Count, fill=Decade)) +
  geom_bar(stat="identity") +
  labs(title = "Pieces of Scooby-Doo Media by Decade")
```

MORE MESSING AROUND WITH DATES
```{R}
library(lubridate)

birthday <- years %>%
  filter(Month == "07", Day == "18")

january <- years %>%
  filter(Month == "01")

random <- scoobydoo %>%
  filter(index == "347")

adding <- years %>%
  mutate(date = make_date(Year, Month, Day)) %>%
  filter(index == "100")
adding2 <- adding$date %m+% months(6)

difference <- years %>%
  mutate(date=make_date(Year, Month, Day)) %>%
  filter(index=="200")

time_diff <- difftime(difference$date, adding$date)
print(time_diff)

```


CATCHPHRASES
```{R}
catchphrases <- scoobydoo %>%
  mutate(across(everything(), as.numeric)) %>%
  select(split_up, another_mystery, set_a_trap, jeepers, jinkies, my_glasses, just_about_wrapped_up, zoinks, groovy, scooby_doo_where_are_you, rooby_rooby_roo)

catchphrases <- catchphrases %>%
  mutate_all(~ifelse(.=="NULL", 0, .)) %>%
  mutate_all(~ifelse(is.na(.), 0, .))

catchphrase_count <- colSums(catchphrases)

catchphrase_count <- data.frame(catchphrase = names(catchphrases), count = catchphrase_count)

ggplot(catchphrase_count, aes(x=catchphrase, y=count)) + geom_bar(stat="identity") + labs(title="Catchphrase Popularity") + theme(axis.text.x = element_text(angle=45, hjust=1))
```


VOICE ACTORS
```{R}
voices <- scoobydoo %>% 
  select(fred_va, daphnie_va, velma_va, shaggy_va, scooby_va)

scooby_function <- function(voices){
  na.omit(voices)
  
unique_va <- sapply(voices, function(x) length(unique(x)))

return(unique_va)
}
scooby_function(voices)

for(i in new)
  number = length(unique(i))
filter(!"NULL")
```


RUN TIMES
```{R}
runtimes <- scoobydoo%>%
  select(index, run_time, format) %>%
  na.omit()

total_time <- runtimes %>%
  group_by(format) %>%
  summarize(run_total = sum(run_time))
  
total_time

ggplot(total_time, aes(x=format, y=run_total)) + geom_bar(stat="identity", fill="lightblue") + labs(title = "Total RunTime by Format")
```


ENGAGEMENT AND RATINGS
```{R}
ratings <- scoobydoo %>%
  select(imdb, engagement, format) %>%
  na.omit() %>%
  filter(!engagement=="NULL")

ratings <- ratings %>%
  mutate(imdb = as.numeric(imdb)) %>%
  mutate(engagement = as.numeric(engagement))

ggplot(ratings, aes(x=imdb, y=engagement, color=format)) + geom_point() + labs(title="IMDB vs. ENGAGEMENT")
```
