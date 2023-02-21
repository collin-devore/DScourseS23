# Part a)
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20230209&lang=en"')

# Part b)
system('cat dates.json')

# Part c)
library("jsonlite")
library("tidyverse")
dates.json <- fromJSON('dates.json')
mydf <- bind_rows(dates.json$result[-1])

# Part d)
print(class(mydf))
print(class(mydf$date))

# Part e)
print(head(mydf))
