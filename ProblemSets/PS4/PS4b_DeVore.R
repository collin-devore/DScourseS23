# 1)
# R script open


# 2)
# Installed


# 3)
library("sparklyr")
library("tidyverse")


# 4)
spark_install(version = "3.0.0")
sc <- spark_connect(master = "local")


# 5)
df1 <- as_tibble(iris)


# 6)
df <- copy_to(sc, df1)


# 7)
print(class(df1))
print(class(df))
print("df1 is a tibble dataframe and df is a lazy dataframe or a spark dataframe.")


# 8)
print(head(df1))
print(head(df))
print("The names do, in fact, change. Specifically, the underscore symbols in df become periods in df1. This makes sense because the names may be delimited by a different symbol, or the underscores may have a different specific meaning so they have to be changed.")


# 9)
	# Part a)
df %>% select(Sepal_Length,Species) %>% head %>% print


# 10)
	# Part a)
df %>% filter(Sepal_Length>5.5) %>% head %>% print


# 11)
df %>% select(Sepal_Length,Species) %>% filter(Sepal_Length>5.5) %>% head %>% print


# 12)
df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length), count = n()) %>% head %>% print


# 13)
	# Part a)
df2 <- df2 %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length), count = n()) %>% head %>% print

	# Part b)
df2 %>% arrange(Species) %>% head %>% print
