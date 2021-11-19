library(tidyverse)

# Recap Exercise I: Cleaning --------------------------------------------------

# 1. Read the file measurements.csv (https://education.rstudio.com/blog/2020/08/more-example-exams/measurements.csv)
# to create a tibble called measurements. 
# (The strings "rad", "sal", and "temp" in the quantity column stand for “radiation”, “salinity”, and “temperature” respectively.)

data1 <- read.csv(url("https://education.rstudio.com/blog/2020/08/more-example-exams/measurements.csv"))


# 2. Create a tibble containing only rows where none of the values are NA and save in a tibble called cleaned.

cleaned <- na.omit(data1)
cleaned <- data1[complete.cases(data1), ]

# 3. Count the number of measurements of each type of quantity in cleaned. 
# Your result should have one row for each quantity "rad", "sal", and "temp".

cleaned %>% group_by(quantity) %>% summarise(n = n())

cleaned %>%
  count(quantity)

# 4. Display the minimum and maximum value of reading separately for each quantity in cleaned. Your result should have one row for each quantity "rad", "sal", and "temp".

cleaned %>% group_by(quantity) %>%
  summarize(minreading = min(reading), 
            maxreading = max(reading))

# 5. Create a tibble in which all salinity ("sal") readings greater than 1 are divided by 100. 
# (This is needed because some people wrote percentages as numbers from 0.0 to 1.0, but others wrote them as 0.0 to 100.0.)

##5 if its higher than 1, than divide, if not, then keep the original
cleaned %>% 
  mutate  (reading = ifelse (quantity == "sal" & reading > 1, reading / 100, reading))

## an alternative is case_when, you can have multiple nested ifs F and T means false and true

cleaned %>% 
  mutate  (reading = case_when (quantity == "sal" & reading > 1 ~ reading / 100,
                                F ~ reading,
                                T ~ reading))

# Recap Exercise II: Functions ---------------------------------------------

# 1. Read the file person.csv (https://education.rstudio.com/blog/2020/08/more-example-exams/person.csv) and store the result in a tibble called person.
#

data2 <- read.csv(url("https://education.rstudio.com/blog/2020/08/more-example-exams/person.csv"))

# 2. Write a function called summarize_table that takes a title string and a tibble as input and returns a string that says
#something like, ��title has # rows and # columns”. For example, summarize_table('our table', person) should return the 
#string "our table has 5 rows and 3 columns".

summarize_table <- function(data) {
  print(paste ("our table has", nrow(data), "rows and", ncol(data), "columns"))
}

summarize_table(data2)

# 3. Write another function called show_columns that takes a string and a tibble as input and returns a string that says something
#like, “table has columns name, name, name". 
# For example, show_columns('person', person) should return the string "person has columns person_id, personal_name, family_name".

show_columns <- function(data) {
  print(paste ("The table has columns", colnames(data), collapse = ''))
  }

show_columns(data2)

# Recap Exercise III: Tidy Data ----------------------------------------

# You want to tidy "https://education.rstudio.com/blog/2020/02/instructor-certification-exams/infant_hiv.csv")

data3 <- read.csv(url("https://education.rstudio.com/blog/2020/02/instructor-certification-exams/infant_hiv.csv"))

# a. The first column is ISO3 country codes.
# b. There are three columns for each year from 2009 to 2017. Each set has estimated, low, and high values for the year (in that order).
# c. A dash - indicates that no data is available.
# d. Our analyst tells us that >95% means “the data is unreliable”.

# Your task is to turn this into a tidy data table for further analysis:

# 1. Discuss what columns a tidy layout for this data would have and why.
# 2. Write a function that takes the link to the file containing this table as input and returns a tidy version of the table:
# 2.1 The function should replace all - and >95% values with NA. 
# 2.2 The body of the function may contain one or more pipelines and may create temporary or intermediate variables, 
# but may not contain any loops.



