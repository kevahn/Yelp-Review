install.packages("rvest")
install.packages("tidyverse")
library(rvest)
library(tidyverse)

url <- "https://www.yelp.com/biz/rays-place-kent"
page <- read_html(url)


#Getting Reviews From All Pages
pageNums <- page %>%
  html_elements(xpath = "//div[@class=' border-color--default__09f24__NPAKY text-align--center__09f24__fYBGO']") %>%
  html_text() %>%
  str_extract('of.*') %>%
  str_remove('of ') %>%
  as.numeric()
pageSequence <- seq(from = 0, to = (pageNums * 10)-10, by = 10)

#store items into vectors
usernames_all = c()
locations_all = c()
dates_all = c()
reviewtext_all = c()
ratings_all = c()

#For loop to retrieve values from all pages
for(i in pageSequence) {
  if (i == 0) {
    page <- read_html(url)
  } else {
    page <- read_html(paste0(url, '?start=', i))
  }
  #Customer Names
  usernames <- page %>%
    html_elements(xpath = "//div[starts-with(@class, ' user-passport')]") %>%
    html_elements(xpath = ".//a[starts-with(@href, '/user_details')]") %>%
    html_text()
  
  #Customer Locations
  locations <- page %>%
    html_elements(xpath = "//div[starts-with(@class, ' user-passport')]") %>%
    html_elements(xpath = ".//span[@class= ' css-qgunke']") %>%
    html_text() %>%
    .[.!="Location"]
  
  #Date Review Was Left
  dates <- page %>%
    html_elements(xpath = "//div[starts-with(@class, ' review')]") %>%
    html_elements(xpath = ".//span[@class= ' css-chan6m']") %>%
    html_text() 
   
  
  #Customer Review Text
  reviewtext <- page %>%
    html_elements(xpath = "//div[starts-with(@class, ' review')]") %>%
    html_elements(xpath = "(.//p[starts-with(@class, 'comment')])[1]") %>%
    html_text()
  
  #Customer Rating
  ratings <- page %>%
    html_elements(xpath = "//div[starts-with(@class, ' review')]") %>%
    html_elements(xpath = "(.//div[contains(@aria-label, 'star rating')])[1]") %>%
    html_attr("aria-label") %>%
    str_remove_all(" star rating") %>%
    as.numeric()
  
  usernames_all = append(usernames_all, usernames)
  locations_all = append(locations_all, locations)
  dates_all = append(dates_all, dates)
  reviewtext_all = append(reviewtext_all, reviewtext)
  ratings_all = append(ratings_all, ratings)
  
  #add N/As to all missing data
  length(usernames_all) <- length(dates_all)
  length(locations_all) <- length(dates_all)
  length(ratings_all) <- length(dates_all)
  length(reviewtext_all) <- length(dates_all)
  
  #removing all rows that contains N/A
  na.omit(df)
  
  #removing duplicate reviews
  dupe <- df [!duplicated(df[c('Review')]),]
  

  
  }




#Creating data frame
df <- data.frame('Date' = dates_all,
                 'Username' = usernames_all, 
                 'Located' = locations_all, 
                 'Rating' = ratings_all,
                 'Review' = reviewtext_all
                 )

#Save data frame as a CSV
write.csv(df, "yelp-review.csv")

view(df)







