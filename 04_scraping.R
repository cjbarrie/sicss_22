library(tidyverse) # loads dplyr, ggplot2, and others
library(ggthemes) # includes a set of themes to make your visualizations look nice!
library(readr) # more informative and easy way to import data
library(stringr) # to handle text elements
library(rvest) #for scraping

pamphdata <- read_csv("https://raw.githubusercontent.com/cjbarrie/sicss_21/main/01_scraping_APIs/data/pamphlets_formatted_gsheets.csv")

# first inspect of html
url <- "https://wayback.archive-it.org/2358/20120130161341/http://www.tahrirdocuments.org/2011/03/voice-of-the-revolution-3-page-2/"

html <- read_html(url)
html

# look at text
pagetext <- html %>%
  html_text()

pagetext

# identify relevant text
pagetext <- html %>%
  html_elements("p") %>%
  html_text(trim=TRUE)

pagetext

# get page date

pagedate <- html %>% 
  html_elements(".calendar") %>%
  html_text(trim=TRUE)

pagedate

# get start url

starturl <- "https://wayback.archive-it.org/2358/20120130135111/http://www.tahrirdocuments.org/"

#get month urls

urls <- character(0)
for (i in 3:13) {
  url <- "https://wayback.archive-it.org/2358/20120130143023/http://www.tahrirdocuments.org/"
  newurl <- ifelse(i <10, paste0(url,"2011/0",i,"/"), 
                   ifelse(i>=10 & i<=12 , paste0(url,"2011/",i,"/"), 
                          paste0(url,"2012/01/")))
  urls <- c(urls, newurl)
}

# get urls for each page

urlpages_all <- character(0) #create empty character string to deposit our final set of urls
urlpages <- character(0) #create empty character string to deposit our urls for each page of each month
for (i in seq_along(urls)) { #for loop for each url stored above
  url <- urls[i] #take the first url from the vector of urls created above
  html <- read_html(url) #read the html
  pages <- html %>%
    html_elements(".page") %>% #grab the page element
    html_text() #convert to text
  pageints <- as.integer(pages) #convert to set of integers
  npages <- max(pageints, na.rm = T) #get number of highest integer
  
  for (j in 1:npages) { #for loop for each of 1:highest page integer for that month's url
    newurl <- paste0(url,"page/",j,"/") #create new url by pasting "page/" and then the number of that page, and then "/", matching the url structure identified above
    urlpages <- c(urlpages, newurl) #bind with previously created page urls for each month
  }
  urlpages_all <- c(urlpages_all, urlpages) #bind the monthly page by page urls together
  urlpages <- character(0) #empty urlpages for next iteration of the first for loop
  urlpages_all <- gsub("page/1/", "", urlpages_all) #get rid of page/1/ as not needed
}

# get urls for each pamphlet page

pamlinks_all <- character(0)
for (i in seq_along(urlpages_all)) {
  url <- urlpages_all[i]
  html <- read_html(url)
  links <- html_elements(html, ".post , h2") %>%
    html_children() %>%
    html_attr("href") %>%
    na.omit() %>%
    `attributes<-`(NULL)
  pamlinks_all <- c(pamlinks_all, links)
}

# get final dataset

df_empty <- data.frame()
for (i in seq_along(pamlinks_all)) {
  url <- pamlinks_all[i]
  html <- read_html(url)
  cat("Collecting url number ",i,": ", url, "\n")
  
  error <- tryCatch(html <- read_html(url),
                    error=function(e) e)
  if (inherits(error, 'error')) {
    df <- data.frame(title = NA,
                     date = NA,
                     text = NA,
                     imageurl = NA,
                     tags = NA)
    next
  }
  
  df <- data.frame(matrix(ncol=0, nrow=length(1)))
  #get titles
  titles <- html_elements(html, ".title") %>%
    html_text(trim=TRUE)
  
  title <- titles[1]
  df$title <- title
  
  #get date
  date <- html_elements(html, ".calendar") %>%
    html_text(trim=TRUE)
  df$date <- date
  
  #get text
  textsep <-  html_elements(html, "p") %>%
    html_text(trim=TRUE)
  text <- paste(textsep, collapse = ",")
  df$text <- text
  
  #get tags
  pamtags <- html_elements(html, ".category") %>%
    html_text(trim=TRUE)
  df$tags <- pamtags
  
  #get link to original pamphlet image
  elements_other <-  html_elements(html, "a") %>%
    html_children()
  url_element <- as.character(elements_other[2])
  imgurl <- str_extract(url_element, "src=\\S+")
  imgurl <- substr(imgurl, 6, (nchar(imgurl)-1))
  
  df$imageurl <- imgurl
  
  df_empty <- rbind(df_empty, df)
}