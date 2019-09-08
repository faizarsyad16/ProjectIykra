
# Title     : IMDB Web Scraping
# Objective : To scrap IMDb website for the 50 most popular feature films released in 2019.
# Author    : Faiz Farhansyah Arsyad


# Step 1 : Install packages 
install.packages('rvest')
library('xml2')
library('rvest')

#Step 2 : Ambil website yang ingin di scrapping

url <- 'https://www.imdb.com/search/title/?year=2019&title_type=feature&'

#Step 3 : Membaca html code dari website yang ingin di scrapping
webpage <- read_html(url)

# Deskripsi kolom sebagai berikut

# Rank: The rank of the film from 1 to 50 on the list of 50 most popular feature films released in 2019.
# Title: The title of the feature film.
# Description: The description of the feature film.
# Runtime: The duration of the feature film.
# Genre: The genre of the feature film,
# Rating: The IMDb rating of the feature film.
# Votes: Votes cast in favor of the feature film.
# Gross_Earning_in_Mil: The gross earnings of the feature film in millions.
# Director: The main director of the feature film. Note, in case of multiple directors, I'll take only the first.
# Actor: The main actor of the feature film. Note, in case of multiple actors, I'll take only the first.



#Step 4 : Menggunakan css selectors untuk membuat scraping pada section ranking

rank_data_html <- html_nodes(webpage,'.text-primary')

#Step 5 : Konversikan data menjadi text
rank_data <- html_text(rank_data_html)

#Step 6 : Melihat contoh data rankingnya
head(rank_data)

length(rank_data) #50


#Step 7 : Konversikan data menjadi numerikal
rank_data<-as.numeric(rank_data)

#Step 8 : Lihat contoh datanya
head(rank_data)



#Step 9 : Menggunakan selector untuk melakukan scrapping pada section title
title_data_html <- html_nodes(webpage,'.lister-item-header a')

#Step 10 : Konversikan kedalam bentuk text
title_data <- html_text(title_data_html)

#Step 11 : Lihat contoh datanya
head(title_data)


#Step 12 : Menggunakan selector untuk melakukan scrapping pada section description
description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')

#Step 13 : Konversikan kedalam bentuk text
description_data <- html_text(description_data_html)

#Step 14 : Lihat contoh datanya
head(description_data)

#Step 15 : Data Preprocessing
description_data<-gsub("\n","",description_data)

#Step 16 : Lihat contoh datanya
head(description_data)
length(description_data) #40

#Step 17 : Menggunakan selector untuk melakukan scrapping pada section runtime
runtime_data_html <- html_nodes(webpage,'.runtime')

#Step 18 : Konversikan kedalam bentuk text
runtime_data <- html_text(runtime_data_html)

#Step 19 : Lihat contoh data runtime
head(runtime_data)

#Step 20 : Data Preprocessing

runtime_data<-gsub(" min","",runtime_data)
runtime_data<-as.numeric(runtime_data)

#Step 21 : Lihat contoh data setelah di preprocessing
head(runtime_data)

length(runtime_data) #44


#Step 21 : Menggunakan selector untuk melakukan scrapping pada section genre
genre_data_html <- html_nodes(webpage,'.genre')

#Step 22 : Konversikan data menjadi text
genre_data <- html_text(genre_data_html)

#Step 23 : Lihat contoh datanya
head(genre_data)

#Step 24 : Data preprocessing untuk genre 
genre_data<-gsub("\n","",genre_data)

#Step 25 : Data preprocessing untuk genre bagian 2 : Menghilangkan space
genre_data<-gsub(" ","",genre_data)

#Step 26 : Menggunakan genre pertama dalam data
genre_data<-gsub(",.*","",genre_data)

#Convering each genre from text to factor
genre_data<-as.factor(genre_data)

#Let's have another look at the genre data
head(genre_data)
length(genre_data) #50

#Using CSS selectors to scrap the IMDB rating section
rating_data_html <- html_nodes(webpage,'.ratings-imdb-rating strong')

#Converting the ratings data to text
rating_data <- html_text(rating_data_html)

#Let's have a look at the ratings
head(rating_data)

#Data-Preprocessing: converting ratings to numerical
rating_data<-as.numeric(rating_data)

#Let's have another look at the ratings data
head(rating_data)
length(rating_data) #40


#Using CSS selectors to scrap the votes section
votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')

#Converting the votes data to text
votes_data <- html_text(votes_data_html)

#Let's have a look at the votes data
head(votes_data)

#Data-Preprocessing: removing commas
votes_data<-gsub(",","",votes_data)

#Data-Preprocessing: converting votes to numerical
votes_data<-as.numeric(votes_data)

#Let's have another look at the votes data
head(votes_data)
length(votes_data) #40

#Using CSS selectors to scrap the directors section
directors_data_html <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)')

#Converting the directors data to text
directors_data <- html_text(directors_data_html)

#Let's have a look at the directors data
head(directors_data)

#Data-Preprocessing: converting directors data into factors
directors_data<-as.factor(directors_data)

#Using CSS selectors to scrap the actors section
actors_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')

#Converting the gross actors data to text
actors_data <- html_text(actors_data_html)

#Let's have a look at the actors data
head(actors_data)

#Data-Preprocessing: converting actors data into factors
actors_data<-as.factor(actors_data)
length(actors_data) #50

#Using CSS selectors to scrap the gross revenue section
gross_data_html <- html_nodes(webpage,'.ghost~ .text-muted+ span')

#Converting the gross revenue data to text
gross_data <- html_text(gross_data_html)

#Let's have a look at the votes data
head(gross_data)

#Data-Preprocessing: removing '$' and 'M' signs
gross_data<-gsub("M","",gross_data)

gross_data<-substring(gross_data,2,6)

#Data-Preprocessing: converting gross to numerical
gross_data<-as.numeric(gross_data)

#Let's look at the length of gross data
length(gross_data) #95

# Step 11: Now we have successfully scraped all the 9 features for the 50 most popular feature films released in 2019. Let's combine them to create a dataframe and inspect its structure.

#Combining all the lists to form a data frame (exclude variable with NA so that we get same number of row)
movies_df<-data.frame(Rank = rank_data, Title = title_data,
                      #Description = description_data, Runtime = runtime_data,
                      Genre = genre_data,
                      #Rating = rating_data,
                      #Gross_Earning_in_Mil = gross_data,
                      Director = directors_data, Actor = actors_data)


#Structure of the data frame
str(movies_df)
#You have now successfully scraped the IMDb website for the 50 most popular feature films released in 2019.

write.csv(movies_df, "web_scrapping_IMDB.csv")


