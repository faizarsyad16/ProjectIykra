# Title       : Homework - Formatting_Data_Online_Retail_Faiz.R 
# Description : Melakukan proses formatting dataset online retail
# Objective   : Formatting data
# Data source : https://archive.ics.uci.edu/ml/datasets/online+retail 

# Step 1 : Menginstall packages yang relevan

install.packages('tidyverse')
install.packages('lubridate')
install.packages('DataExplorer')
install.packages('readxl')

#note : packages readxl ditujukan untuk membaca file excel (dataset online retail dalam bentuk excel)


# Step 2 : Load library dari packages yang diinstall
library(tidyverse)
library(lubridate)
library(DataExplorer)
library(readxl)


# Step 3 : Membaca dataset dengan menggunakan readxl
online_retail_data <- read_excel('Online_Retail.xlsx')

# Step 4 : Melihat contoh data untuk 6 rows pertama
head(online_retail_data)

# Step 5 : Melihat contoh data untuk 6 rows terakhir
tail(online_retail_data)

# Step 6 : Melihat summary data 
summary(online_retail_data)

# Step 7 : Memploting jumlah missing data
plot_missing(online_retail_data)

#Note : Didapat bahwa terdapat 24,93% data customer ID yang hilang

# Step 8 : Menampilkan struktur data
str(online_retail_data)

# Step 9 : Melakukan cleaning data. Cleaning dapat dilakukan dengan dua opsi yaitu menyertakan unique value dan mendrop customer id dengan command is na
online_retail_data_new = online_retail_data[!is.na(online_retail_data$CustomerID),]

#note : saya menggunakan opsi command is na

# Step 10 : Cek missing data dengan plot
plot_missing(online_retail_data_new)

#Menjawab soal homework --> Please make a tidy table from produk, transaksi, and profil_pelanggan table, thus contain the following variables using ol_sample_drop data frame:

# Step 11 : Membuat tabel dengan 2 variabel 
# CustomerID | frequency
# frequency adalah berapa kali satu orang membeli barang
frequency <- online_retail_data_new %>% group_by(CustomerID) %>% summarise(frequency = n_distinct(InvoiceNo))

# Step 12 : Menampilkan isi 6 rows pertama pada frekuensi untuk mengecek variabel
head(frequency)

# Step 13 : Membuat tabel dengan 2 variabel:
# CustomerID | monetary
# monetary adalah berapa jumlah uang yang dibelanjakan oleh masing-masing orang
monetary = online_retail_data_new %>% group_by(CustomerID) %>% summarise(monetary = sum(UnitPrice*Quantity))

# Step 14 : Menampilkan isi 6 rows pertama pada monetery untuk mengecek variabel
head(monetary)

# Step 15 : Membuat tabel dengan 2 variabel
# Menambahkan satu kolom di tabel df_online_retail_drop bernama 'recency'
# recency berisi berapa hari sejak pembelian terakhir customer ke tanggal 31 Desember 2011
recency = online_retail_data_new %>% group_by(CustomerID) %>% arrange(desc(InvoiceDate)) %>% filter(row_number()==1) %>% mutate(recency = as.numeric(as.duration(interval(InvoiceDate, ymd('2011-12-31'))))/86400)

# Step 16 :  Menampilkan isi 6 rows pertama pada recency untuk mengecek variabel
head(recency)

# Step 17 : Join ketiganya
online_retail_data_fix = recency %>% left_join(frequency, by = 'CustomerID') %>% left_join(monetary, by = 'CustomerID')

# Step 18 : Tampilkan hanya CustomerID, Recency, Frequency, dan Monetary sesuai soal
online_retail_data_result = online_retail_data_fix %>% select(CustomerID, recency, frequency, monetary)

# Step 19 : Menyimpan hasil dalam bentuk csv 
write.csv(online_retail_data_result, "C:/Users/user/Documents/online_retail_data_result.csv", quote=F, row.names = F)






