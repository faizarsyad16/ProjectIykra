# Title       : Machine Learning II
# Description : Classification Model using 'Random Forest' and 'Decision Tree' on HR use case
# Dataset     : https://github.com/arikunco/machinelearning/blob/master/dataset/HR_comma_sep.csv
# Author      : Faiz Farhansyah Arsyad

# Step 1 : Menginstall packages yang relevan
#install.packages("rpart") --> Mengeluarkan command classification decision tree
#install.packages("randomForest") --> Mengeluarkan command classification random forest
  
# Step 2 : Load library
library(rpart)
library(randomForest)

# Step 3 : Membaca Dataset
hr_dataset = read.csv('https://raw.githubusercontent.com/arikunco/machinelearning/master/dataset/HR_comma_sep.csv')

# Step 4 : Menunjukan 6 rows pertama dari dataset (variable checking)
head(hr_dataset)

# Step 5 : Menunjukan 6 rows terakhir dari dataset (variable checking)
tail(hr_dataset)

# Step 6 : Menjalankan fungsi str pada data untuk membaca jenis data dari masing-masing field 
#perlu diketahui bahwa klasifikasi bisa dilakukan dengan jenis data 'Factor'
str(hr_dataset)


# Step 7 : Split kolom left sesuai dengan soal. Left di split menjadi train dan test data, dengan sampel pada traindata 70%
hr_dataset$left <- as.factor(hr_dataset$left)
train <- sample(1:nrow(hr_dataset),as.integer(0.7*nrow(hr_dataset)))
traindata <- hr_dataset[train,]
testdata <- hr_dataset[-train,]

# Step 8 : Menjalankan metode Decision Tree 
tree <- rpart(left ~ ., 
              data = (traindata), method = "class")
predict(tree, data.frame(testdata),type="class" )
conf_decisionTree <- table(testdata[,'left'],predict(tree, data.frame(testdata),type="class"))


# Step 9 : Menjalankan metode Random forest 
randomFor <- randomForest(left ~ ., data = data.frame(traindata), ntree=100, importance = TRUE)
predict(randomFor, data.frame(testdata),type="class")
conf_randomFor <- table(testdata[,'left'],predict(randomFor, data.frame(testdata), type="class"))

# Step 10 : Meng-Assign TP, FN, FP and TN dari conf table decision tree
TP_DT <- conf_decisionTree[1, 1] 
FN_DT <- conf_decisionTree[1, 2] 
FP_DT <- conf_decisionTree[2,1] 
TN_DT <- conf_decisionTree[2,2] 

# Step 11 : Meng-Assign TP, FN, FP and TN dari conf table random forest
TP_RF <- conf_randomFor[1, 1] 
FN_RF <- conf_randomFor[1, 2] 
FP_RF <- conf_randomFor[2,1] 
TN_RF <- conf_randomFor[2,2] 

# Step 12 : Menghitung akurasi decision tree
acc_DT <- (TP_DT+TN_DT)/(TP_DT+FN_DT+FP_DT+TN_DT)
#acc_DT --> 0.96

# Step 13 : Menghitung akurasi random forest
acc_RF <- (TP_RF+TN_RF)/(TP_RF+FN_RF+FP_RF+TN_RF)
acc_RF
#acc_RF --> 0.97

# Step 14 : Menghitung presisi decision tree
prec_DT <- TP_DT / (TP_DT+FP_DT)
#prec_DT -->0.97

# Step 15 : Menghitung presisi random forest
prec_RF <- TP_RF / (TP_RF+FP_RF)
prec_RF
#prec_RF -->0.97

# Step 16 : Menghitung recall decision tree
rec_DT <- TP_DT / (TP_DT+FN_DT)
#rec_DT -->0.98 

# Step 17 : Menghitung recall random forest
rec_RF <- TP_RF / (TP_RF+FN_RF)
#rec_DT -->0.99


#Konklusi --> Menilik dari data akurasi presisi dan recall, metode random forest lebih baik digunakan dibandingkan decision tree karena memiliki nilai lebih tinggi