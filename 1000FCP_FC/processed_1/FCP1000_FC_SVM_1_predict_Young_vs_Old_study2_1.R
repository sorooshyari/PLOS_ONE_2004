
#when my_condition == 1: (i.e. A) in Manuscript)
#Train one machine on 78 brains, 39 of which comprise the 71+ age group and 39 of which have been randomly selected from the group of 21-30 age group. 
#Use the remaining 809 brans as test data and examine (i.e. plot) which ones are predicted as being young and old by the trained machine. 
#This will allow us to quantify via various means the accuracy of the classification. 
#Obviously the 419 remaining brains from the 21-30 age group should be classified as young, but the bifurcation into 
#young and aged is not so clear with the other 390 brains.

#when my_condition == 2: (i.e. B) in Manuscript)
#Train on 170 brains, 85 of which comprise the 31-40 age group and 85 of which have been randomly selected from the 21-30 age group. 
#Use the remaining 717 brans as test data and examine (i.e. plot) which ones are predicted as being young and old by the trained machine. 
#Obviously, the 373 remaining brains from the 21-30 age group should be classified as young, but the bifurcation into 
#young and aged is not so clear with the other 344 brains.

#when my_condition == 3: (i.e. C) in Manuscript)
#Train one machine on 78 brains, 39 of which comprise the 71+ age group and 39 of which have been randomly selected from the group of 61-70 age group. 
#Use the remaining 809 brans as test data and examine (i.e. plot) which ones are predicted as being young and old by the trained machine. 
#Obviously the 28 remaining brains from the 61-70 age group should be classified as young, but the bifurcation into 
#young and aged is not so clear with the other 781 brains.


#age groups:
#21-30: 458 
#31-40: 85 
#41-50: 119 
#51-60: 119 
#61-70: 67 
#(71+ : 39)
#71-80: 31 
#81+ : 8 

set.seed(123)

rm(list = ls())     #to do a clear in r

library(tools)
library(e1071)
library(kernlab)
library(R.matlab)
library(matrixcalc)

printf <- function(...) invisible(print(sprintf(...)))

############################### Read in the data and set up the aggregate "x" matrix and the label-vector "y" (begin) ##############################

my_condition = 1   #train on 39 Y (21to30) and 39 O (71plus), test on 887 - (2*39) = 809 brains
#my_condition = 2   #train on 85 Y (21to30) and 85 O (31-40), test on 887 - (2*85) = 717 brains
#my_condition = 3   #train on 39 Y (61to70) and 39 O (71plus), test on 887 - (2*39) = 809 brains

total_loop1 = 1

idx_of_loop = 1
correct_Aged = 0
correct_Young = 0


while(idx_of_loop <= total_loop1){       # outer-most while-loop for leave-one-out analysis

#num_feat=294   #i.e. 42 * 7  all features
#num_feat=210   #i.e. 42 * 5  stat features
#num_feat=84    #i.e. 42 * 2  deviation-based features
num_feat=861   #i.e. ((42 ^2) - 42)/2  CC features

if(my_condition == 1){

num_total = 887
num_Young_total = 458
num_Aged_total = 39

num_for_test = min(num_Young_total, num_Aged_total)

total_indices= 1:num_total
Young_indices = sample.int(num_Young_total, num_Young_total)
Young_indices_for_train = Young_indices[1:39]
Aged_indices_for_train = (num_total - 39 + 1):num_total

train_indices =c(Young_indices_for_train, Aged_indices_for_train)

test_indices = total_indices[!total_indices %in% train_indices]

if(num_feat == 294){
Name_file1 = read.csv("Your_Path/Name_age_sorted_test1_NoLabels_study2.csv")
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:296, train_indices]       #this is the full set of features
Name_data1_test = Name_file1[3:296, test_indices]         #this is the full set of features
}

if(num_feat == 210){
Name_file1 = read.csv("Your_Path/Name_age_sorted_test1_NoLabels_study2_StatFeatures.csv")
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:212, train_indices]        #stat features
Name_data1_test = Name_file1[3:212, test_indices]          #stat features
}

if(num_feat == 84){
Name_file1 = read.csv("Your_Path/Name_age_sorted_test1_NoLabels_study2_DevFeatures.csv")
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:86, train_indices]        #deviation-based features
Name_data1_test = Name_file1[3:86, test_indices]          #deviation-based features
}

if(num_feat == 861){
Name_file1 = read.csv("Your_Path/Name_age_sorted_test1_NoLabels_study2_CCFeatures.csv") 
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:863, train_indices]        #deviation-based features
Name_data1_test = Name_file1[3:863, test_indices]          #deviation-based features
}
}


if(my_condition == 2){

num_total = 887
num_Young_total = 458
num_Aged_total = 85

num_for_test = min(num_Young_total, num_Aged_total)

total_indices = 1:num_total
Young_indices = sample.int(num_Young_total, num_Young_total)
Young_indices_for_train = Young_indices[1:85]
Aged_indices_for_train = (458 + 1):(458 + 85)


train_indices =c(Young_indices_for_train, Aged_indices_for_train)

test_indices = total_indices[!total_indices %in% train_indices]

if(num_feat == 294){
Name_file1 = read.csv("Your_Path/Name_age_sorted_test1_NoLabels_study2.csv")
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:296, train_indices]       #this is the full set of features
Name_data1_test = Name_file1[3:296, test_indices]         #this is the full set of features
}

if(num_feat == 210){
Name_file1 = read.csv("Your_Path/Name_age_sorted_test1_NoLabels_study2_StatFeatures.csv")
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:212, train_indices]        #stat features
Name_data1_test = Name_file1[3:212, test_indices]          #stat features
}

if(num_feat == 84){
Name_file1 = read.csv("Your_Path/Name_age_sorted_test1_NoLabels_study2_DevFeatures.csv")
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:86, train_indices]        #deviation-based features
Name_data1_test = Name_file1[3:86, test_indices]          #deviation-based features
}

if(num_feat == 861){
Name_file1 = read.csv("Your_Path/Name_age_sorted_test1_NoLabels_study2_CCFeatures.csv") 
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:863, train_indices]        #deviation-based features
Name_data1_test = Name_file1[3:863, test_indices]          #deviation-based features
}
}


if(my_condition == 3){

num_total = 887
num_Young_total = 67
num_Aged_total = 39

num_for_test = min(num_Young_total, num_Aged_total)

total_indices= 1:num_total
Young_indices = (num_total - num_Young_total - num_Aged_total) + sample.int(num_Young_total, num_Young_total)
Young_indices_for_train = Young_indices[1:39]
Aged_indices_for_train = (num_total - 39 + 1):num_total

train_indices =c(Young_indices_for_train, Aged_indices_for_train)

test_indices = total_indices[!total_indices %in% train_indices]

if(num_feat == 294){
Name_file1 = read.csv("Your_Path/Name_age_sorted_test1_NoLabels_study2.csv")
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:296, train_indices]       #this is the full set of features
Name_data1_test = Name_file1[3:296, test_indices]         #this is the full set of features
}

if(num_feat == 210){
Name_file1 = read.csv("Your_Path/Name_age_sorted_test1_NoLabels_study2_StatFeatures.csv")
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:212, train_indices]        #stat features
Name_data1_test = Name_file1[3:212, test_indices]          #stat features
}

if(num_feat == 84){
Name_file1 = read.csv("Your_Path/Name_age_sorted_test1_NoLabels_study2_DevFeatures.csv")
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:86, train_indices]        #deviation-based features
Name_data1_test = Name_file1[3:86, test_indices]          #deviation-based features
}

if(num_feat == 861){
Name_file1 = read.csv("Your_Path/Name_age_sorted_test1_NoLabels_study2_CCFeatures.csv") 
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:863, train_indices]        #deviation-based features
Name_data1_test = Name_file1[3:863, test_indices]          #deviation-based features
}
}

y_label = c(rep(1, num_for_test), rep(-1, num_for_test))

# label = "1" means young
# label = "0/-1" means old

############# Train the SVM with the training data ############# 

svp <- ksvm(t(Name_data1_train), y_label, type="C-svc", kernel='vanilladot', C=60, scaled=c())     #linear kernel

############# Predict labels on test data ############# 

ypred_all = predict(svp, t(Name_data1_test))

idx_of_loop = idx_of_loop + 1

}        #outer-most while-loop 

#uncomment the appropriate 4-sum below to write the results to csv files

#write.csv(ypred_all, file ="hold_ypred_all_my_condition_1.csv")
#write.csv(test_indices, file ="hold_test_indices_my_condition_1.csv")
#write.csv(train_indices, file ="hold_train_indices_my_condition_1.csv")
#write.csv(hold_all_ages[test_indices], file ="hold_ages_of_tested_my_condition_1.csv")

#write.csv(ypred_all, file ="hold_ypred_all_my_condition_2.csv")
#write.csv(test_indices, file ="hold_test_indices_my_condition_2.csv")
#write.csv(train_indices, file ="hold_train_indices_my_condition_2.csv")
#write.csv(hold_all_ages[test_indices], file ="hold_ages_of_tested_my_condition_2.csv")

#write.csv(ypred_all, file ="hold_ypred_all_my_condition_3.csv")
#write.csv(test_indices, file ="hold_test_indices_my_condition_3.csv")
#write.csv(train_indices, file ="hold_train_indices_my_condition_3.csv")
#write.csv(hold_all_ages[test_indices], file ="hold_ages_of_tested_my_condition_3.csv")
