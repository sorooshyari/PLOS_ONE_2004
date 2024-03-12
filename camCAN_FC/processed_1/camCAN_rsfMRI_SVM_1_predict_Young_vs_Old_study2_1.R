
#CamCAN rsfMRI monotonicity study - SVM used

num_feat = 326028   #i.e. (808 * 807)/2  CamCAN rsfMRI CC features

#when my_condition == 1: interval 1 (i.e. A) in Manuscript 
#Train one machine on 90 brains, 45 of which comprise the 81+ age group and 45 of which have been randomly selected from the group of 18-30 age group. 
#Use the remaining 561 brains as test data and examine (i.e. plot) which ones are predicted as being young and old by the trained machine. 
#This will allow us to quantify via various means the accuracy of the classification. 
#Obviously the 34 remaining brains from the 18-30 age group should be classified as young, but the bifurcation into 
#young and aged is not so clear with the other 527 brains.

#when my_condition == 2: interval 2 (i.e. B) in Manuscript 
#................

#when my_condition == 3: interval 3 (i.e. C) in Manuscript 
#................

#age groups 
#18-30: 79 
#31-40: 105 
#41-50: 101 
#51-60:  101
#61-70:  103
#71-80:  117
#81+ : 45

set.seed(123)

rm(list = ls())     #to do a clear in r

library(tools)
library(e1071)
library(kernlab)
library(R.matlab)
library(matrixcalc)

printf <- function(...) invisible(print(sprintf(...)))

############################### Read in the data and set up the aggregate "x" matrix and the label-vector "y" (begin) ##############################

my_condition = 1   #train on 45 Y (18to30) and 45 O (81plus), test on 651 - (2*45) = 561 brains
#my_condition = 2   #train on 79 Y (18to30) and 79 O (31to40), test on 651 - (2*79) = 493 brains
#my_condition = 3     #train on 45 Y (71to80) and 45 O (81plus), test on 651 - (2*45) = 561 brains

total_loop1 = 1

idx_of_loop = 1
correct_Aged = 0
correct_Young = 0


while(idx_of_loop <= total_loop1){       # outer-most while-loop for leave-one-out analysis 

#num_feat=2142   #i.e. 306 * 7  all features
#num_feat=1530   #i.e. 306 * 5  stat features
#num_feat=612    #i.e. 306 * 2  deviation-based features
num_feat=326028   #i.e. (306 * 305)/2  CC features

if(my_condition == 1){

num_total = 651
num_Young_total = 79
num_Aged_total = 45

num_for_test = min(num_Young_total, num_Aged_total)

total_indices= 1:num_total
Young_indices = sample.int(num_Young_total, num_Young_total)
Young_indices_for_train = Young_indices[1:45]
Aged_indices_for_train = (num_total - 45 + 1):num_total

train_indices =c(Young_indices_for_train, Aged_indices_for_train)

test_indices = total_indices[!total_indices %in% train_indices]


if(num_feat == 2142){
Name_file1 = read.csv("/.csv")
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:(num_feat + 2), train_indices]       #this is the full set of features
Name_data1_test = Name_file1[3:(num_feat + 2), test_indices]         #this is the full set of features
}

if(num_feat == 1530){
Name_file1 = read.csv("/.csv")
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:(num_feat +2), train_indices]        #stat features
Name_data1_test = Name_file1[3:(num_feat + 2), test_indices]          #stat features
}

if(num_feat == 612){
Name_file1 = read.csv("/.csv")
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:(num_feat + 2), train_indices]        #deviation-based features
Name_data1_test = Name_file1[3:(num_feat + 2), test_indices]          #deviation-based features
}

if(num_feat == 326028){
Name_file1 = read.csv("Your_Path/camCAN_rsfMRI_CCfeatures_per_subjects_rsfMRI_monotonicity_condition_1.csv") 
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:(num_feat + 2), train_indices]        #CC features
Name_data1_test = Name_file1[3:(num_feat + 2), test_indices]          #CC features
}
}

if(my_condition == 2){

num_total = 651
num_Young_total = 79
num_Aged_total = 105

num_for_test = min(num_Young_total, num_Aged_total)

total_indices = 1:num_total
Young_indices = sample.int(num_Young_total, num_Young_total)
Young_indices_for_train = Young_indices[1:num_for_test]

Aged_indices_for_train_pre = num_Young_total + sample.int(num_Aged_total, num_Aged_total)
Aged_indices_for_train = Aged_indices_for_train_pre[1:num_Young_total]

train_indices =c(Young_indices_for_train, Aged_indices_for_train)

test_indices = total_indices[!total_indices %in% train_indices]

if(num_feat == 612){
Name_file1 = read.csv("/.csv")
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:(num_feat + 2), train_indices]        #deviation-based features
Name_data1_test = Name_file1[3:(num_feat + 2), test_indices]          #deviation-based features
}

if(num_feat == 2142){
Name_file1 = read.csv("/.csv")
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:296, train_indices]       #this is the full set of features
Name_data1_test = Name_file1[3:296, test_indices]         #this is the full set of features
}

if(num_feat == 1530){
Name_file1 = read.csv("/.csv")
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:212, train_indices]        #stat features
Name_data1_test = Name_file1[3:212, test_indices]          #stat features
}

if(num_feat == 326028){
Name_file1 = read.csv("Your_Path/camCAN_rsfMRI_CCfeatures_per_subjects_rsfMRI_monotonicity_condition_2.csv") 
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:(num_feat + 2), train_indices]        #CC features
Name_data1_test = Name_file1[3:(num_feat + 2), test_indices]          #CC features
cat("testing1ks \n")
}
}


if(my_condition == 3){

num_total = 651
num_Young_total = 117
num_Aged_total = 45

num_for_test = min(num_Young_total, num_Aged_total)

total_indices= 1:num_total
Young_indices = (num_total - num_Young_total - num_Aged_total) + sample.int(num_Young_total, num_Young_total)
Young_indices_for_train = Young_indices[1:num_Aged_total] 


Aged_indices_for_train = (num_total - num_Aged_total + 1):num_total

train_indices =c(Young_indices_for_train, Aged_indices_for_train)

test_indices = total_indices[!total_indices %in% train_indices]

if(num_feat == 612){
Name_file1 = read.csv("/.csv")
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:(num_feat + 2), train_indices]        #deviation-based features
Name_data1_test = Name_file1[3:(num_feat + 2), test_indices]          #deviation-based features
}

if(num_feat == 1530){
Name_file1 = read.csv("/.csv")
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:212, train_indices]        #stat features
Name_data1_test = Name_file1[3:212, test_indices]          #stat features
}

if(num_feat == 2142){
Name_file1 = read.csv("/.csv")
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:296, train_indices]       #this is the full set of features
Name_data1_test = Name_file1[3:296, test_indices]         #this is the full set of features
}

if(num_feat == 326028){
Name_file1 = read.csv("Your_Path/camCAN_rsfMRI_CCfeatures_per_subjects_rsfMRI_monotonicity_condition_3.csv") 
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:(num_feat + 2), train_indices]        #CC features
Name_data1_test = Name_file1[3:(num_feat + 2), test_indices]          #CC features
}
}

y_label = c(rep(1, num_for_test), rep(-1, num_for_test))

# label = "1" means young
# label = "0/-1" means old

############# Train the SVM with the training data ############# 

svp <- ksvm(t(Name_data1_train), y_label, type="C-svc", kernel='vanilladot', C=60, scaled=c())     #linear kernel

############# Predict labels on test data ############# 

rm(Name_file1, Name_data1_train)   # remove these vars from memory to make space

ypred_all = predict(svp, t(Name_data1_test))

idx_of_loop = idx_of_loop + 1

}        # outer-most while-loop 

#write.csv(ypred_all, file ="hold_SVM_ypred_all_camCAN_rsfMRI_CCfeats_my_condition_1.csv")
#write.csv(test_indices, file ="hold_SVM_test_indices_camCAN_rsfMRI_CCfeats_my_condition_1.csv")
#write.csv(train_indices, file ="hold_SVM_train_indices_camCAN_rsfMRI_CCfeats_my_condition_1.csv")
#write.csv(hold_all_ages[test_indices], file ="hold_SVM_ages_of_tested_camCAN_rsfMRI_CCfeats_my_condition_1.csv")

#write.csv(ypred_all, file ="hold_SVM_ypred_all_camCAN_rsfMRI_CCfeats_my_condition_2.csv")
#write.csv(test_indices, file ="hold_SVM_test_indices_camCAN_rsfMRI_CCfeats_my_condition_2.csv")
#write.csv(train_indices, file ="hold_SVM_train_indices_camCAN_rsfMRI_CCfeats_my_condition_2.csv")
#write.csv(hold_all_ages[test_indices], file ="hold_SVM_ages_of_tested_camCAN_rsfMRI_CCfeats_my_condition_2.csv")

#write.csv(ypred_all, file ="hold_SVM_ypred_all_camCAN_rsfMRI_CCfeats_my_condition_3.csv")
#write.csv(test_indices, file ="hold_SVM_test_indices_camCAN_rsfMRI_CCfeats_my_condition_3.csv")
#write.csv(train_indices, file ="hold_SVM_train_indices_camCAN_rsfMRI_CCfeats_my_condition_3.csv")
#write.csv(hold_all_ages[test_indices], file ="hold_SVM_ages_of_tested_camCAN_rsfMRI_CCfeats_my_condition_3.csv")
