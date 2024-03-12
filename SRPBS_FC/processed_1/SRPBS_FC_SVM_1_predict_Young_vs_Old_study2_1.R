#SRPBS monotonicity - use an SVM
#combined healthy controls from 5 centers:    KyotoU, HiroshimaU, OsakaU, ShowaU, ATR 

num_feat = 9730      #i.e. ((140 ^2) - 140)/2  CC features

#age groups:
#18-30: 327
#31-40:  126
#41-50:  115
#51-60:  69
#61-70:  61
#71+ (71-80): 11
#81+ :  0

#when my_condition == 1:  A aka interval 1 in the Manuscript
#when my_condition == 2:  B aka interval 2 in the Manuscript
#when my_condition == 3:  C aka interval 3 in the Manuscript



set.seed(123)

rm(list = ls())     #to do a clear in r

library(tools)
library(e1071)
library(kernlab)
library(R.matlab)
library(matrixcalc)

printf <- function(...) invisible(print(sprintf(...)))


############################### Read in the data and set up the aggregate "x" matrix and the label-vector "y" (begin) ##############################

my_condition = 1   #train on 72 Y (18to30) and 72 O (61plus), test on 709 - (2*72) = 565 brains
#my_condition = 2   #train on 126 Y (18to30) and 126 O (31to40), test on 709 - (2*126) = 457 brains
#my_condition = 3     #train on 69 Y (51to60) and 69 O (61plus), test on 709 - (2*69) = 571 brains

total_loop1 = 1

idx_of_loop = 1
correct_Aged = 0
correct_Young = 0


while(idx_of_loop <= total_loop1){       # outer-most while-loop for leave-one-out analysis 

num_feat = 9730   #i.e. (140 * 139)/2  CC features


if(my_condition == 1){

num_total = 709
num_Young_total = 327
num_Aged_total = 72

num_for_test = min(num_Young_total, num_Aged_total)

total_indices= 1:num_total
Young_indices = sample.int(num_Young_total, num_Young_total)
Young_indices_for_train = Young_indices[1:72]
Aged_indices_for_train = (num_total - 72 + 1):num_total

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

if(num_feat == 9730){
Name_file1 = read.csv("Your_Path/combined_HC_from_KyotoU_HiroshimaU_OsakaU_ShowaU_ATR_NaN_removed_1_monotonicity_condition_1.csv") 
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:(num_feat + 2), train_indices]        #CC features
Name_data1_test = Name_file1[3:(num_feat + 2), test_indices]          #CC features
}
}


if(my_condition == 2){

num_total = 709
num_Young_total = 327
num_Aged_total = 126

num_for_test = min(num_Young_total, num_Aged_total)

total_indices = 1:num_total
Young_indices = sample.int(num_Young_total, num_Young_total)
Young_indices_for_train = Young_indices[1:num_for_test]

Aged_indices_for_train_pre = num_Young_total + sample.int(num_Aged_total, num_Aged_total)
Aged_indices_for_train = Aged_indices_for_train_pre[1:num_Young_total]

train_indices =c(Young_indices_for_train, Aged_indices_for_train_pre)

test_indices = total_indices[!total_indices %in% train_indices]

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


if(num_feat == 9730){
Name_file1 = read.csv("Your_Path/combined_HC_from_KyotoU_HiroshimaU_OsakaU_ShowaU_ATR_NaN_removed_1_monotonicity_condition_2.csv") 
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:(num_feat + 2), train_indices]        #CC features
Name_data1_test = Name_file1[3:(num_feat + 2), test_indices]          #CC features
}
}


if(my_condition == 3){

num_total = 709
num_Young_total = 69
num_Aged_total = 72

num_for_test = min(num_Young_total, num_Aged_total)

total_indices= 1:num_total
Young_indices = (num_total - num_Young_total - num_Aged_total) + sample.int(num_Young_total, num_Young_total)
Young_indices_for_train = Young_indices[1:num_Aged_total] 

Aged_indices= (num_total - num_Aged_total + 1):num_total
Aged_indices_for_train = Aged_indices[1:num_for_test]

train_indices =c(Young_indices, Aged_indices_for_train)

test_indices = total_indices[!total_indices %in% train_indices]

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

if(num_feat == 9730){
Name_file1 = read.csv("Your_Path/combined_HC_from_KyotoU_HiroshimaU_OsakaU_ShowaU_ATR_NaN_removed_1_monotonicity_condition_3.csv") 
Name_file1 <- Name_file1[, -1]                           # delete column 1 
hold_all_ages <- Name_file1[1,]                           # record row 1 that contains all the subject ages 
Name_file1 <- Name_file1[-1,]                           # delete row 1 that contains all the subject ages 
Name_data1_train = Name_file1[3:(num_feat + 2), train_indices]        #CC features
Name_data1_test = Name_file1[3:(num_feat + 2), test_indices]          #CC features
}
}

y_label = c(rep(1, num_for_test), rep(-1, num_for_test))

############# Train the SVM with the training data ############# 

svp <- ksvm(t(Name_data1_train), y_label, type="C-svc", kernel='vanilladot', C=60, scaled=c())     #linear kernel

############# Predict labels on test data ############# 

ypred_all = predict(svp, t(Name_data1_test))

idx_of_loop = idx_of_loop + 1

}        # outer-most while-loop 

#uncomment the appropriate 3-sum below to write the results to a csv file

#write.csv(ypred_all, file ="hold_SVM_ypred_all_SRPBS_rsfMRI_CCfeats_my_condition_1.csv")
#write.csv(test_indices, file ="hold_SVM_test_indices_SRPBS_rsfMRI_CCfeats_my_condition_1.csv")
#write.csv(train_indices, file ="hold_SVM_train_indices_SRPBS_rsfMRI_CCfeats_my_condition_1.csv")
#write.csv(hold_all_ages[test_indices], file ="hold_SVM_ages_of_tested_SRPBS_rsfMRI_CCfeats_my_condition_1.csv")

#write.csv(ypred_all, file ="hold_SVM_ypred_all_SRPBS_rsfMRI_CCfeats_my_condition_2.csv")
#write.csv(test_indices, file ="hold_SVM_test_indices_SRPBS_rsfMRI_CCfeats_my_condition_2.csv")
#write.csv(train_indices, file ="hold_SVM_train_indices_SRPBS_rsfMRI_CCfeats_my_condition_2.csv")
#write.csv(hold_all_ages[test_indices], file ="hold_SVM_ages_of_tested_SRPBS_rsfMRI_CCfeats_my_condition_2.csv")

#write.csv(ypred_all, file ="hold_SVM_ypred_all_SRPBS_rsfMRI_CCfeats_my_condition_3.csv")
#write.csv(test_indices, file ="hold_SVM_test_indices_SRPBS_rsfMRI_CCfeats_my_condition_3.csv")
#write.csv(train_indices, file ="hold_SVM_train_indices_SRPBS_rsfMRI_CCfeats_my_condition_3.csv")
#write.csv(hold_all_ages[test_indices], file ="hold_SVM_ages_of_tested_SRPBS_rsfMRI_CCfeats_my_condition_3.csv")
