Q).Problem- prediction of the number of comments in the upcoming 24 hours on
those blogs, The train data was generated from different base times that may
temporally overlap. Therefore, if you simply split the train into disjoint partitions,
the underlying time intervals may overlap. Therefore, the you should use the
provided, temporally disjoint train and test splits to ensure that the evaluation is
fair.

a. Read the dataset and identify the right features
b. Clean dataset, impute missing values and perform exploratory data analysis.
c. Visualize the dataset and make inferences from that
d. Perform any 3 hypothesis tests using columns of your choice, make conclusions
e. Create a linear regression model to predict the number of comments in the next 24 hours
(relative to basetime)
f. Fine tune the model and represent important features
g. Interpret the summary of the linear model
h. Report the test accuracy vs. the training accuracy
i. Interpret the final model coefficients
j. Plot the model result and compare it with assumptions of the model

setwd("F:/AcadGild/workings")

lib=c("bigmemory", "readr", "Hmisc", "dplyr", "MASS", "ggplot2", "lattice", "caret", "rpart", 
      "randomForest", "rpart.plot","lattice", "rattle", "data.table","RColorBrewer", "reshape2",
      "InformationValue","stringr", "VIF", "Information", "Amelia", "gdata", "party","car","lubridate",
      "zoo", "sqldf", "fuzzyjoin", "party", "mice", "tseries", "timeSeries","forecast","tidyverse")
sapply(lib, require, character.only=TRUE, quietly=TRUE)


# import train data set
Train_blog<-fread("F:/AcadGild/workings/BlogFeedback/blogData_train.csv", header=FALSE)

# Assign variable names to the train data set
colnames(Train_blog)<-c("Average", "standard deviation", "min", "max","median","Average", "standard deviation", "min", "max","median",
                        "Average", "standard deviation", "min", "max","median","Average", "standard deviation", "min", "max","median",
                        "Average", "standard deviation", "min", "max","median","Average", "standard deviation", "min", "max","median",
                        "Average", "standard deviation", "min", "max","median","Average", "standard deviation", "min", "max","median",
                        "Average", "standard deviation", "min", "max","median","Average", "standard deviation", "min", "max","median",
                        "fbbeforebase", "fbrelativeto base","fbT1toT2","fb 1st 24hrs","fbdif52and53","tbbeforebase", "tbrelativeto base","tbT1toT2",
                        "tb 1st 24hrs","tbdif52and53","length of time", "length of pb","text1","text2","text3","text4","text5",
                        "text6","text7","text8","text9","text10","text11","text12","text13","text14","text15","text16","text17",
                        "text18","text19","text20","text21","text22","text23","text24","text25","text26","text27","text28",
                        "text29","text30","text31","text32","text33","text34","text35","text36","text37","text38","text39","text40",
                        "text41","text42","text43","text44","text45","text46","text47","text48","text49","text50","text51","text52",
                        "text53","text54","text55","text56","text57","text58","text59","text60","text61","text62","text63","text64",
                        "text65","text66","text67","text68","text69","text70","text71","text72","text73","text74","text75","text76",
                        "text77","text78","text79","text80","text81","text82","text83","text84","text85","text86","text87","text88",
                        "text89","text90","text91","text92","text93","text94","text95","text96","text97","text98","text99","text100",
                        "text101","text102","text103","text104","text105","text106","text107","text108","text109","text110","text111","text112",
                        "text113","text114","text115","text116","text117","text118","text119","text120","text121","text122","text123","text124",
                        "text125","text126","text127","text128","text129","text130","text131","text132","text133","text134","text135","text136",
                        "text137","text138","text139","text140","text141","text142","text143","text144","text145","text146","text147","text148",
                        "text149","text150","text151","text152","text153","text154","text155","text156","text157","text158","text159","text160",
                        "text161","text162","text163","text164","text165","text166","text167","text168","text169","text170","text171","text172",
                        "text173","text174","text175","text176","text177","text178","text179","text180","text181","text182","text183","text184",
                        "text185","text186","text187","text188","text189","text190","text191","text192","text193","text194","text195","text196",
                        "text197","text198","text199","text200","monday","tuesday","wednesday","thursday","friday","saturday","sunday",
                        "monday bpost","tuesday bpost","wednesday bpost","thursday bpost","friday bpost","saturday bpost","sunday bpost",
                        "B post P", "minimum fb P", "max fb P","average fb P", "fbtarget")
summary(Train_blog)
dim(Train_blog)
str(Train_blog)
class(Train_blog)
describe(Train_blog)
distinct(Train_blog)
head(Train_blog)
Train_blog<-Train_blog[!duplicated(Train_blog),]

# import test data set
Test_01<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.01.00_00.csv", header=FALSE)
Test_02<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.02.00_00.csv", header=FALSE)
Test_03<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.03.00_00.csv", header=FALSE)
Test_04<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.04.00_00.csv", header=FALSE)
Test_05<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.05.00_00.csv", header=FALSE)
Test_06<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.06.00_00.csv", header=FALSE)
Test_07<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.07.00_00.csv", header=FALSE)
Test_08<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.08.00_00.csv", header=FALSE)
Test_09<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.09.00_00.csv", header=FALSE)
Test_10<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.10.00_00.csv", header=FALSE)
Test_11<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.11.00_00.csv", header=FALSE)
Test_12<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.12.00_00.csv", header=FALSE)
Test_13<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.13.00_00.csv", header=FALSE)
Test_14<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.14.00_00.csv", header=FALSE)
Test_15<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.15.00_00.csv", header=FALSE)
Test_16<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.16.00_00.csv", header=FALSE)
Test_17<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.17.00_00.csv", header=FALSE)
Test_18<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.18.00_00.csv", header=FALSE)
Test_19<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.19.00_00.csv", header=FALSE)
Test_20<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.20.00_00.csv", header=FALSE)
Test_21<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.21.00_00.csv", header=FALSE)
Test_22<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.22.00_00.csv", header=FALSE)
Test_23<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.23.00_00.csv", header=FALSE)
Test_24<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.24.00_00.csv", header=FALSE)
Test_25<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.25.00_00.csv", header=FALSE)
Test_26<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.26.00_00.csv", header=FALSE)
Test_27<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.27.00_00.csv", header=FALSE)
Test_28<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.28.00_00.csv", header=FALSE)
Test_29<-fread("F:/AcadGild/workings/BlogFeedback/blogData_test-2012.02.29.00_00.csv", header=FALSE)

Test_all<-rbind(Test_01,Test_02,Test_03,Test_04,Test_05,Test_06,Test_07,Test_08,Test_09,Test_10,
                Test_11,Test_12,Test_13,Test_14,Test_15,Test_16,Test_17,Test_18,Test_19,Test_20,
                Test_21,Test_22,Test_23,Test_24,Test_25,Test_26,Test_27,Test_28,Test_29)

# Assign variable names to the test data set
colnames(Test_all)<-c("Average", "standard deviation", "min", "max","median","Average", "standard deviation", "min", "max","median",
                      "Average", "standard deviation", "min", "max","median","Average", "standard deviation", "min", "max","median",
                      "Average", "standard deviation", "min", "max","median","Average", "standard deviation", "min", "max","median",
                      "Average", "standard deviation", "min", "max","median","Average", "standard deviation", "min", "max","median",
                      "Average", "standard deviation", "min", "max","median","Average", "standard deviation", "min", "max","median",
                      "fbbeforebase", "fbrelativeto base","fbT1toT2","fb 1st 24hrs","fbdif52and53","tbbeforebase", "tbrelativeto base","tbT1toT2",
                      "tb 1st 24hrs","tbdif52and53","length of time", "length of pb","text1","text2","text3","text4","text5",
                      "text6","text7","text8","text9","text10","text11","text12","text13","text14","text15","text16","text17",
                      "text18","text19","text20","text21","text22","text23","text24","text25","text26","text27","text28",
                      "text29","text30","text31","text32","text33","text34","text35","text36","text37","text38","text39","text40",
                      "text41","text42","text43","text44","text45","text46","text47","text48","text49","text50","text51","text52",
                      "text53","text54","text55","text56","text57","text58","text59","text60","text61","text62","text63","text64",
                      "text65","text66","text67","text68","text69","text70","text71","text72","text73","text74","text75","text76",
                      "text77","text78","text79","text80","text81","text82","text83","text84","text85","text86","text87","text88",
                      "text89","text90","text91","text92","text93","text94","text95","text96","text97","text98","text99","text100",
                      "text101","text102","text103","text104","text105","text106","text107","text108","text109","text110","text111","text112",
                      "text113","text114","text115","text116","text117","text118","text119","text120","text121","text122","text123","text124",
                      "text125","text126","text127","text128","text129","text130","text131","text132","text133","text134","text135","text136",
                      "text137","text138","text139","text140","text141","text142","text143","text144","text145","text146","text147","text148",
                      "text149","text150","text151","text152","text153","text154","text155","text156","text157","text158","text159","text160",
                      "text161","text162","text163","text164","text165","text166","text167","text168","text169","text170","text171","text172",
                      "text173","text174","text175","text176","text177","text178","text179","text180","text181","text182","text183","text184",
                      "text185","text186","text187","text188","text189","text190","text191","text192","text193","text194","text195","text196",
                      "text197","text198","text199","text200","monday","tuesday","wednesday","thursday","friday","saturday","sunday",
                      "monday bpost","tuesday bpost","wednesday bpost","thursday bpost","friday bpost","saturday bpost","sunday bpost",
                      "B post P", "minimum fb P", "max fb P","average fb P", "fbtarget")

summary(Test_all)
dim(Test_all)
str(Test_all)
class(Test_all)
describe(Test_all)
Test_all<-Test_all[!duplicated(Test_all),]
head(Test_all)

# missing values imputation 
md.pattern(Test_all) # no missing values 
colSums(is.na(Test_all))
sapply(Test_all, function(x) sum(is.na(x)))
missmap(Test_all)

colSums(is.na(Train_blog))
md.pattern(Train_blog) #no missing values
missmap(Train_blog)

library(tree)
library(C50)

model<-tree(Train_blog$fbtarget~.,data = Train_blog) # tree based model for non linear complex data
model
summary(model)

model1<-lm(Train_blog$fbtarget~., data = Train_blog)
model1
summary(model1)

predtree<-predict(model1,Test_all)
predtree



