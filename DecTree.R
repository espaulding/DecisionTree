#Author: Eric Spaulding

#to run the script in R GUI once working directory is set
#source("DecTree.R")

#to run the script from commandline
#Rscript DecTree.R

rm(list=ls(all=TRUE))

source("DecisionTreeLib.R")

#alias the X11() function as windows() if os=linux so that script can run in linux as well
if(length(grep("linux",R.Version()$os)) ){
  windows <- function( ... ) X11( ... )
}

#Load the cancer data
trainfile = "train.csv"
testfile  = "test.csv"
train     = set$csvtoset(read.csv(trainfile), classLabelName="class")
test      = set$csvtoset(read.csv(testfile), classLabelName="class")
tree      = decision_tree$buildTree(train)


result    = decision_tree$classify(test,tree,noanswer="vote")
print(paste("The accuracy on the test set from ",testfile," was ",round(result$accuracy,3)*100,"%",sep=""))
print(paste("The number of items correctly classified was        :",result$numcorrect))
print(paste("The number of items incorrectly classified was      :",result$numwrong))
print(paste("The number of items that couldn't be classified was :",result$unclassifiable))

print("")
result    = decision_tree$classify(test,tree,noanswer="ignore")
print(paste("The accuracy on the test set from ",testfile," was ",round(result$accuracy,3)*100,"%",sep=""))
print(paste("The number of items correctly classified was        :",result$numcorrect))
print(paste("The number of items incorrectly classified was      :",result$numwrong))
print(paste("The number of items that couldn't be classified was :",result$unclassifiable))

#print(infogain(train,1))
