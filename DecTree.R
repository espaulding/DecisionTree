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

# SETTINGS
numbins       = 5 #if this is NULL no binning will be done
crossvalidate = TRUE
folds         = 10
printtree     = FALSE

#Load the cancer data
#trainfile = "train.csv"; testfile  = "test.csv"; classlabel = "class"
trainfile = "fruit.csv" ;testfile  = "testFruit.csv"; classlabel = "Class"
train     = set$csvtoset(read.csv(trainfile), classLabelName=classlabel)
test      = set$csvtoset(read.csv(testfile), classLabelName=classlabel)
full      = set$add(train,test)

print("")

if(!is.null(numbins)){
    result    = set$binsetdata(train, numbins=numbins)
    train     = result$newset
    test      = set$binsetdata(test, numbins=numbins, max=result$max, min=result$min)$newset
    full      = set$binsetdata(full, numbins=numbins)$newset
}

if(!crossvalidate){
    print("building tree...")
    tree      = decision_tree$buildTree(train)

    if(printtree){
        print("tree hierarchy")
        decision_tree$print(tree,1,traversal="BFS")
    }
}

if(!is.null(numbins)){
    print(paste("The tree used data put into",numbins,"bins"))
}

if(crossvalidate){
    print(paste("building trees and doing",folds,"fold crossvaldiation with common class voting"))
    print("")
    result = decision_tree$crossvalidation(full,folds,noanswer="vote")
    print(paste("The accuracy on the full set ",round(result$accuracy,3)*100,"%",sep=""))
    print(paste("The number of items correctly classified was        :",result$numcorrect))
    print(paste("The number of items incorrectly classified was      :",result$numwrong))
    print(paste("The number of items that couldn't be classified was :",result$unclassifiable))

    print(paste("building trees and doing",folds,"fold crossvaldiation without common class voting"))
    print("")
    result = decision_tree$crossvalidation(full,folds,noanswer="ignore")
    print(paste("The accuracy on the full set ",round(result$accuracy,3)*100,"%",sep=""))
    print(paste("The number of items correctly classified was        :",result$numcorrect))
    print(paste("The number of items incorrectly classified was      :",result$numwrong))
    print(paste("The number of items that couldn't be classified was :",result$unclassifiable))
} else {
    result = decision_tree$classify(test,tree,noanswer="vote")
    print(paste("The accuracy on the test set from ",testfile," was ",round(result$accuracy,3)*100,"%",sep=""))
    print(paste("The number of items correctly classified was        :",result$numcorrect))
    print(paste("The number of items incorrectly classified was      :",result$numwrong))
    print(paste("The number of items that couldn't be classified was :",result$unclassifiable))
    
    print("")
    result = decision_tree$classify(test,tree,noanswer="ignore")
    print(paste("The accuracy on the test set from ",testfile," was ",round(result$accuracy,3)*100,"%",sep=""))
    print(paste("The number of items correctly classified was        :",result$numcorrect))
    print(paste("The number of items incorrectly classified was      :",result$numwrong))
    print(paste("The number of items that couldn't be classified was :",result$unclassifiable))
    print("")
    print("")
    result = decision_tree$classify(train,tree,noanswer="vote")
    print(paste("The accuracy on the test set from ",trainfile," was ",round(result$accuracy,3)*100,"%",sep=""))
    print(paste("The number of items correctly classified was        :",result$numcorrect))
    print(paste("The number of items incorrectly classified was      :",result$numwrong))
    print(paste("The number of items that couldn't be classified was :",result$unclassifiable))
    
    print("")
    result = decision_tree$classify(train,tree,noanswer="ignore")
    print(paste("The accuracy on the test set from ",trainfile," was ",round(result$accuracy,3)*100,"%",sep=""))
    print(paste("The number of items correctly classified was        :",result$numcorrect))
    print(paste("The number of items incorrectly classified was      :",result$numwrong))
    print(paste("The number of items that couldn't be classified was :",result$unclassifiable))
}

#print(infogain(train,1))
