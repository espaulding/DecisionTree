#Author: Eric Spaulding

decision_tree = list()
set = list()

#receive a table from read.csv and wrap the result in a conceptual set format
#if a valid classLabelName is provided dataColumns and classLabelColumn will be superceded
#classLabelName is case sensitive
set$csvtoset = function(csvData, dataColumns=NULL, classLabelColumn=NULL, classLabelName=NULL){
    newset = list()
    if(!is.null(dataColumns)){ newset$data=csvData[,dataColumns]; }
    if(!is.null(classLabelColumn)){ newset$class=csvData[,classLabelColumn]; }

    classLabelColumn = which(colnames(csvData) == classLabelName)
    if(length(classLabelColumn) == 1){ #length of 0 or more than 1 indicates an unuseable index
	dataColumns = 1:ncol(csvData)
	dataColumns = dataColumns[-classLabelColumn]
	newset$data=csvData[,dataColumns]
	newset$class=csvData[,classLabelColumn]
    }
    return(newset)
}

#Given a set object remove the designated rows or columns from the set's data matrix and class vector
set$subset = function(localset, rowsToRemove=NULL, colsToRemove=NULL){
    ss = localset
    if(!is.null(rowsToRemove)){ 
	ss$data  = ss$data[-rowsToRemove,]
	ss$class = ss$class[-rowsToRemove]
    }
    if(!is.null(colsToRemove)){ 
	ss$data  = ss$data[,-colsToRemove]
    }
    return(ss)
}

#Use a decision tree previously built to classify a set of test data
#set       : Must be in the expected set format
#set$data  : must contain a matrix of discrete values similiar to the values
#            seen in the training set used to build the tree. Rows must be
#            samples (test instances) and columns must be dimensions (attributes)
#set$class : is optional as far as actual classification goes, but the classifier
#            requires class lables for each sample in order to report the trees
#            accuracy, number correctly classified, number incorrectly classified,
#            and number that could not be classified
#noanswer  : The default setting is "vote", which allows the tree to choose the 
#            most common class present at a node as the answer when the tree does
#            not have a decision branch to match the sample its attempting to classify.
#            The other method available is "ignore", which causes the classifier
#            to report unclassifiable for these samples.
#            Unclassifiable samples are added to the number incorrectly classified
#            as far as the accuracy calculations are concerned
decision_tree$classify = function(set, tree, noanswer="vote"){
    answers  = apply(set$data,1,function(r) decision_tree$classifyInstance(r,tree,noanswer=noanswer))
    accuracy = NaN; if(!is.null(set$class)){ accuracy = set$class == answers; }
    result   = list(answers=answers,accuracy=mean(accuracy))
    result$numcorrect     = sum(accuracy)
    result$numwrong       = length(accuracy) - result$numcorrect
    result$unclassifiable = sum(answers == "unclassifiable")
    return(result)
}

#Use a decision tree previously built to classify a single sample (test instance)
#sample   : A single row of a set as defined by the DecisionTreeLib
#tree     : A tree object built by the buildTree function
#noanswer : See the decription in the classify function
decision_tree$classifyInstance = function(sample, tree, noanswer="vote"){
    answer = NULL
    node = tree
    while(is.null(answer)){
    	if(node$type != "leaf"){
            value       = as.numeric(sample[node$dim])
            branchindex = which(node$decision == value)
            if(length(branchindex) == 1){ #follow the branch
                node = node$branch[[branchindex]]
            } else { #no branch to follow for this sample
                if(noanswer=="vote"){ answer = node$common; }
                    else                { answer = "unclassifiable"; }
                }
        } else { #we're at a leaf node so collect its answer
            answer = node$answer
        }
    }
    return(answer)
}

#Wrapper for the recursive ID3 function
decision_tree$buildTree = function(train){
    return(decision_tree$ID3(train, 1:ncol(train$data)))
}

#Iterative Dichotomizer 3 algorithm
#ID3(S, attributes yet to be processed)
#	Create a Root node for the tree
#	Base cases
#		If S are all same class, return the single node tree root with that label
#		If attributes is empty return r node with label equal to most common class
#	Otherwise
#		Find attribute with greatest information gain
#		Set decision attribute for root 
#		For each value of the chosen attribute
#			Add a new branch below root
#			Determine Sv for that value
#			If Sv is empty 
#               #this adds nothing to the algorithm if classify(noanswer="vote")
#				Add a leaf with label of most common class 
#			Else
#				Add subtree to this branch: ID3(Sv, attributes – this attribute)
decision_tree$ID3 = function(nodedata, dims){
    root = list(type="leaf",label="class")
    counts = as.matrix(table(nodedata$class)) #count the number of set items from each class
    root$common = rownames(counts)[which(counts == max(counts))][1]

    #base cases
    classes = levels(as.factor(nodedata$class))
    if(length(classes)==1 || length(dims)==0){ 
	root$answer = root$common
	return(root); 
    }

    #main algorithm
    root$type="node"
    #find the dimension(attribute) with greatest information gain
    infogainByDim   = sapply(c(1:length(dims)), function(d) decision_tree$infogain(nodedata,d))
    indexBestDim    = order(infogainByDim, decreasing=TRUE)[1]
    root$label      = colnames(nodedata$data)[indexBestDim]
    root$dim        = dims[indexBestDim]

    #identify branches leaving this node (1 for each discrete data value)
    root$decision   = as.vector(levels(as.factor(nodedata$data[,root$label])))
    datarows        = 1:nrow(nodedata$data)
    subsetRows      = sapply(root$decision, function(v) which(v==nodedata$data[,indexBestDim]))
	
    #generate a new node(subtree or leaf) to attach for each branch
    root$branch = list()
    for(v in 1:length(root$decision)){
    	subset = set$subset(nodedata, rowsToRemove=datarows[-subsetRows[[v]]], colsToRemove=indexBestDim)
    	if(nrow(subset$data) != 0){ root$branch[[v]] = decision_tree$ID3(subset,dims[-indexBestDim]); }
    	else { root$branch[[v]] = list(type="leaf",label="class",answer=root$common) }
    }
    return(root)
}

#take a set of class labels and determine the entropy of the set by proportions of each class in the set.
#all 1 class in the set leads to 0 entropy (smallest entropy)
#even distribution between multiple classes leads to 1 entropy (maximum entropy)
decision_tree$entropy = function(c){
    entropybyclass = function(p){ 
        if(p == 0){ return(0) } 
	return(-p*log2(p)) 
    }
    e = sapply(levels(as.factor(c)), function(x) entropybyclass(length(which(c == x)) / length(c)))
    return(sum(e))
}

#function that takes a set and the column index of one dimension in that set
#first, the function will determine all possible discrete values in the set 
#       for the given dimension
#then, then the function will determine the entropy of each subset generated 
#      by filtering the set by these discrete values
#next, the entropy of each subset will be weighted by subset size with respect
#      to the size of the original set
#finally, these weighted subset entropies will be summed and compared to the
#         entropy of the original set using the following formula
#infogain = entropy(set) - sum(weight_v*entropy(set_v))
#in this case set must always be a list() containing the following items
#   data  : this must be a matrix of data with samples as rows and dimensions(attributes) as columns
#   class : this must be a vector with 1 entry for each row of the matrix denoting that sample's class
decision_tree$infogain = function(nodedata,dim){
    weight_entropy = function(nd,sr){ 
        weight  = length(nd$class[sr])/nrow(nd$data)
        entropy = decision_tree$entropy(nd$class[sr])
        return(weight*entropy)
    }
    values        = levels(as.factor(nodedata$data[,dim])) #what are the discrete values for the dimension
    subsetRows    = sapply(values, function(v) which(v==nodedata$data[,dim]))
    upper_entropy = decision_tree$entropy(nodedata$class)
    lower_entropy = sum(sapply(subsetRows, function(s) weight_entropy(nodedata,s)))
    return(upper_entropy - lower_entropy)
}
