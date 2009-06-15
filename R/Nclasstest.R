`Nclasstest` <-
function(T,GS)
{
	 # Using PPV,NPV,Sensitivity and specificity to evaluate the performance of a n-class classification. 
     # GS is golden standard classification result or reference result, could be a matrix or vector with numeric elements
     # T is the testing result, could be a Boolean (gene-gene interaction) or digital (gene-trait interaction) matrix or vector.
     # GS and T should have the same dimensions
    
    # error message 
	if (length(GS)!=length(T))
	{stop("GS and T should have the same dimensions")}
	
	classes.GS <- unique(GS) # Calculate all the classes existed in GS
	
	classes.T <- unique(T) # Calculate all the classes existed in T
	
	classes<- union(classes.GS,classes.T) # calculate all the classes in GS and T
	if(is.na(match(0,classes)))
	{stop("Test result should include negative(0) case")}
	ClassLength<- length(classes) 
	
	PCount <-NA # true predicted size of the classes
	Tref <-NA   # size of the classes in GS
	Ttest <-NA  # size of the classes in T
	
	PV <-NA     # predictive value (precision rate) of all the classes
	S <-NA      # Sensitivity (false rate) of all the classes
	
	for (i in 1:length(classes))
	{
		PCount[i] <-length(intersect(which(T == classes[i]),which(GS == classes[i])))
		Tref[i]<-length(which(GS == classes[i]))
		Ttest[i]<-length(which(T == classes[i]))
	}
	
	for (i in 1:length(classes))
	{
		if(Ttest[i]==0)
		    PV[i]<-0
		else		
		    PV[i] <- PCount[i]/Ttest[i]
		S[i]  <- PCount[i]/Tref[i] 
	}  
	
    ClassPos_0<- which(classes==0)	
	ClassPos_P<- which(classes!=0)
	if(sum(Ttest[ClassPos_P])==0)
	   PPV<-0
	else
	   PPV<- sum(PCount[ClassPos_P])/sum(Ttest[ClassPos_P])
	if(Ttest[ClassPos_0]==0)
	   NPV <-0
	else
	   NPV<- PCount[ClassPos_0]/Ttest[ClassPos_0]
 	
	SE <- sum(PCount[ClassPos_P])/sum(Tref[ClassPos_P])
	SP <- PCount[ClassPos_0]/Tref[ClassPos_0]

    binary.performance<-t(c(PPV, NPV, SE, SP))
    colnames(binary.performance)<-c("PPV","NPV","Se","Sp")
    
   SortClass<-sort(classes,index.return=TRUE)
	ClassNum<-SortClass$x
	PV<- PV[SortClass$ix]
	S <- S[SortClass$ix]
   multi.performance<-cbind(ClassNum, PV,S)
		
	rownames(multi.performance)<-1:length(classes)

	colnames(multi.performance)<-c("class.type","PV","S")
	if(length(classes)==2)
		return(list(binary.performance=binary.performance))
	else
	    return(list(multi.performance = multi.performance,binary.performance=binary.performance))
}

