\name{Nclasstest}
\alias{Nclasstest}
\title{ Multi-class Statistical Scores }
\description{
 This function allows the computation of statistical scores for the evaluation of the performances of a n-ary classification test. In fact, it extends the concepts of Sensitivity (Se), Specificity (Sp), Positive Predictive Value (PPV) and Negative Predictive Value (NPV) -defined for binary tests- to any number of classes. 
}
\usage{
Nclasstest(T, GS)
}

\arguments{
  \item{T}{ A matrix or vector from testing results, wherein the element represents the class type. }
  \item{GS}{ A matrix or vector from Gold Standard results, wherein the element represents the class type. }
}
\details{
  Specificity, sensitivity, negative and positive predictive value are used in combination to quantify different aspects of the accuracy of a binary test, evaluating different proportions of correctly and incorrectly classified items, when compared to a known classification, considered the gold standard. In this context the \emph{test} is the ensemble of all the operations performed to classify each items; \emph{positive} and \emph{negatives} label the items according to the two classes c=N, P= 0,1  they belong to: \emph{true} (T) and \emph{false} (F) represent the ability of the test to classify coherently or not a given item in the test classification with respect to the gold standard classification. 
 }
\value{

  \item{binary.performance}{A matrix of statistical scores for both binary and multiple classes test, including PPV, NPV, Se  and Sp.}
   \item{multi.performance}{A matrix of statistical scores for multiple classes test , including predictive value (PV)  and Sensitivity/Specificity (S) of each class. }
   
    In multiple test case,  the PPV and Se from \emph{binary.performance}  summarize the performance of all the positive (non-zero) classifications, while NPV and Sp evaluate the negative (zero) classification performance.

}

\references{ C. Nardini, H. Peng, L. Wang, L. Benini, M.D. Kuo, MM-Correction: Meta-analysis-Based Multiple Hypotheses Correction in Omic Studies? \emph{Springer CCIS}, \bold{25} , pp 242--255, 2008. }
\author{ Nardini, C. and Liu, Y-H.}
\note{ The test results T and the reference results GS should have the same dimensions. 
}

\examples{
# ######Binary test ########
GS<-cbind(c(0, 1),c(0, 0),c(1, 1))
 T <-cbind(c(1, 1),c(1, 0),c(1, 1))
 Nclasstest(T,GS)
 
# binary.performance 
#      PPV  NPV   Se     Sp 
#[1,] 0.6    1     1     0.3333333
 
#########Multiple classes test #######
 GS <- cbind(c(0, -1, 1), c(0, 1, 0), c(1, 0, 1)) 
 T <- cbind(c(1, -1, 1), c(0, 1, -1), c(0, 1, 1)) 
 Nclasstest(T, GS) 
 
# multi.performance 
#      class.type   PV       S 
#   1      -1       1.00    0.5 
#   2       0       0.25    0.5 
#   3       1       0.75    0.6 

# binary.performance 
#          PPV     NPV   Se     Sp 
#[1,] 0.5714286   0.5    0.8   0.25 

        
\dontshow{

## The function is currently defined as
function(T,GS)
{
     # Using PPV,NPV,Sensitivity and specificity to evaluate the performance of a n-class classification. 
     # GS is Golden Standard classification result or reference result, could be a matrix or vector with numeric elements
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
}
}

\keyword{ classif }
