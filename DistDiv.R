# This R script was written by Erin Fry in the Vinny Lynch lab at the University of Chicago. 
# It was made on June 15, 2016 and updated on June 17, 2016.
# The goal of this script is to find the percentage divergence of two distributions, that is to say, the amount that the two distributions are different from one another

# First, enter the parameters to make your 2 distributions. You may wish to modify this to add real data rather than R-generated distributions or to include more than 2 distributions. 

## Sample distributions


dist1<-rnorm(n=10000,mean=9,sd=1)
dist2<-rnorm(n=10000, mean=5,sd=1)

## Enter the number of bins you want to include (sample bins = 100)

nbin = 100

## The function 'DistDiv' finds the frequency at which two distributions are different from one another
## The first two arguments (dist1 and dist2) are the distributions of interest
## The second is the number of bins you would like to divide the data into, default=100

DistDiv<-function(dist1,dist2,nbin=100) {

## Define the bins each distribution will be broken up into
  minimum=(min(dist1, dist2)) #minimum value of each distribution
  maximum=(max(dist1, dist2)) #maximum value of each distribution
  bins <- seq(minimum, maximum, by =(maximum-minimum)/nbin )  #creates evenly spaced bins from the minimum to maximum values
 
## Create a data frame to contain the number of counts from each distribution in each bin
  ## the hist(plot=FALSE) function creates a list containing count information in the bins speficied above
  counts<-as.data.frame(cbind(hist(dist1, plot=FALSE, breaks=bins)$counts,hist(dist2, plot=FALSE, breaks=bins)$counts))
  colnames(counts)<- c("Dist1Counts", "Dist2Counts") #set the column names
  
## find the number of overlapping counts across all bins
  ## create new column containing the minimum count of the two distributions
  ## this minimum count is equal to half of the overlap between the distributions in that bin
  counts$overlap<-apply(counts[,1:2],1,min)  #Take the minimum count for each bin
  
  ## multiply the overlap by two to equal the percent overlap of the two distributions
  ## then divide by the total number of observations, and subtract from one
  ## Returns the proportion different between the two distributions (the complement of this number gives the proportion of overlap)
  return(1-(2*sum(counts$overlap))/sum(counts$Dist1Counts,counts$Dist2Counts))
  
}

DistDiv(dist1,dist2,100)