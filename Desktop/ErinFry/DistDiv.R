##Example distributions

dist1<-rnorm(n=10000,mean=9,sd=1)
dist2<-rnorm(n=10000, mean=5,sd=1)

#The function 'DistDiv' finds the frequency at which two distributions are different from one another
#The first two arguments (dist1 and dist2) are the distributions of interest
#The second is the number of bins you would like to divide the data into, default=100

DistDiv<-function(dist1,dist2,nbin=100) {

#first, define the bins each distribution will be broken up into
  minimum=(min(dist1, dist2)) #minimum value of both distributions
  maximum=(max(dist1, dist2)) #maximum value of both distributions
  bins <- seq(minimum, maximum, by =(maximum-minimum)/nbin )  #create nbins from the minimum to maximum values

#Create a data frame to contain the number of counts from each distribution in each bin
  #the hist(plot=FALSE) function creates a list containing count information in the bins speficied above
  counts<-as.data.frame(cbind(hist(dist1, plot=FALSE, breaks=bins)$counts,hist(dist2, plot=FALSE, breaks=bins)$counts))
  colnames(counts)<- c("Dist1Counts", "Dist2Counts") #set the column names
  
#find the number of overlapping counts across all bins
  ##create new column containing the minimum count of the two distributions
  ##this minimum count is equal to half of the overlap between the distributions in that bin
  counts$overlap<-apply(counts[,1:2],1,min)  #Take the minimum count for each bin
  
  #multiple the overlap by two to equal the percent overlap between the two distributions
  #then divide by the total number of observations to get the proportion overlap between the two distributions
  return(1-(2*sum(counts$overlap))/sum(counts$Dist1Counts,counts$Dist2Counts))
  
}
