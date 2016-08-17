#K means function that takes data with 2 or 3 variables, a fixed cluster number, 
#and iterates over the k means cluster algorithm a maximum of maxIter times, or until the 
#cluster assignments do not change. This functions repeats nReps times and finds the lowest
#distance for the cluster assignments over all nReps. It takes those mimimized cluster assignments
#and plots them. 


mykmeans<-function(nReps,myScatterInput,myClusterNum,maxIter){
  clusterHistory <- vector(maxIter, mode="list")
  centerHistory <- vector(maxIter, mode="list")
  #Run K means algorithm nReps number of times, with different starting seed each time.
  for (i in 1:nReps){
    set.seed(1000+i)
    data_matrix<-as.matrix(myScatterInput)
    #Randomly choose myClusterNum points from the myScatterInput data frame. 
    #As the starting cluster centroids. 
    Centroids<-data_matrix[sample(nrow(data_matrix), myClusterNum), ]
    #set old and new cluster arrays to iteratively reassign and compare cluster values after each nRep
    Clustersold<-rep(0,nrow(data_matrix))
    Clustersnew<-rep(1,nrow(data_matrix))
    
    #Make new centroids by taking the average of all the points in each cluster. 
    #repeat until the cluster assignments do not change, or until the maxIter 

    count<-0
    while(count<maxIter & identical(Clustersold,Clustersnew)==FALSE) {
      count<-count+1
      Clustersold<-Clustersnew
      #check the distances of each data point to each of the centroids
      #from http://stackoverflow.com/questions/31571236/my-own-k-means-algorithm-in-r
      distsToCenters <- euclid(data_matrix, Centroids)
      #find the centroid that is closest to each datapoint - and assign datapoint to that cluster
      #from http://stackoverflow.com/questions/31571236/my-own-k-means-algorithm-in-r
      Clustersnew <- apply(distsToCenters, 1, which.min)
      #recalculate new centroids but computing the average of all datapoints in each cluster
      #this line by me - I am so proud that i thought of this when I was exhausted
      Centroids<-sapply(1:ncol(data_matrix),function(x) sapply(1:myClusterNum, function(y) mean(data_matrix[ which(Clustersnew== y),x],na.rm=TRUE)))
      #end of while loop
      }
   
    
    #Once you have reached a terminating condition, 
    #compute the sum of all Euclidean distances from each point to their respective centroids.
    #for row x, x want to take the euclidean distance of row x with 
    sumdists <- sapply(1:nrow(data_matrix), function(x) euclid2(data_matrix[x,], Centroids[Clustersnew[x],]))
    tempSum<-sum(sumdists)
    
    # Identify the replication with the lowest sum of Euclidean distances from points to centroids 
    #as your best result and print the value to the console.
    if(i==1){
      finalSum<<-tempSum
      finalClusters<<-Clustersnew
    }else{
      if(tempSum<finalSum){
        finalSum<<-tempSum
        finalClusters<<-Clustersnew
        minRep<<-i
      } 
    }
    

  
    #end of nReps  
  }
  
  data_matrix<-cbind(data_matrix,finalClusters)
  print(paste("The minimal sum is", finalSum,  "which is found on rep", minRep))
  #Plot here if the data frame is 2d or 3d. 
  if(ncol(myScatterInput)==2){
    plot(data_matrix[,1], data_matrix[,2], col=data_matrix[,3])
  } else if(ncol(myScatterInput)==3){
    if(require("scatterplot3d")==FALSE){
      install.packages("scatterplot3d")
    }
    scatterplot3d(data_matrix[,1], data_matrix[,2], data_matrix[,3],color=data_matrix[,4])
  } else {
    print("Sorry Paul, this dataset you entered has too many dimensions to plot.")
  }
 

#end of k means  
}


#euclidean distance matrix for multiple points
#function from http://stackoverflow.com/questions/31571236/my-own-k-means-algorithm-in-r
euclid <- function(points1, points2) {
  distanceMatrix <- matrix(NA, nrow=dim(points1)[1], ncol=dim(points2)[1])
  for(i in 1:nrow(points2)) {
    distanceMatrix[,i] <- sqrt(rowSums(t(t(points1)-points2[i,])^2))
  }
  distanceMatrix
}

#euclidean distance value for two points
euclid2 <- function(row1, row2) {
  distance<-0
  for(i in 1:length(row2)) {
    distance<-distance+(row1[i]-row2[i])^2
  }
  distance<-sqrt(distance)
}




#reads parameters
params<-read.csv("hw3params.csv", header=FALSE)
#reads data
myScatterInput1<-iris[,1:4]
#myScatterInput1<-data.frame(read.csv("hw3data.csv", header=FALSE))


mykmeans(nReps=as.numeric(params[1]), 
         myScatterInput=myScatterInput1,
         myClusterNum=as.numeric(params[2]),
         maxIter=as.numeric(params[3]))









