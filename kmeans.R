rm(list=ls())

#global variables
samples=20
k=4
features=2
#generates random data; columns are instances (samples), features are rows!
generateData = function(features,samples) {
  dataForMatrix = rnorm(n = (features*samples) )
  returnMatrix = matrix( data = dataForMatrix, nrow = features, ncol =samples)
  return(returnMatrix)
}
dataMatrix = generateData(features, samples)

euclid = function(x,y)
{
  if (length(x)!=length(y))
  {stop("Vectors not of same length")}
  else
    {
      return(sqrt(sum( (y-x)^2 ) ))
    }
}
manhattan = function(x,y)
{
  if (length(x)!=length(y))
  {stop("Vectors not of same length")}
  else
  {
    return(sum(abs(y-x)))
  }
}

pickCentroids = function(samples, k)
{
  return(sample(1:samples,k))
}
createCentroidMatrix= function (centroids, features)
{
  centroidMatrix = matrix(data=0,nrow = features, ncol=length(centroids))
  for(i in 1:k)
  {
    centroidMatrix[,i]= dataMatrix[, centroids[i]]
  }
  return(centroidMatrix)
}
computeDistanceMatrix = function(dataMatrix, centroidMatrix)
{
  distanceMatrix = matrix(data=0, nrow = k, ncol=length(dataMatrix[1,]))
  for(i in 1:samples) 
  {
    for (j in 1:k)
    {
      distanceMatrix[j,i]  =euclid(dataMatrix[,i], centroidMatrix[,j])
    }
  }
  return(distanceMatrix)
}
computeMembershipVector= function(distanceMatrix)
{
  membershipVector = numeric(length(distanceMatrix[1,]))
  for(i in 1:samples)
  {
    membershipVector[i]= c(which.min(distanceMatrix[,i]))
  }
  return(membershipVector)
}
recomputeCentroidLocation = function(centroidMatrix,membershipVector)
{
  for(j in 1:features)
  {
    for (i in 1:k)
    {
      centroidMatrix[j,i] = mean(dataMatrix[j,(which(membershipVector %in% i)  )])
    }
    return(centroidMatrix)
  }
}

main = function(dataMatrix, k, maxIterations=10000000000)
{
  #initialising local variables
  samples = length(dataMatrix[1,])
  features = length(dataMatrix[,1])
  iter=0
  oldMembershipVector = numeric(length = length(dataMatrix[1,]))
  firstCentroids = pickCentroids(samples = samples, k = k)
  SSE=0
  while(iter<maxIterations) 
  { 
    centroidMatrix = createCentroidMatrix(centroids = firstCentroids, features)
    distanceMatrix = computeDistanceMatrix(dataMatrix = dataMatrix, centroidMatrix = centroidMatrix)
    
    if(exists("membershipVector"))
    {oldMembershipVector = membershipVector}
    
    membershipVector = computeMembershipVector(distanceMatrix)
    
    oldCentroidMatrix = centroidMatrix
    centroidMatrix = recomputeCentroidLocation(centroidMatrix,membershipVector)
    
    distanceMoved=0
    for (i in 1:k)
    {
      distanceMoved= distanceMoved + euclid(oldCentroidMatrix[,i], centroidMatrix[,i])
    }
    
    oldSSE = SSE
    SSE=0
    #SSE calculation
    for (i in 1:k)
    {
      centroid = centroidMatrix[,i]
      for (j in 1:length( which( membershipVector %in% i) ) )
      {
        SSE=SSE+(euclid(dataMatrix[,j], centroid )^2)
      }
    }
    #oldSSE- SSE >
    
    
    #could change if structure to change escape 
    
    iter=iter+1
    if (iter>=3)
    {
      if((oldSSE-SSE) <0.01)
      {break}
      else if (distanceMoved<0.01)
      {break}
      else if (sum(membershipVector==oldMembershipVector)==length(membershipVector))
      {break}}
    
  }
  cat("membership vector: ",membershipVector, "\n",
      "iterations: ", iter,'\n',
      "old membership vector: ", oldMembershipVector,'\n')
}

main(dataMatrix,k)


counter=0
while(counter<10000)
{
main(dataMatrix,k)
counter=counter+1
}
