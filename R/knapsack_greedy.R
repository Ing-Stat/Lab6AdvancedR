n <- 800
knapsack_objects <-
  data.frame(
    w = sample(1:4000, size = n, replace = TRUE), v = runif(n = n, 0, 10000))


knapsack_greedy <- function(cx, W){
  #' Title
  #'
  #' @export knapsack_greedy
  #' 
  #' @return 
  
  startTime <- Sys.time() 
  
  WeightLimit  <- W
  sumWeight    <- 0
  sumValue     <- 0
  counter      <- 1
  #numberInSack <- 0
  vectorItemNr <- c()
  
  #check the input
  if(!is.data.frame(cx) || (length(cx) != 2)){(stop("Not a data frame or incorrect number of parameters!"))}
  
  if(min(cx$v) < 0) {stop("Vector 'v' is not all positive!")}
  if(min(cx$w) < 0) {stop("Vector 'w' is not all positive!")}
  if(W < 0) {stop("The weight should be positive!")}
  
  startTime <- Sys.time()
  
  valuePerWeight <- cx$v/cx$w
  elementNumber <- c(1:length(cx$w))
  cx <- cbind(cx, valuePerWeight, elementNumber)
  cx <- cx[order(cx$valuePerWeight, decreasing = TRUE),] 
  
  while (sumWeight < WeightLimit) {
    
    sumWeight <- sumWeight + cx$w[counter]
    sumValue  <- sumValue + cx$v[counter]
    vectorItemNr[counter] <- cx$elementNumber[counter]
    #numberInSack <- numberInSack + 1 
    counter <- counter + 1
  }
  
  stopTime <- Sys.time()
  timeDiff <- stopTime - startTime
  timeDiff
  
  resultat <- list("value" = sumValue, "elements" = sort.int(vectorItemNr), "time_elapsed" = round(timeDiff, digits = 5))
  return(resultat)
  
}  


#knapsack_greedy(cx = knapsack_objects[1:800, ], W = 3500) 
