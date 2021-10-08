RNGversion(min(as.character(getRversion()), "3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")

n <- 16
knapsack_objects <- 
  data.frame(
    w = sample(1:4000, size = n, replace = TRUE), v = runif(n = n, 0, 10000))

brute_force_knapsack <- function(cx, W) {
  #' Title
  #'
  #' @export brute_force_knapsack
  #' 
  #' @return 
  
  if(!is.data.frame(cx) || (length(cx) != 2)){(stop("Not a data frame or incorrect number of parameters!"))}
  if(min(cx$v) < 0) {stop("Vector 'v' is not all positive!")}
  if(min(cx$w) < 0) {stop("Vector 'w' is not all positive!")}
  if(W < 0) {stop("The weight should be positive!")}
  
  startTime <- Sys.time()
  
  vectorMaxCombinations <- vector()
  vectorMaxValues       <- vector()
  
  Wlimit <- W
  
  combinationItems <- c(1:length(cx$w))
  
  for(s in 2:length(cx$v)) {
    
    rowCounter <- 1
    
    sampleSize         <- s
    listOfCombinations <- utils::combn(combinationItems, sampleSize)     
    
    lista <- t(listOfCombinations)
    
    langd <- length(lista)/sampleSize
    
    vector_Result_Values  <- vector()
    vector_Result_Weights <- vector()
    vector_Result_Items   <- vector()
    
    for(m in 1:langd) {
      
      items <- lista[m, ]
      
      len <- length(items)
      sumValue  <- 0
      sumWeight <- 0
      
      for(k in 1:len) {
        sumValue  <- sumValue  + cx$v[items[k]]
        sumWeight <- sumWeight + cx$w[items[k]]        
      }
      
      if(sumWeight > Wlimit) next
      vector_Result_Values[rowCounter]  <- sumValue
      vector_Result_Weights[rowCounter] <- sumWeight 
      vector_Result_Items[rowCounter]   <- stringr::str_c(c(items), collapse = ", ") 
      rowCounter <- rowCounter + 1   
    }
    
    vektorLangd <- length(vector_Result_Items)
    if(vektorLangd == 0) next
    maxNr <- which.max(vector_Result_Values)
    vectorMaxCombinations[s-1] <- vector_Result_Items[maxNr]
    vectorMaxValues[s-1]       <- vector_Result_Values[maxNr]
  }
  
  maxValue <- max(vectorMaxValues)
  maxValueRow <- which.max(vectorMaxValues)
  kombination <- vectorMaxCombinations[maxValueRow]
  
  maxValue
  kombination <- as.numeric(unlist((strsplit(kombination, ","))))
  
  stopTime <- Sys.time()
  
  timeDiff <- stopTime - startTime
  timeDiff
  
  resultat <- list("value" = round(maxValue, digits = 3), "elements" = kombination, "time_elapsed" = round(timeDiff, digits = 4))
  return(resultat)
}

#brute_force_knapsack(cx = knapsack_objects[1:16, ], W = 3500)
