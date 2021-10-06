RNGversion(min(as.character(getRversion()), "3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")

n <- 16
knapsack_objects <- 
  data.frame(
    w = sample(1:4000, size = n, replace = TRUE), v = runif(n = n, 0, 10000))


knapsack_dynamic <- function(cx, W){
  
  #check the input
  if(!is.data.frame(cx) || (length(cx) != 2)){(stop("Not a data frame or incorrect number of parameters!"))}
  if(min(cx$v) < 0) {stop("Vector 'v' is not all positive!")}
  if(min(cx$w) < 0) {stop("Vector 'w' is not all positive!")}
  
  #create the tabulation matrix:
  m_tabulation <- matrix(data = NA, nrow = length(cx$w) + 1, ncol = W + 1, byrow = FALSE, dimnames = NULL)
  m_tabulation[1,] <- 0
  m_tabulation[,1] <- 0
  #print(m_tabulation)
  
  #write the recursive formula into the recursive function:
  tabulation_formula <- function(tabulation_row_number, tabulation_column_number){
    
  }
  
  #call the function recursively until the table is filled
  
  #find the best solution from the table
  
  #calculate the running time
}


knapsack_dynamic(cx = knapsack_objects[1:8, ], W = 2500)