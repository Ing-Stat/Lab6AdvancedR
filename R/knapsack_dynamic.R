RNGversion(min(as.character(getRversion()), "3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")

n <- 16
knapsack_objects <- 
  data.frame(
    w = sample(1:4000, size = n, replace = TRUE), v = runif(n = n, 0, 10000))


knapsack_dynamic <- function(cx, W){
  
  #Number of items
  n_items <- length(cx$w)
  
  #check the input
  if(!is.data.frame(cx) || (length(cx) != 2)){(stop("Not a data frame or incorrect number of parameters!"))}
  if(min(cx$v) < 0) {stop("Vector 'v' is not all positive!")}
  if(min(cx$w) < 0) {stop("Vector 'w' is not all positive!")}
  
  startTime <- Sys.time()
  
  #print(cx)
  
  #create the tabulation matrix:
  m_tabulation <- matrix(data = 0, nrow = n_items + 1, ncol = W + 1, byrow = FALSE, dimnames = NULL)
  #print(m_tabulation)
  
  #write the recursive formula into the recursive function, Currently, nonrecursive implementation. The formula can be found at https://en.wikipedia.org/wiki/Knapsack_problem#0-1%20knapsack%20problem
  tabulation_formula <- function(tabulation_row_number, tabulation_column_number){
    if (cx$w[tabulation_row_number - 1] > W){
      m_tabulation[tabulation_row_number, tabulation_column_number] <<- m_tabulation[tabulation_row_number - 1, tabulation_column_number]
      return(m_tabulation[tabulation_row_number, tabulation_column_number])
    }
    else if (cx$w[tabulation_row_number - 1] <= W){
      m_tabulation[tabulation_row_number, tabulation_column_number] <<- max(m_tabulation[tabulation_row_number - 1, tabulation_column_number],  m_tabulation[tabulation_row_number - 1, max(tabulation_column_number - cx$w[tabulation_row_number - 1], 0)] + cx$v[tabulation_row_number - 1])
      return(m_tabulation[tabulation_row_number, tabulation_column_number])
    }
    else{
      return(m_tabulation[tabulation_row_number, tabulation_column_number])
    }
  }
  
  #Fill the table. Currently, nonrecursive implementation. As can be observed, the computational complexity is =(W*n_items)
  for (items in 2:(n_items + 1)) {
    for (tabulated_weight in 2:(W + 1)) {
      tabulation_formula(items, tabulated_weight)
      #print(m_tabulation)
    }
  }

  #find the best solution from the table
  n_max_value <- max(m_tabulation)
  #print(n_max_value)
  n_value_subtracted <- n_max_value
  
  #Find which objects are included
  v_included_objects <- c()
  for (item in 1:n_items) {
    if(length(which(m_tabulation[n_items + 1 - item,] == n_value_subtracted)) > 0){
      next
    }
    else{
      n_value_subtracted <- n_value_subtracted - cx$v[n_items + 1 - item]
      v_included_objects <- c(v_included_objects, n_items + 1 - item)
    }
  }
  
  #print(v_included_objects)
  
  #calculate the running time
  stopTime <- Sys.time()
  
  timeDiff <- stopTime - startTime
  timeDiff
  
  resultat <- list("value" = n_max_value, "elements" = sort.int(v_included_objects), "time_elapsed" = round(timeDiff, digits = 4))
  return(resultat)
}


knapsack_dynamic(cx = knapsack_objects[1:16, ], W = 3500)