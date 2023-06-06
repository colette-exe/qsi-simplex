# Angelica Nicolette U. Adoptante
# 2020-01692
# CMSC 150 - B1L
# Exercise 9: Simplex Method

# simplex:
#   input: 
#       tableau = matrix that represents initial tableau, 
#       isMax   = boolean: if true, do maximization
#                          if false, do minimization
#       problem = boolean: if true, return num (for the problem in the handout)
#   output: (labeled list)
#       final.tableau = final matrix
#       basic.solution = a matrix/vector of the final solution
#                      = with labels for each value
#       opt.val = the maximum or minimum value
#       shipping.num =  a matrix of the number of items shipped from a plant to a warehouse. The rows
#         represent the plants and the columns are the warehouse. Make sure to name each row and column 
#         properly. This return value will only be returned when the problem parameter is true. 
#         This return value is for the given problem above and should not be returned (problem parameter should be false) 
#         if the function will be used apart from the problem.

is.negative <- function(vector) {
  negative = TRUE
  for (i in 1:length(vector)) {
    if (isTRUE(vector[i] < 0)) {
      negative = TRUE
      break
    } else {
      negative = FALSE
    }
  }
  return(negative)
}

# simplex function
simplex <- function(tableau, isMax, problem) {
  num_row = nrow(tableau)
  num_col = ncol(tableau)
  
  #----SOLVING-----------------------------------
  counter = 1
  while (is.negative(tableau[num_row,]) && counter != 20) {

    # first: check the negative number with the largest magnitude
    negatives = vector() #store all negatives in a vector
    index = 0
    for (i in 1:(num_col-1)) {
      if (tableau[num_row, i] < 0) {
        index = index + 1
        negatives[index] = tableau[num_row, i]
        }
    }
    #to determine what the pivot column is
    pivot_col_indicator = max(abs(negatives[1:index]))
    
    #learned this from Nizamuddin Siddiqui at tutorialspoint.com
    index = which(tableau==(-pivot_col_indicator), arr.ind=TRUE)
    
    # second : get column number
    pivot_col = index[1,2] # first element is the row number, the second is the column number

    # third : find the pivot element
    # get the smallest test ratio 
    #index of the least tr
   
    #put test ratios in a vector
    #if /0 put -1
    test_ratios = vector()
    for (i in 1:(num_row-1)) {
      test_ratios[i] = tableau[i,num_col]/tableau[i,pivot_col]
      if (is.na(test_ratios[i])) test_ratios[i] = -1
      if (is.infinite(test_ratios[i])) test_ratios[i] = -1 # tableau[i,pivot_col] = 0
      if (isTRUE(tableau[i,pivot_col] < 0)) test_ratios[i] = -1
    }
    
    #get the least tr from a new vector for valid test ratios
    valid_tr = vector()
    index = 1
    for (i in 1:length(test_ratios)) {
      if (isFALSE(test_ratios[i] < 0)) {
        valid_tr[index] = test_ratios[i]
        index = index + 1
      }
    }
    
    if (length(valid_tr) == 0) {
      counter = 20
      break
    }
    less_index = which(test_ratios==min(valid_tr), arr.ind=TRUE)
    
    #pivot element's row index
    index_pr = less_index[1] #if there's a tie between rows and their test ratios, choose the first one
    pivot_element = tableau[index_pr, pivot_col]
    pivot_row = tableau[index_pr, ]
    
    #get normalized row
    normalized = pivot_row/pivot_element
    
    #eliminate rows 
    for (i in 1:num_row) {
      if (isTRUE(i==index_pr)){
        tableau[i,] = normalized
      }
      else {
        C = tableau[i,pivot_col]
        tableau[i,] = tableau[i,] - (normalized*C)
      }
    }
    counter = counter + 1
  }
  
  final.tableau = tableau
  #--------------------------------------------------------
  
  #----Basic Solution--------------------------------------
  basic.solution = matrix(0, nrow=1, ncol=(num_col-1))
  if (isMax) { #maximization
    for (i in 1:(num_col-1)) {
      for (j in 1:num_row) {
        if (tableau[j,i] == 1) {
          basic.solution[i] = tableau[j,num_col]
        } else {
          if (tableau[j,i] > 0) {
            basic.solution[i] = 0 
            break
          }
        }
      }
    }
    names = colnames(tableau)
    names = names[-length(names)]
    colnames(basic.solution) = names #learned this from https://stat.ethz.ch/R-manual/R-devel/library/base/html/colnames.html
  } else { #minimization
    basic.solution = tableau[num_row,1:(num_col-1)]
    basic.solution[(num_col-1)] = tableau[num_row, num_col]
  }
  
  #--------------------------------------------------------
  
  #----Optimum Value---------------------------------------
  opt.val = basic.solution[(num_col-1)]
  #-------------------------------------------------------
  
  #----Shipping Num---------------------------------------
  shipping_names = list(c("DEN", "PHO", "DAL"), c("SAC", "SL", "ALB", "CHI", "NYC"))
  shipping.num = matrix(basic.solution[9:23], nrow=3, ncol=5, dimnames=shipping_names, byrow = TRUE) # 3 plants, 5 warehouses
  
  #-------------------------------------------------------
  
  #-------------------------------------------------------

  #if problem is true, solve for shipping num
  #add shipping num to list
  if (problem) {
    solution = list(final.tableau = final.tableau, basic.solution = basic.solution, opt.val = opt.val, shipping.num = shipping.num)
  } else {
    solution = list(final.tableau = final.tableau, basic.solution = basic.solution, opt.val = opt.val)
  }
  
  if (counter == 20) {
    print("No Feasible Solution")
    return(NA)
  } else {
    #return the labeled list
    return (solution)
  }
}

#matrix set up-er
set_up <- function(fxns, isMax) {
  #matrix
  mat = matrix(0, nrow = length(fxns), ncol = length(fxns[[1]]))
  for (i in 1:nrow(mat)) mat[i, ] = fxns[[i]]
  rownames(mat) = 1:nrow(mat)
  colnames(mat) = 1:ncol(mat)
  
  if (isMax) {
      t_matrix = mat
  } else {
      #transpose matrix
      t_matrix = t(mat)
      t_matrix[nrow(t_matrix),] = -t_matrix[nrow(t_matrix),]
  }
  
  #naming
  names = vector()
  for (i in 1:(ncol(t_matrix)-1)) names[i] = paste("s", i, sep="")
  names[ncol(t_matrix)] = "Solution"
  colnames(t_matrix) = names
  
  #slack variables
  slack_vars = matrix(0, nrow=nrow(t_matrix), ncol=nrow(t_matrix))
  for (i in 1:nrow(slack_vars)) {
    for (j in 1:ncol(slack_vars)) {
      if (isTRUE(i == j)) {
        slack_vars[i,j] = 1
      }
    }
  }
  
  names = vector()
  for(i in 1:(ncol(slack_vars)-1)) names[i] = paste("x", i, sep="")
  names[ncol(slack_vars)] = "Z"
  colnames(slack_vars) = names
  
  test = cbind(t_matrix[,1:(ncol(t_matrix)-1)], slack_vars)
  test = cbind(test, t_matrix[,ncol(t_matrix)])
  test[nrow(test), ncol(test)] = 0
  colnames(test)[ncol(test)] = "Solution"
  
  return(test)
}


