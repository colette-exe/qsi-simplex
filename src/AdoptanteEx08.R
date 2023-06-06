# Angellica Nicolette U. Adoptante
# CMSC 150 - B1L
# Exercise 08 - Quadratic Spline Interpolation

# function name: poly.qsi
# input: data [vector containing x and y]; x = value to be evaluated
# output: qsi.fxns - a list containing all functions per interval
# y - estimated value of fn(x) using the function for an interval based on the value of x

# quadratic spline
# n + 1 data points; n intervals (ex. 4 data points, 3 intervals)
# i = 2 
# i to n


#---------------------------------------
pivotRowFinder <- function(amat, n, col, max) {
  for (i in 1:n) {
    if (isTRUE(abs(amat[i,col]) == max)) {
      break;
    }
  }
  
  return(i)
}
swap <- function(a, arow, b, brow, mat) {
  temp = a
  a = b
  b = temp
  
  #update original matrix
  mat[arow,] = a
  mat[brow,] = b
  
  return(mat)
}
GaussianElimination <- function(a) {
  n = nrow(a)
  valid = TRUE
  
  #Phase 1 : Forward Elimination
  for (i in 1:n) {
    #looking for pivot row number
    pivot_row_index = pivotRowFinder(a, n, i, max(abs(a[i:n, i])))
    
    #if there's no unique solution
    if (isTRUE(a[pivot_row_index, i] == 0)) {
      valid = FALSE
      break;
    }
    
    #pivot
    a = swap(a[pivot_row_index,], pivot_row_index, a[i,], i, a)
    
    m = i+1
    if (i != n){
      for (j in m:n) {
        #get pivot element
        pivot_element = a[i,i]
        #get multiplier
        multiplier = a[j,i] / pivot_element
        
        #round multiplier
        round(multiplier, digits = 4)
        
        #multiply the multiplier
        temporary_vector = multiplier*a[i,]
        
        #round temporary vector
        round(temporary_vector, digits= 4)
        
        #subtract original vector and temporary vector
        resulting_vector = a[j,] - temporary_vector
        
        #update row
        a[j,] = round(resulting_vector, digits = 4)
      }
    }
  }
  
  #Phase 2: Backward Substitution
  if (isTRUE(valid)){
    x <- numeric(n+1)
    for (i in n:1) {
      x[i] = round((a[i, n+1] - sum(a[i, (i+1):n] * x[(i+1):n])) / a[i,i], digits = 4)
    }
    x = x[-(n+1)]
    #variables used
    variables = dimnames(a)[[2]][1:n]
    
    solution <- list(SolutionSet = x, variables = variables, ResultingMatrix = a)
    return(solution)
  } 
  else {
    return(NA)
  }
}
#---------------------------------------

poly.qsi <- function (data, x) {
  
  #x and f(x) values
  x_data = data[[1]]
  y_data = data[[2]]
  #x - the value to be evaluated
  
  #lengths
  x_data_pts = length(x_data)
  y_data_pts = length(y_data)
  
  #the number of x's should be equal to the number of y's
  if (isTRUE(x_data_pts == y_data_pts) && x <= x_data[x_data_pts] && x >= x_data[1]) {
    
    #get n (intervals)
    n = x_data_pts - 1
    i = 2
    
    #condition 1: Internal Knots
    #condition 2: End Points
    #condition 3: Equal First Derivatives at the Interior Knots
    #condition 4: Second Derivative is 0 at the first point
    condition_1 = vector()
    condition_2 = vector()
    condition_3 = vector()
    condition_4 = "a1 = 0"
    
    condition_3_simplified = vector()
    counter_1 = counter_2 = counter_3 = 1
    for (j in i:n) {
      
      #--condition 1---------------------------
      #two equations per i
      #ai-1 * xi-1 ^ 2 + bi-1 * xi-1 + ci-1 = f(xi-1)
      #ai * xi-1 ^ 2 + bi * xi + ci = f(xi-1)
      temp1 = paste(x_data[j] ^ 2, "*a", j-1, " + ", x_data[j], "*b", j-1, " + ", "1*c", j-1, " = ", y_data[j], sep = "")
      temp2 = paste(x_data[j] ^ 2, "*a", j, " + ", x_data[j], "*b", j, " + ", "1*c", j, " = ", y_data[j], sep = "")
      
      #add to condition 1 list
      #update counter
      condition_1[counter_1] = temp1
      counter_1 = counter_1 + 1
      condition_1[counter_1] = temp2
      counter_1 = counter_1 + 1
      #----------------------------------------
      
      #--condition 2----------------------------
      if (j-1 == 1) {
        temp = paste(x_data[j-1] ^ 2, "*a", j-1, " + ", x_data[j-1], "*b", j-1, " + ", "1*c", j-1 , " = ", y_data[j-1], sep = "")
        condition_2[counter_2] = temp
        counter_2 = counter_2 + 1
        }
      if (j+1 == x_data_pts) {
        temp = paste(x_data[j+1] ^ 2, "*a", j, " + ", x_data[j+1], "*b", j, " + ", "1*c", j , " = ", y_data[j+1], sep = "")
        condition_2[counter_2] = temp
        counter_2 = counter_2 + 1
      }
      #----------------------------------------
        
      #--condition 3---------------------------
      temp = paste(2*x_data[j], "*a", j-1, " + ", "1*b", j-1, " = ", 2*x_data[j], "*a", j, " + ", "b", j, sep="")
      condition_3[counter_3] = temp
      temp = paste(2*x_data[j], "*a", j-1, " + ", "1*b", j-1, " + ", -2*x_data[j], "*a", j, " + ", "-1*b", j, sep="")
      condition_3_simplified[counter_3] = temp
      counter_3 = counter_3 + 1
      #----------------------------------------
    }
    
    
    #list of all equations per condition
    condition.fxns = list(condition_1, condition_2, condition_3, condition_4)
    #functions for condition 3 where simplified
    condition.fxns_simplified = list(condition_1, condition_2, condition_3_simplified)
    
    #setting up the matrix
    #3n equations - 1 (condition 4, a1 = 0) = 3n-1 rows and 3n columns
    rhs = matrix(nrow=(3*n)-1)
    s = list()  #s holds all simplified versions of the polynomials per interval
    rhs_row = 1
    #iterate through the 3 conditions
    for (i in 1:3) {
      #temporary vector that holds each term in the equation
      temp = vector()
      
      #iterate through each equation per condition
      for (j in 1:length(condition.fxns_simplified[[i]])) {
        
        #setting up rhs
        rhs_term = strsplit(condition.fxns_simplified[[i]][j], "\\=");
        
        #the first element of rhs_term is the equation on the left side of =
        #split that using '++ as separator
        term = strsplit(rhs_term[[1]][1], "\\+")
        temp[j] = term
        
        #if rhs is null (for condition 3), make it 0
        if (is.na(rhs_term[[1]][2]) || is.null(rhs_term[[1]][2])) {
          rhs[rhs_row,] = 0
        } 
        else {
          rhs[rhs_row,] = eval(parse(text = rhs_term[[1]][2]))
        }
        
        #increments up to 3n-1 rows
        rhs_row = rhs_row + 1
      }
      s[[i]] = temp
    }
    #s[[1]][[1]][1]

    #matrix containing the coefficients of the polynomials
    aug_coeff_mat = matrix(0, nrow = (3*n)-1, ncol=(3*n)-1)
    
    #row increases per polynomial (up to ((3*n)-1))
    #used for aug_coeff_mat
    row = 1
    
    #iterate through all terms (s[[i]][[j]][k])
    for (i in 1:length(s)) { #iterate through each condition
      for (j in 1:length(s[[i]])) { #iterate through each equation per condition
        each_row = vector()
        for (k in 1:length(s[[i]][[j]])) { #iterate through each term per equation
          term = strsplit(s[[i]][[j]][k], "\\*")
          #first element of term is the coefficient
          element = eval(parse(text = gsub(" ", "", term[[1]][1])))
          
          #getting the variable index
          #second element of term is the variable (ex. b1, c2, etc)
          another_term = strsplit(term[[1]][2], "")
          letter = gsub(" ", "", another_term[[1]][1]) #getting rid of spaces if there's any
          index = eval(parse(text = gsub(" ", "", another_term[[1]][2])))
          
          #returns corresponding column number 
          #depending on the variable letter (a,b,or c)
          #and the subscript of that variable, 1,2,3...
          col_num  = switch(
                  letter,
                  "b" = 2*index + (index-2),
                  "c" = 2*index + (index-1),
                  "a" = (2*index + (index-2))-1
                )
          
          #col_num is our index for each_row
          if (isTRUE(col_num != 0)) { #not a1
            each_row[col_num] = element
          } 
          
          #for indices with no value, assign 0
          for (l in 1:((3*n)-1)) {
            if (is.na(each_row[l])) each_row[l] = 0
          }
          
        } #end of k loop
        
        #increment row number
        #assign each_row to aug_coeff_mat[row,]
        aug_coeff_mat[row,] = each_row;
        row = row + 1

      } #end of j loop
    }#end of i loop
    
    #dimnames
    counter = 1 #keeps track of the switching of letters, in the order: b c a
    counter2 = 1 #keeps track of the switching of numbers (b1, c1, a2, ...)
    dim_names = vector()
    for (i in 1:((3*n)-1)) {
      #1 - b
      #2 - c
      #3 - a

      if (isTRUE(counter == 1)) 
        letter = paste("b", counter2, sep="")
      if (isTRUE(counter == 2)) {
        letter = paste("c", counter2, sep="")
        counter2 = counter2 + 1
      }
      if (isTRUE(counter == 3)) 
        letter = paste("a", counter2, sep="")
      
      counter = counter + 1
      if (counter == 4) counter = 1
      
      dim_names[i] = letter
    } 
    #last dimname for rhs
    dim_names[3*n] = "RHS"
    
    #attach rhs
    aug_coeff_mat = cbind(aug_coeff_mat, rhs)
    
    #assign dimnames
    dimnames(aug_coeff_mat) <- list(dim_names[1:(3*n)-1], dim_names[1:(3*n)])
    
    #solve for the values
    gauss = GaussianElimination(aug_coeff_mat)
    
    #solutionSet
    solutionSet = vector()
    solutionSet[1] = 0 # a1 = 0
    solutionSet[2:(3*n)] = gauss$SolutionSet[1:(length(gauss$SolutionSet))]
    
    #set up polynomials per interval
    #y, fn(x), n = interval
    #fi(x) = aix^2 + bix + ci
    qsi.fxns = list()
    for (i in 1:n) {
      pol = paste("function (x) ", solutionSet[((2*i) + (i-2))], " * x^2 + ", solutionSet[((2*i) + (i-1))], " * x + ", solutionSet[3*i], sep="")
      qsi.fxns[[i]] = eval(parse(text = pol))
    }
    
    #getting the interval to use
    interval = 0

    for (i in 1:x_data_pts) {
      if (x >= x_data[i] && i != x_data_pts) {
        interval = interval + 1
      }
    }

    #evaluating based on the interval
    fxn = qsi.fxns[[interval]]
    y = round(fxn(x), digits = 2)
    
    #labeled list containing qsi.fxns and estimated y
    solution = list(qsi.fxns = qsi.fxns, y = y)
    
    return(solution)
  }
  else {
    return(NA)
  }
}
