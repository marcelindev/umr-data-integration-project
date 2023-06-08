## computes plis intersection between combination of columns
plis <- function(data,combination) {
  # Create PLI for columns 
  pli_list <- list()
  for (i in 1:length(combination)) {
    pli_list <- append(pli_list,split(1:nrow(data), data[,combination[i]]))
  }
  
  
  pli_list <- lapply(pli_list, function(pli){
    pli[length(pli) > 1]
  })
  
  combinations <- combn(pli_list, 2, simplify = FALSE)  # Generate all combinations of 2 elements
  common_elements <- lapply(combinations, function(x) Reduce(intersect, x))
  
  common_elements <- lapply(common_elements, function(pli){
    pli[length(pli) > 1]
  })
  filtered_list <- Filter(function(x) length(x) > 0, common_elements)
  
  if(length(filtered_list) == 0) return(TRUE)
  return(FALSE)
}

## creates lattice for dataset
lattice <- function(ds) {
  variables <- ncol(ds)
  lat <- list()
  cnames <- colnames(ds)
  for (i in 1:variables) {
    
    lat[[i]] <- combn(cnames,i)
  }
  return(lat)
}



### finding unic column combinations using apriori approach
ucc_func <- function(df,df_lattice){
  ucc <- list()
  i <- 1
  for(level in 1:length(df_lattice)) {
    
    
    level_matrix <-df_lattice[[level]]
    
    for(j in 1:ncol(level_matrix)){
      cat("testing : ",level_matrix[,j],"\n")
      if(length(level_matrix[,j]) == 1) to_pass <- c(level_matrix[,j],level_matrix[,j])
      else to_pass <- level_matrix[,j]
      
      if(plis(df,to_pass) == T ){
        ucc[[i]] <- to_pass
        i <- i+1
      }
    }
    
    
  }
  
  return(ucc)
}

## create plis 
create_pli <- function(data,combination){
  pli_list <- list()
  for (i in 1:length(combination)) {
    pli_list <- append(pli_list,split(1:nrow(data), data[,combination[i]]))
  }
  if(length(combination) == 1) return(pli_list)
  else {
    
    pli_list <- lapply(pli_list, function(pli){
      pli[length(pli) > 1]
    })
    
    combinations <- combn(pli_list, 2, simplify = FALSE)  # Generate all combinations of 2 elements
    common_elements <- lapply(combinations, function(x) Reduce(intersect, x))
    
    common_elements <- lapply(common_elements, function(pli){
      pli[length(pli) > 1]
    })
    filtered_list <- Filter(function(x) length(x) > 0, common_elements)
    
    return(filtered_list)
  }
}


### find fds 
### for validation of fd 
check_fd <- function(ds,lhs,rhs){
  pli_rhs <- create_pli(ds,rhs)
  pli_lhs <- create_pli(ds,lhs)
  
  intersection <- Reduce(intersect,list(pli_rhs,pli_lhs))
  if(setequal(pli_lhs,intersection)) return(T)
  else return(F)
}

find_fds <- function(ds) {
  lat <- lattice(ds)
  maxi <- length(lat)-1
  for (i in 1:maxi) {
    level <- lat[[i]]
    next_level <- lat[[i+1]]
    for (j  in 1:ncol(level)) {
      lhs <- level[,j]
      for (x  in 1:ncol(next_level)) {
       
        if(all(as.vector(lhs) %in% as.vector(next_level[,x]))){
          rhs <- next_level[,x]
          
          if(check_fd(ds,lhs,rhs)) cat("found fd : ",lhs,"->",rhs,"\n")
        }
      } 
      
    }
  }
  
}


### find source dataset(not needed anymore after country code library)
find_source <- function(entry) {
  if(entry %in% unique(toupper(unemployement_2015_2019$Country))) return("unemployement")
  if(entry %in% unique(toupper(happiness_2015_2019$Country))) return("happiness")
  if(entry %in% unique(toupper(poverty_2015_2019$Country))) return("poverty") 
  return("none")
}


