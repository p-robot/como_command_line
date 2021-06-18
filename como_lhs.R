
library(lhs)

# Function to create LHS sample of model input parameters
como_lhs <- function(nsamples, param_mins, param_maxs){

nparams <- length(param_mins)

# Create uniform LHS
lhs <- maximinLHS(nsamples, nparams, dup = 1, method = "build")

param_ranges <- (param_maxs - param_mins)

# Adjust LHS to limits of parameters
mat_ranges <- matrix(param_ranges, nrow = nsamples, ncol = nparams, byrow = TRUE)
mat_mins <-  matrix(param_mins, nrow = nsamples, ncol = nparams, byrow =TRUE)

param_lhs <- lhs*mat_ranges + mat_mins

return(param_lhs)
}

# Pass a list to create an LHS, returns a data.frame
# 
# Example
# -------
# 
# params <- list(
# 	"assortativity" = list(min = 0, max = 1), 
# 	"average_annual_hazard" = list(min =2.65, max = 5.3)
# )
# como_lhs_list(10, params)
# 
como_lhs_list <- function(nsamples, param_list){
	
	param_mins <- unlist(lapply(param_list, '[[', "min"))
	param_maxs <- unlist(lapply(param_list, '[[', "max"))

	lhs <- como_lhs(nsamples, param_mins, param_maxs)
	lhs <- as.data.frame(lhs)
	names(lhs) <- names(param_list)
	return(lhs)
}

