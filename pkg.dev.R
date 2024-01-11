source("~/Desktop/sreg/creg.R")
source("~/Desktop/sreg/sreg.R")

sreg <- function(Y,S,D,G.id = NULL, Ng = NULL, X=NULL, exp.option = FALSE)
{
  if (is.null(G.id) | is.null(Ng))
  {
    result <- res.sreg(Y,S,D,X)
    summary.sreg(result)
  }else{
    result <- res.creg(Y,S,D,G.id,Ng,X, exp.option = FALSE)
    summary.creg(result)
  }
  return(result)
}

test <- sreg(Y,S,D,X)
testc <- sreg(Y,S,D,G.id,Ng,X)

