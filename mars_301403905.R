library(rpart)
library(earth)

set.seed(1)
x1 <- 1:100
x2 <- rnorm(100)
x3 <- rnorm(100)
y <- x1+rnorm(100)
data <- data.frame(y,x1,x2,x3)


new_node <- function(data,childl=NULL,childr=NULL){
  nn <- list(data=data,childl=childl,childr=childr)
  class(nn) <- "node"
  return(nn)
}

new_region <- function(coords=NULL,x,y){
  if(is.null(coords)) {
    coords <- sapply(x,range)
  }
  out <- list(coords=coords,x=x,y=y)
  class(out) <- "region"
  out
}

recpart <- function(x,y,debug=FALSE){
  init <- new_node(new_region(x=x,y=y))
  tree <- recpart_recursive(init,debug)
  class(tree) <- c("tree",class(tree))
  return(tree)
}

recpart_recursive <- function(node,debug=FALSE) {
  R <- node$data
  if(length(R$y) == 1) { return(node) }
  lof_best <- Inf
  for(v in 1:ncol(R$x)){
    tt <- split_points(R$x[,v])
    for(t in tt) {
      gdat <- data.frame(y=R$y,x=as.numeric(R$x[,v] <= t))
      lof <- LOF(y~.,gdat)
      if(lof < lof_best) {
        lof_best <- lof
        if(debug) best_split <- c(v=v,t=t)
        childRs <- split(R,xvar=v,spt=t)
      }
    }
  }
  if(debug) {
    cat("best split on variable",best_split["v"], "at", best_split["t"],"\n")
  }
  node$childl <- recpart_recursive(new_node(childRs$Rl),debug)
  node$childr <- recpart_recursive(new_node(childRs$Rr),debug)
  return(node)
}

split_points <- function(xv,Bm) {
  out <- sort(unique(xv[Bm>0]))
  return(out[-length(out)])
}

LOF <- function(form,data) {
  ff <- lm(form,data)
  return(sum(residuals(ff)^2))
}

split.region <- function(R,xvar,spt){
  r1_ind <- (R$x[,xvar] <= spt)
  c1 <- c2 <- R$coords
  c1[2,xvar] <- spt; c2[1,xvar] <- spt
  Rl <- new_region(c1,R$x[r1_ind,,drop=FALSE],R$y[r1_ind])
  Rr <- new_region(c2,R$x[!r1_ind,,drop=FALSE],R$y[!r1_ind])
  return(list(Rl=Rl,Rr=Rr))
}

fwd_stepwise <- function(formula, data){
  fwd_list = lm(formula, data)
  return(fwd_list)
}

bwd_stepwise <- function(bwd_in, data){ 
  bwd_list = step(bwd_in, data)
  return(bwd_list)
}

mars.control <- function(){
  control_list<-list()
  return(control_list)
}

mars <- function(formula, data, control){ 
  fwd_out <- fwd_stepwise(formula, data) 
  bwd_out <- bwd_stepwise(fwd_out, data) 
  return(bwd_out) 
}


?split
mars.control()
mars(y~., data=data)

testdata <- data
rm(data) # remove the original dataframe "data" from Global Environment.
mars(y ~., data = testdata)




print.region <- function(R,print.data=FALSE){
  cat("coordinates:\n") # \n indicates start the following in a new line
  print(R$coords)
  if(print.data){
    cat("y:\n")
    print(R$y)
    cat("x:\n")
    print(R$x)
  }
}

plot_regions.tree <- function(tree){
  plot(tree$data$x[,1],tree$data$x[,2],xlab="X1",ylab="X2") 
  plot_regions.node(tree$childl)
  plot_regions.node(tree$childr)
}

plot_regions.node<- function(node) {
  if(is.null(node))  return(NULL)
  x <- node$data$coords[,1]
  y <- node$data$coords[,2]
  rect(x[1], y[1], x[2], y[2], border="red")
  plot_regions.node(node$childl)
  plot_regions.node(node$childr)
}

set.seed(123); n <- 10
x <- data.frame(x1=rnorm(n),x2=rnorm(n))
y <- rnorm(n)
mytree <- recpart(x,y)
plot_regions.tree(mytree)


?rep

init_B <- function(n, Mmax){
  B <- data.frame( matrix(NA,nrow=n,ncol=(Mmax+1)) )
  B[,1] <- 1
  names(B) <- c("B0",paste0("B",1:Mmax))
  return(B)
}

init_B(1,3)

recpart_fwd <- function(y,x,Mmax){
  #---------------------------------------------------
  # Error checking for Mmax:(write your code below)
  # ...
  #---------------------------------------------------
  
  if(Mmax<2) {
    warning("Input Mmax must be >= 2; setting to 2")
    Mmax <- 2
  }
  
  # Initialize:
  N <- length(y) # sample size
  n <- ncol(x) # number of predictors = number of X
  # B: a data frame with optimal basis function as columns
  B <- init_B(N,Mmax) # Exercise: write init_B()
  # splits: a data frame records all the optimal (m, v, t):
  # m: parent basis func to split, v: splitting var, t: splitting point
  splits <- data.frame(m=rep(NA,Mmax),v=rep(NA,Mmax),t=rep(NA,Mmax))
  #
  #---------------------------------------------------
  # Looping for forward selection:
  for(M in 1:Mmax) { # contrast to indexing 2...Mmax in Friedman
    lof_best <- Inf
    for(m in 1:M) { # choose a basis function to split
      for(v in 1:n){ # select a variable to split on
        tt <- split_points(x[,v],B[,m]) # Exercise: write split_points() 
        for(t in tt) { 
          Bnew <- data.frame(B[,(1:M)[-m]], # drop m-th col: B[,-m]
                             # replace parent B[,m] with Btem1,Btem2
                             Btem1=B[,m]*(x[,v]>t), 
                             Btem2=B[,m]*(x[,v]<=t)) 
          gdat <- data.frame(y=y,Bnew)
          lof <- LOF(y~.,gdat) #  Use your LOF() from week 4
          if(lof < lof_best) { 
            lof_best <- lof
            splits[M,] <- c(m,v,t) 
          } # end if
        } # end loop over splits
      } # end loop over variables
    } # end loop over basis functions to split
    # save optimal (m, v, t) and update basis functions
    mstar <- splits[M,1]; vstar <- splits[M,2]; tstar <- splits[M,3]
    cat("[Info] best (m,v,t,lof): (",mstar,vstar,tstar,lof_best,")\n")
    B[,M+1] <- B[,mstar]*(x[,vstar]<=tstar) # 
    B[,mstar] <- B[,mstar]*(x[,vstar]>tstar)
  } # end loop over M
  return(list(B=B,splits=splits))
}
