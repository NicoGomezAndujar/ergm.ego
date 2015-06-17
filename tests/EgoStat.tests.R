library(ergm.ego)
library(ergm)

n <- 100
e <- 150
ds <- c(10,15,5,20)

y <- network.initialize(n, directed=FALSE)
y %v% "a" <- sample(1:3,n,replace=TRUE)
y <- san(y~edges+degree(0:3), target.stats=c(e,ds))

y.e <- as.egodata(y)

f <- ~ edges +
  nodecov("a") +
    
    nodefactor("a", 0) + nodefactor("a", 1) + nodefactor("a", 2) +
      
      nodematch("a") + nodematch("a", TRUE) + nodematch("a", TRUE, 2) +
        
        absdiff("a") + absdiff("a", 2) +
          
          degree(0) + degree(3) + degree(0:6) +
            degree(0, by="a") + degree(3, by="a") + degree(0:6, by="a") +
              degree(0, by="a", homophily=TRUE) + degree(3, by="a", homophily=TRUE) + degree(0:6, by="a", homophily=TRUE) +
                
                degrange(0) + degrange(3) + degrange(0:6) +
                  degrange(0) + degrange(3) + degrange(0:6) +
                    degrange(0, by="a") + degrange(3, by="a") + degrange(0:6, by="a") +
                      degrange(0, by="a", homophily=TRUE) + degrange(3, by="a", homophily=TRUE) + degrange(0:6, by="a", homophily=TRUE) +
                        
                        degrange(0,2) + degrange(3,5) + degrange(0:6,7) +
                          degrange(0,2) + degrange(3,5) + degrange(0:6,7) +
                            degrange(0,2, by="a") + degrange(3,5, by="a") + degrange(0:6,7, by="a") +
                              degrange(0,2, by="a", homophily=TRUE) + degrange(3,5, by="a", homophily=TRUE) + degrange(0:6,7, by="a", homophily=TRUE) +
                                
                                concurrent + concurrent("a") +
                                  
                                  concurrentties + concurrentties("a") +
                                    
                                    degreepopularity


f.y <- ergm.update.formula(f, y~.)
environment(f.y) <- globalenv()
f.y.e <- ergm.update.formula(f, y.e~.)
environment(f.y.e) <- globalenv()

stopifnot(all.equal(summary(f.y),summary(f.y.e)))
        