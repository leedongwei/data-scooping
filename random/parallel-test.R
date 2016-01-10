library(doParallel)


cl<-makeCluster(4)
registerDoParallel(cl)

a <- rep(0, 10)

ptrack <- proc.time()

foreach(i=1:10) %dopar% {
  print (c("start i:", i))
  
  p1 <- proc.time()
  Sys.sleep(5)
  proc.time() - p1
  
  print (c("ended i:", i))
  
  return (proc.time() - p1)
}

print ("FINAL")
print (proc.time() - ptrack)

stopCluster(cl)
