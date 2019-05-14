library(data.table)
irisk <- iris

set.seed(666)
kres <- kmeans(iris[c(3,4)], 3, 20)$cluster
kres[kres == 1] <- "A"
kres[kres == 2] <- "B"
kres[kres == 3] <- "C"

irisk <- cbind(irisk, kres)
colnames(irisk)[6] <- "Cluster"

fwrite(irisk, file = "./data/iris.txt", sep = "\t")
fwrite(irisk, file = "./data/iris.csv", sep = ";")
