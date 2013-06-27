# Simple Bootstrapping for Hierarchical Clustering


## Install

```s
install.packages("devtools")
library("devtools")
install_github("bootstrap", "sgibb")
```

## Example

```s
## load library
library("bootstrap")

## hclust example
createHclustObject <- function(x)hclust(dist(x), "ave")

## bootstrap
b <- bootstrap(USArrests, fun=createHclustObject, n=100L)

## plot
hc <- createHclustObject(USArrests)
plot(hc)

## draw bootstrap values to corresponding node
bootlabels.hclust(hc, b, col="blue")
```
![cluster dendrogram](http://i.imgur.com/BXMVdAV.png)
