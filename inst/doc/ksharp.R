## ---- echo=FALSE--------------------------------------------------------------
suppressMessages(library(Rcssplot))
library(cluster)
library(dbscan)
library(ksharp)
set.seed(300299)
RcssDefaultStyle <- Rcss("ksharp.Rcss")

## ---- echo=FALSE--------------------------------------------------------------
dbscan.named = function(data, eps) {
  result = dbscan(data, eps=eps)
  names(result$cluster) = rownames(data)
  result$data = data
  result
}

## ---- echo=FALSE--------------------------------------------------------------
plotClusters <- function(data, clusters=NULL, main="", Rcssclass="sharp") {

  # when clusters not specified, assume all belong to class "plain"
  if (is.null(clusters)) {
    clusters = setNames(rep("plain", nrow(data)), rownames(data))
  }
  if (is.null(names(clusters))) {
    names(clusters) = rownames(data)
  }

  RcssCompulsoryClass = RcssGetCompulsoryClass(Rcssclass)

  titlepos = RcssValue("ksharp", "title.pos", default=c(0, 1))
  padding = RcssValue("ksharp", "padding", default=0.1)

  xlim = range(data[,1])
  ylim = range(data[,2])
  if (xlim[2]-xlim[1] > ylim[2]-ylim[1]) {
    ylim = xlim
  } else {
    xlim = ylim
  }
  xysize = xlim[2]-xlim[1]
  xylim = xlim+(c(-1,1)*xysize*padding)
  xysize = xylim[2]-xylim[1]
  rm(xlim, ylim)	

  plot(xylim, xylim)
  rect(xylim[1], xylim[1], xylim[2], xylim[2])
  
  # create cluster names like C0, C1, etc
  cnames = paste0("C", clusters)
  clusters = split(names(clusters), cnames)

  # add points to the chart
  for (iname in names(clusters)) {
    idata = data[clusters[[iname]], , drop=FALSE]
    points(idata[,1], idata[,2], Rcssclass=iname)
  }

  text(xylim[1] + titlepos[1]*xysize,
       xylim[1] + titlepos[2]*xysize,
       main, Rcssclass="main")
}
emptyplot = function(n) {
  for (i in 1:n) {
    plot(c(0, 1), c(0, 1), type="n")
  }
}

## ---- echo=FALSE--------------------------------------------------------------
# override table to remove spurious empty line
table = function(x) {
  x = base::table(x)
  width = 5
  padspaces = function(y) {
    if (length(y)>1) {
      return(sapply(y, padspaces))
    }
    y = as.character(y)
    paste(c(rep(" ", width-nchar(y)), y), collapse="")
  }

  line1 = paste(sapply(names(x), padspaces), collapse="")
  line2 = paste(sapply(x, padspaces), collapse="")
  cat(line1, "\n")
  cat(line2, "\n")
}

## ---- echo=FALSE--------------------------------------------------------------
K1 = kdata.1
km1 = kmeans(K1, 2)

## ---- echo=FALSE, fig.width=7.2, fig.height=1.8-------------------------------
oldpar = par(mfrow=c(1,4))
plotClusters(K1, main="raw data", Rcssclass="unsharp")
plotClusters(K1, km1$cluster, main="kmeans (k=2)", Rcssclass="unsharp")
emptyplot(2)
# this vignette uses Rcssplot, which changes the behavior of par a little
# resetting par is achieved either via graphics:: or dev.off()
graphics::par(oldpar) 

## ---- echo=FALSE, fig.width=7.2, fig.height=1.8-------------------------------
sharp1 = ksharp(km1, data=K1)
oldpar = par(mfrow=c(1,4))
thresholds = c(0.05, 0.1, 0.2, 0.4)
for (t in thresholds) {
  plotClusters(K1, ksharp(sharp1, t, method="sil")$cluster, main=paste0("k=2, threshold=", t))
}
graphics::par(oldpar)

## -----------------------------------------------------------------------------
library(ksharp)
km1 = kmeans(kdata.1, centers=2)
table(km1$cluster)

## -----------------------------------------------------------------------------
ks1 = ksharp(km1, threshold=0.05, data=kdata.1)
table(ks1$cluster)

## -----------------------------------------------------------------------------
ks1.10 = ksharp(ks1, threshold=0.1)
table(ks1.10$cluster)
ks1.20 = ksharp(ks1, threshold=0.2)
table(ks1.20$cluster)

## ---- echo=FALSE--------------------------------------------------------------
oldpar = par(mfrow=c(3,4))
methods = c("silhouette", "medoid", "neighbor")
for (m in methods) {
  for (t in thresholds) {
    plotClusters(K1, ksharp(sharp1, t, method=m)$cluster, main=paste0(m, ", ", t))
  }
}
graphics::par(oldpar)

## ---- echo=FALSE--------------------------------------------------------------
K2 = kdata.2
K2dist = dist(K2)
km2 = kmeans(K2, 2)
db2 = dbscan.named(K2, 0.3)
sharp2 = ksharp(db2, method="neighbor")

## ---- echo=FALSE, fig.width=7.2, fig.height=1.8-------------------------------
oldpar = par(mfrow=c(1,4))
plotClusters(K2, cluster=NULL, main="raw", Rcssclass="unsharp")
plotClusters(K2, km2$cluster, main="kmeans, k=2", Rcssclass="unsharp")
plotClusters(K2, db2$cluster, main="dbscan, k=2", Rcssclass="unsharp")
emptyplot(1)
graphics::par(oldpar)

## ---- echo=FALSE--------------------------------------------------------------
oldpar = par(mfrow=c(3,4))
for (m in methods) {
  for (t in thresholds) {
    plotClusters(K2, ksharp(sharp2, t, method=m)$cluster, main=paste0(m, ", ", t))
  }
}
graphics::par(oldpar)

## ---- echo=FALSE, fig.height=1.8----------------------------------------------
K3 = kdata.3
db3 = dbscan.named(K3, 0.25)
oldpar = par(mfrow=c(1,4))
plotClusters(K3, cluster=NULL, main="raw", Rcssclass="unsharp")
plotClusters(K3, db3$cluster, main="dbscan", Rcssclass="unsharp")
emptyplot(2)
graphics::par(oldpar)

## ---- echo=FALSE--------------------------------------------------------------
sharp3 = ksharp(db3, data=K3, method="neighbor")
oldpar = par(mfrow=c(3,4))
for (m in methods) {
  for (t in thresholds) {
    plotClusters(K3, ksharp(sharp3, t, method=m)$cluster, main=paste0(m, ", ", t))
  }
}
graphics::par(oldpar)

## ---- echo=FALSE--------------------------------------------------------------
K4 = kdata.4
K4.centers = matrix(c(-3,-3,-3,3,3,-3,3,3), ncol=2, byrow=T)
km4 = kmeans(K4, K4.centers)
sharp4 = ksharp(km4, data=K4)

## ---- echo=FALSE--------------------------------------------------------------
oldpar = par(mfrow=c(3,4))
thresholds2 = c(0.05, 0.1, 0.2, 0.4)
for (m in methods) {
  for (t in thresholds2) {
    plotClusters(K4, ksharp(sharp4, t, method=m)$cluster, main=paste0(m, ", ", t))
  }
}
graphics::par(oldpar)

## -----------------------------------------------------------------------------
sessionInfo()

