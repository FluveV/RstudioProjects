# -----------------------------------------------------
# Filip Agneessens
# SCRIPT 2 
# 5 October 2023
# -----------------------------------------------------

# Activating packages

#install.packages("igraph")
library("igraph")
#install.packages("sna")
library("sna")

# Creating data
Simpsons_n<-matrix(c(0, 1, 1, 1, 0, 1, 0, 0, 0, 0,
                     1, 0, 1, 0, 1, 0, 1, 0, 0, 0,
                     1, 1, 0, 1, 1, 1, 1, 0, 0, 0,
                     1, 0, 1, 0, 0, 1, 0, 0, 0, 0,
                     0, 1, 1, 0, 0, 0, 1, 0, 0, 0,
                     1, 0, 1, 1, 0, 0, 1, 1, 0, 0,
                     0, 1, 1, 0, 1, 1, 0, 1, 0, 0,
                     0, 0, 0, 0, 0, 1, 1, 0, 1, 0,
                     0, 0, 0, 0, 0, 0, 0, 1, 0, 1,
                     0, 0, 0, 0, 0, 0, 0, 0, 1, 0),10,10)
# Adding names (node labels)
rownames(Simpsons_n)<-c("Ned","Marge","Homer","Abe","Maggie",
                        "Bart","Lisa","Krusty","Bob","Cecil")
colnames(Simpsons_n)<-c("Ned","Marge","Homer","Abe","Maggie",
                        "Bart","Lisa","Krusty","Bob","Cecil")

# Plotting
sna::gplot(Simpsons_n, gmode="graph", displaylabels = TRUE)
Simpsons_i<-graph_from_adjacency_matrix(Simpsons_n, mode = c("undirected"))
plot(Simpsons_i)

# Degree centrality
sna::degree(Simpsons_n, gmode="graph")
# it's better if we could add the names...
out1<-sna::degree(Simpsons_n, gmode="graph")
names(out1)<-row.names(Simpsons_n)
out1
igraph::degree(Simpsons_i)

# Density
sna::gden(Simpsons_n)
igraph::edge_density(Simpsons_i)

# Freeman's closeness
sna::closeness(Simpsons_n, gmode="graph")
igraph::closeness(Simpsons_i) # 1/geodesic
igraph::closeness(Simpsons_i, normalized=T)

# Freeman's closeness - second example
Simpsons_n2<-Simpsons_n
Simpsons_n2[9,10]<-0
Simpsons_n2[10,9]<-0
sna::closeness(Simpsons_n2, gmode="graph")
Simpsons_i2<-Simpsons_i
Simpsons_i2[9,10]<-0
igraph::closeness(Simpsons_i2, normalized=T)

# Reciprocal closeness
sna::closeness(Simpsons_n, gmode="graph", cmode="suminvundir")
out1<-sna::closeness(Simpsons_n, gmode="graph", cmode="suminvundir")
names(out1)<-row.names(Simpsons_n)
out1
# NOTE: in igraph this seems to be called harmonic mean
igraph::harmonic_centrality(Simpsons_i, normalized =TRUE)
#Y. Rochat, Closeness Centrality Extended to Unconnected Graphs: the Harmonic Centrality Index, ASNA 2009. https://infoscience.epfl.ch/record/200525

# Reciprocal closeness - second example
sna::closeness(Simpsons_n2, gmode="graph", cmode="suminvundir")
igraph::harmonic_centrality(Simpsons_i2, normalized =TRUE)

# Betweenness
sna::betweenness(Simpsons_n, gmode="graph")
igraph::betweenness(Simpsons_i)
igraph::betweenness(Simpsons_i, normalized=T)

#------
MAT4<-matrix(c(
  0, 1, 1, 0,
  1, 0, 1, 0,
  1, 1, 0, 1,
  0, 0, 1, 0),4,4)
MAT4%*%MAT4
MAT4%*%MAT4%*%MAT4

#------
MAT11<-matrix(c(
  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
  1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
  0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0),11,11)
gplot(MAT11)
eigen(MAT11)$values
1/max(eigen(MAT11)$values)
MAXBETA<-1/max(eigen(MAT11)$values)

# Power/Beta centrality
sna::bonpow(MAT11)
# doesn't work because exponent = 1 by default
?sna::bonpow
sna::bonpow(MAT11, exponent=.1)/sum(sna::bonpow(MAT11, exponent=.1))
GRAPH11<-graph_from_adjacency_matrix(MAT11,mode=c("undirected"),diag= FALSE)
igraph::bonpow(GRAPH11, exponent=.1)
igraph::bonpow(GRAPH11, exponent=.2)
igraph::bonpow(GRAPH11, exponent=.3)
igraph::bonpow(GRAPH11, exponent=.4)
igraph::bonpow(GRAPH11, exponent=.5)
igraph::bonpow(GRAPH11, exponent=MAXBETA)
igraph::bonpow(GRAPH11, exponent=MAXBETA-.001)

OUT2<-matrix(0,11,7)
OUT2[,1]<-igraph::bonpow(GRAPH11, exponent=-.2)/sum(igraph::bonpow(GRAPH11, exponent=-.2))
OUT2[,2]<-igraph::bonpow(GRAPH11, exponent=-.1)/sum(igraph::bonpow(GRAPH11, exponent=-.1))
OUT2[,3]<-igraph::bonpow(GRAPH11, exponent=.0)/sum(igraph::bonpow(GRAPH11, exponent=.0))
OUT2[,4]<-igraph::bonpow(GRAPH11, exponent=.1)/sum(igraph::bonpow(GRAPH11, exponent=.0))
OUT2[,5]<-igraph::bonpow(GRAPH11, exponent=.2)/sum(igraph::bonpow(GRAPH11, exponent=.2))
OUT2[,6]<-igraph::bonpow(GRAPH11, exponent=.3)/sum(igraph::bonpow(GRAPH11, exponent=.3))
OUT2[,7]<-igraph::bonpow(GRAPH11, exponent=.4)/sum(igraph::bonpow(GRAPH11, exponent=.4))
OUT2
plot(OUT2[1,], type="o", col="blue")
plot(OUT2[4,], type="o", col="red")

library(network)
data("flo", package = "network")
