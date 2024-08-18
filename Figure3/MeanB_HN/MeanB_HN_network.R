
setwd(readClipboard())
getwd()

library(igraph)
library(Hmisc)

B_PP1<- read.delim('MeanB_HN.txt', row.name = 1, check.names = FALSE)
dim(B_PP1)
head(B_PP1)
B_PP <- t(B_PP1)
dim(B_PP)

#######Correlation analysis########
B_PP_corr <- rcorr(B_PP, type = 'spearman')
B_PP_corr
r <- B_PP_corr$r
r[abs(r) < 0.6] <- 0
r[r > 0] <- 0
p <- B_PP_corr$P
write.csv(p,"MeanBaHN_p.csv")

p <- p.adjust(p, method = 'BH') 
p[p>=0.05] <- -1
p[p<0.05 & p>=0] <- 1
p[p==-1] <- 0 

z <- r * p
diag(z) <- 0
head(z)[1:6,1:6]

#write.table(data.frame(z, check.names = FALSE), 'MeanBaHN.matrix.txt',col.names = NA, sep = '\t', quote = FALSE)
##Check the matrix,and modify the format of the matrix
#######Network analysis########
A1<- read.delim('MeanBaHN.matrix.txt', row.name = 1, check.names = FALSE)
dim(A1)
A <- as.matrix(A1)
igraph <- graph.adjacency(A, weighted = TRUE, mode = 'undirected')
igraph
vcount(igraph)
igraph <- delete.vertices(igraph, names(degree(igraph)[degree(igraph) == 0]))
vcount(igraph)
E(igraph)$correlation <- E(igraph)$weight
E(igraph)$weight <- abs(E(igraph)$weight)
plot(igraph)

adj_matrix <- as.matrix(get.adjacency(igraph, attr = 'correlation'))
write.table(data.frame(adj_matrix, check.names = FALSE), 'MeanBaHN_network.adj_matrix.txt', col.names = NA, sep = '\t', quote = FALSE)

tax <- read.delim('MeanBaHN_group.txt', row.name = 1,
                  check.names = FALSE, stringsAsFactors = FALSE)
head(tax)
tax <- tax[as.character(V(igraph)$name), ]
head(tax)
V(igraph)$Genus <- tax$genus
V(igraph)$Group <- tax$group
igraph
plot(igraph)

edge <- data.frame(as_edgelist(igraph))
edge_list <- data.frame(
  source = edge[[1]],
  target = edge[[2]],
  weight = E(igraph)$weight,
  correlation = E(igraph)$correlation
)
head(edge_list)
write.table(edge_list, 'MeanBaHN.edge_list.txt', sep = '\t', row.names = FALSE, quote = FALSE)

node_list <- data.frame(
  label = names(V(igraph)),
  genus = V(igraph)$Genus,
  group = V(igraph)$Group)
head(node_list)
write.table(node_list, 'MeanBaHN.node_list.txt', sep = '\t', row.names = FALSE, quote = FALSE)


