############################################################################
cat("Correlations and dimension reduction \n")
############################################################################
library(psych)
## melting a correlation matrix to compute correlations between all columns in R and detect highly correlated variables
conversion.data <- read.csv("data/data_processed/conversion_data.csv")
conversion.data[c("X", "X.1")] <- NULL

data.lca <- read.csv("data/data_processed/data_lca.csv")
data.lca[c("X", "X.1")] <- NULL
data <- data.lca

############################################################################
cat("Remove spatial identifier \n")

data <- data[, sapply(data, function(col) length(unique(col))) > 1]
a <- grep("^coal2id$", colnames(data.lca))
data <- data.lca[,(a+1):length(data.lca)]
data <- data.frame(lapply(data, as.numeric))
b <- colnames(data)




############################################################################
cat("Identify different correlation intensities among variables \n")

d_cor <- as.matrix(cor(data), method="spearmen")
d_cor_melt <- arrange(melt(d_cor), -abs(value))
d_cor_melt <- d_cor_melt[!(d_cor_melt$Var1==d_cor_melt$Var2), ] ## both directions of correlation are identified, so one can be removed

d_cor_strong <- d_cor_melt[(d_cor_melt$value >= 0.8 | d_cor_melt$value <= -0.8), ]
d_cor_strong <- d_cor_strong[ (duplicated(d_cor_strong$value)==FALSE), ]

d_cor_high <- d_cor_melt[(d_cor_melt$value >= 0.6 &  d_cor_melt$value < 0.8 | d_cor_melt$value <= -0.6 & d_cor_melt$value > -0.8), ]
d_cor_high <- d_cor_high[ (duplicated(d_cor_high$value)==FALSE), ]

d_cor_medium <- d_cor_melt[(d_cor_melt$value >= 0.4 &  d_cor_melt$value < 0.6 | d_cor_melt$value <= -0.6 & d_cor_melt$value > -0.6), ]
d_cor_medium <- d_cor_medium[ (duplicated(d_cor_medium$value)==FALSE), ]

d_cor_weak <- d_cor_melt[(d_cor_melt$value >= 0.2 &  d_cor_melt$value < 0.4 | d_cor_melt$value <= -0.2 & d_cor_melt$value > -0.4), ]
d_cor_weak <- d_cor_weak[ (duplicated(d_cor_medium$value)==FALSE), ]



############################################################################
cat("Visualizing correlation network \n")

## network
library(igraph)



############################################################################
cat("Prepare network data: Links and nodes \n")

links <- d_cor_melt[(d_cor_melt$value >= 0.2 | d_cor_melt$value <= -0.2), ]
links <- links[ (duplicated(links$value)==FALSE), ]
links <- na.omit(links)

## igraph pictures nodes with negative edge over each other
## solution: 
## 1) add column to identify if correlation is pos or neg
## 2) remove mathematical sign in correlation
## 3) color edge in graph based on mathematical sign to show if correlation is positiv or negativ
colnames(links) <- c("from", "to", "weight")
links$sign <- 'neg'
links[(links$weight > 0),'sign'] <- 'pos'
links$weight <- abs(links$weight) # remove sign

links$intensity <- "weak"
links[(links$weight >= 0.4),'intensity'] <- 'medium'
links[(links$weight >= 0.6),'intensity'] <- 'high'
links[(links$weight >= 0.8),'intensity'] <- 'strong'
links$color <- "#e4ff56"
links[(links$intensity == 'medium'),'color'] <- '#f4e33b'
links[(links$intensity == 'high'),'color'] <- '#e3ad2c'
links[(links$intensity == 'strong'),'color'] <- '#e39824'

a <- as.character(unique(links$from))
b <- as.character(unique(links$to))
nodes <- data.frame(c(a,b))
nodes <- unique(nodes)
colnames(nodes) <- "id"


net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 




############################################################################
cat("visualization of basic correlations \n")

## color for positive and negative correlation
plot(net, vertex.shape= "circle", edge.color=c("#b82b2b","#71965a")[(E(net)$sign=="neg")+1],
     edge.arrow.size=.2, edge.curved=0, edge.width=E(net)$weight*7, 
     vertex.color="white", vertex.frame.color="#555555",
     vertex.label.color="black",
     vertex.label.cex=.8,
     main = "Positive correlation = green, negative correlation = red") 


## color for medium and strong correlation
plot(net, vertex.shape= "circle", edge.color=E(net)$color,
     edge.arrow.size=.2, edge.curved=0, edge.width=E(net)$weight*7, 
     vertex.color="white", vertex.frame.color="#555555",
     vertex.label.color="black",
     vertex.label.cex=.8,
     main = "Intensity of correlation, the higher/lower correlation coefficient the darker edge")


## cliques (fully connected subgroup)
net.sym <- as.undirected(net, mode= "collapse",
                         edge.attr.comb=list(weight="sum", "ignore"))
cliques(net.sym) # list of cliques       
sapply(cliques(net.sym), length) # clique sizes
largest_cliques(net.sym)
vcol <- rep("white", vcount(net.sym))
vcol[unlist(largest_cliques(net.sym))] <- "gold"

plot(as.undirected(net.sym), vertex.shape= "circle", edge.color=E(net)$color,
     edge.arrow.size=.2, edge.curved=0, edge.width=E(net)$weight*7, 
     vertex.color=vcol, vertex.frame.color="#555555",
     vertex.label.color="black",
     vertex.label.cex=.8,
     main = "Cliques (fully connected variables)")


## community detection based on hierarchical clustering
deg <- degree(net, mode="all") ## degree
ceb <- cluster_edge_betweenness(net) 
dendPlot(ceb, mode="hclust") ## dendrogram plot of dependencies

colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(ceb, as.undirected(net), vertex.shape= "circle", 
     edge.arrow.size=.2, edge.curved=0, edge.width=E(net)$weight*7, 
     vertex.color="white", vertex.frame.color="#555555",
      vertex.label.color="black",
     vertex.label.cex=.8,
     main = "Community detection based on hierarchical clustering") 

length(ceb) # number of communities
membership(ceb) # community membership
modularity(ceb) # how modular the graph partitioning is
crossing(ceb, net) # boolean vector: TRUE for edges across communities

## community detection based on hierarchical clustering
cfg <- cluster_fast_greedy(as.undirected(net))

plot(cfg, as.undirected(net), vertex.shape= "circle", 
     edge.arrow.size=.2, edge.curved=0, edge.width=E(net)$weight*7, 
     vertex.color="white", vertex.frame.color="#555555",
     vertex.label.color="black",
     vertex.label.cex=.8,
     main = "Community detection based on Greedy optimization of modularity") 


## only edges where correlation is >= 0.6 or <= -0.6
net.sp <- delete_edges(net, E(net)[weight<=0.6])

png("out/data_exploration/highly_corr_variables.png", 2000, 2000, res = 300)
#png("my_plot.png", 600, 600)
plot(net.sp, vertex.shape= "circle", edge.color=c("#b82b2b","#71965a")[(E(net)$sign=="neg")+1],
     edge.arrow.size=.2, edge.curved=0, edge.width=E(net)$weight*7, 
     vertex.color="white", vertex.frame.color="#555555",
     vertex.label.color="black",
     vertex.label.cex=.8,
     main = "Only edged where correlation coefficient is higher than 0.6 or lower than -0.6") 
dev.off()

## only edges where correlation is >= 0.70 or <= -0.70
net.sp <- delete_edges(net, E(net)[weight<=0.70])

plot(net.sp) 
plot(net.sp, vertex.shape= "circle", edge.color=c("#b82b2b","#71965a")[(E(net)$sign=="neg")+1],
     edge.arrow.size=.2, edge.curved=0, edge.width=E(net)$weight*7, 
     vertex.color="white", vertex.frame.color="#555555",
     vertex.label.color="black",
     vertex.label.cex=.8,
     main = "Only edged where correlation coefficient is higher than 0.7 or lower than -0.7") 

rm(links, nodes, ceb, cfg, deg, net, net.sp, net.sym, vcol, colrs, a, b)



############################################################################
cat("Remove temporal and spatial variables because of potential autocorrelation and high intercorrelations \n")

## remove spatial variables
data$CountryOrigincat <- NULL
data$dem_birth_countrycat <- NULL

## remove temporal variables
data$RefugeeStatusDatecat <- NULL ## redundant with YearArrival and status span
data$YearArrivalcat <- NULL



############################################################################
cat("Identify correlated variables with correlation > 0.70 \n")

d_cor <- as.matrix(cor(data), method="spearmen")
d_cor_melt <- arrange(melt(d_cor), -abs(value))
d_cor_melt <- d_cor_melt[!(d_cor_melt$Var1==d_cor_melt$Var2), ]

## here only the strongest correlations will be removed, correlation coefficient might be chanes
d_cor_strong <- d_cor_melt[(d_cor_melt$value >= 0.70 | d_cor_melt$value <= -0.70), ]
d_cor_strong <- d_cor_strong[ (duplicated(d_cor_strong$value)==FALSE), ]
d_cor_strong <- na.omit(d_cor_strong)


############################################################################
cat("Investigate kind of correlation before removing one of two high correlated columns\n")
## by kind of correlation conclusions can be drawn after LCA

a <- c(d_cor_strong$Var1)
b <- c(d_cor_strong$Var2)
cor_from <- colnames(data[a])
cor_to <- colnames(data[b])
conversion.back <- conversion.data[,c(1,2,4,3)] ## conversion of lca coded values back to breaks of equally sized groups
corplot.list <- list()

for (i in 1:length(a)) {
M <- cor(data[cor_from[i]], data[cor_to[i]], method="spearman")
B <- cbind(data[cor_from[i]], data[cor_to[i]]) ## subset of correlated columns

#conversion.data$lca <- as.numeric(conversion.back$lca)
conversion.sub.from <- conversion.back[(conversion.back$variable == cor_from[i]), 3:4]
conversion.sub.from <- unique(conversion.sub.from)
conversion.sub.to <- conversion.back[(conversion.back$variable == cor_to[i]), 3:4]
conversion.sub.to <- unique(conversion.sub.to)

B[,cor_from[i]] <- B[,cor_from[i]] %l% conversion.sub.from
B[,cor_to[i]] <- B[,cor_to[i]] %l% conversion.sub.to
TB <- as.matrix(table(B))
TB <- TB/sum(TB)

brewer.div <- colorRampPalette(brewer.pal(9, "Blues"))
corplot.list[[i]] <- levelplot (TB, col.regions = brewer.div(200), aspect = "iso",scale=list(x=list(rot=45)))


data[a[i]] <- NULL ## remove one of the high correlated columns from the data frame
}

rm(conversion.sub.from, conversion.sub.to, B, a, b, TB, cor_from, cor_to)

############################################################################
cat("Check final correlations\n")

d_cor <- as.matrix(cor(data), method="spearmen")
d_cor_melt <- arrange(melt(d_cor), -abs(value))
d_cor_melt <- d_cor_melt[!(d_cor_melt$Var1==d_cor_melt$Var2), ]
max(d_cor_melt[,3])

data.lca.numeric <- data

rm(d_cor, d_cor_high, d_cor_medium, d_cor_strong, d_cor_weak, d_cor_melt, M)
rm(conversion.catdata.lca, conversion.catdata, conversion.sub, data.lca.coded)

write.csv(conversion.back, file = "data/data_processed/conversion_back.csv")
write.csv(data.lca.numeric, file = "data/data_processed/data_lca_numeric.csv")
