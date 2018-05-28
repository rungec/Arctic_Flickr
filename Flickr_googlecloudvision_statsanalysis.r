#this script performs descriptive and statistic analysis on the Google Cloud Vision https://cloud.google.com/vision/ labels for the Arctic Flickr photos
#follows on from Flickr_googlecloudvision_label.r and Flickr_googlecloudvision_postprocessing.r
# http://kateto.net/network-visualization
# http://www.rdatamining.com/docs/text-mining-with-r
# https://stackoverflow.com/questions/9471906/what-are-the-differences-between-community-detection-algorithms-in-igraph#9478989
# https://www.r-bloggers.com/summary-of-community-detection-algorithms-in-igraph-0-6/ 

### Set up libraries ----
require(sf)
require(tidyverse)
#require(ExPosition)
#require(Rgraphviz)

options(stringsAsFactors = FALSE)

wd <- "D:/Box Sync/Arctic/CONNECT/Paper_3_Flickr/Analysis/tag_analysis/output"
setwd(wd)

#load data
load("Flickr_Artic_60N_plus_flickrandgooglelabels_userinfo_urban.Rdata")
#flickrshp - flickr data for each photo plus user classifications, what region and whether urban

#drop urban
flickrshp <- flickrshp[is.na(flickrshp$InCity), ]

##################
#Contingency table
subdat <- flickrshp[1:10000, grep("googletag", names(flickrshp))] %>% st_set_geometry(NULL) 
subdat <- subdat[rowSums(is.na(subdat))<length(subdat), ] #drop rows with all NAs

wordL <- lapply(1:nrow(subdat), function(i) {
  currow <- unname(unlist(subdat[i, !is.na(subdat[i,])])) %>% droplevels()
  d <- expand.grid(currow, currow)
  return(d)
})

wordDF <- do.call(rbind, wordL)
write.csv(wordDF, "Googlevision_nourban_long.csv", row.names=FALSE)
ctbl <- table(wordDF)
save(ctbl, file="Googlevision_contingency_table_nourban.Rdata")


#words we might drop
#food
#dog but not sled dog/husky
#urban/building/metropolitan area
#car

###################
#Plot heatmap ----
###################
load(file="Googlevision_contingency_table_nourban.Rdata")
require(ggplot2)
wdf <- as.data.frame(ctbl)
names(wdf) <- c("Word1", "Word2", "Freq")
wdf <- wdf[wdf$Freq>0, ]

ggplot(wdf, aes(Word1, Word2)) +
  geom_tile(aes(fill=Freq)) +
  scale_fill_gradient(name = 'log co-mentions', low = 'royalblue', high = 'gold', trans='log10') + 
  theme(axis.title.y = element_blank(), axis.title.x = element_blank(), 
        axis.text.x =element_text(size=1, angle=90, hjust=1), axis.text.y=element_text(size=1))
ggsave("Googlevision_contingency_table_heatmap.pdf", width=20, height=20, units=c("in"))

diag(ctbl) <- 0
require(corrplot)
corrplot(ctbl, method='color', type='lower', diag = FALSE, order="original", tl.cex=0.5, is.corr = FALSE)




###################
#Make network diagrams ----
###################
require(threejs) #for interactive igraph
require(htmlwidgets)
require(igraph) #for network diagram
require(wesanderson) #for colors

ctbl <- load("Googlevision_contingency_table_nourban.Rdata")
samplerows <- sample(1:nrow(ctbl), 1000)
ctbl <- ctbl[samplerows, samplerows]

#function to find words associated with a keyword
findfun <- function(word, threshold_freq) {
  a <- ctbl[which(dimnames(ctbl)$Var2==word), ] #pull the 'word' row
  assocwords <- names(a[a > threshold_freq]) #drop words with freq less than threshold_freq
  return(assocwords)
} 

#classify words & make a node table
catwords <- c("food", "urban", "dog", "car", "art")
thresholds <- c(0, 0, 0, 0, 0)
nodes <- data.frame(id=dimnames(ctbl)$Var1, category=c("none"), category_index=1)
for (i in seq_along(catwords)){
  currwords <- findfun(catwords[i], thresholds[i]) #extract words associated with that one
  nodes$category[nodes$id %in% currwords] <- catwords[i]
  nodes$category_index[nodes$id %in% currwords] <- i+1
}
write.csv(nodes, "Googlevision_words_categories.csv", row.names=FALSE)

###################  
#make igraph object
word_gr <- graph_from_adjacency_matrix(ctbl, mode=c("upper"), weighted=TRUE, diag=FALSE) 
#there are some cases where google vision returned the same word twice for a photo, diag=FALSE drops these
#weighted=TRUE means the values in the adjacency matrix gives the weight of the connections (edges) between the words (vertices/nodes)
#mode="upper" creates an undirected graph using only the upper right triangle of the matrix
#add the categories
V(word_gr)$category <- nodes$category[match(V(word_gr)$name, nodes$id)]
V(word_gr)$category_index <- nodes$category_index[match(V(word_gr)$name, nodes$id)]

###################
# Plot the degree distribution for our network:
#(a measure of how many connections each word has)
deg.dist <- degree_distribution(word_gr, cumulative=T, mode="all")
deg <- degree(word_gr, mode="all") # Compute node degrees (#links)
write.csv(deg, "Googlevision_degree_centrality.csv", row.names=TRUE)
write.csv(data.frame(degree=0:max(deg), freq=deg.dist), "Googlevision_degree_distribution.csv", row.names=FALSE)
png("Googlevision_degree_distribution.png")
  plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")
  dev.off()
png("Googlevision_degree_distribution_zoom.png")
  plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency", xlim=c(0,50))
  dev.off()

###################
#plot
#set colour of nodes to match the category word
mycols <- c("grey70", wes_palette("Darjeeling", 5))
V(word_gr)$color <- mycols[V(word_gr)$category_index]
V(word_gr)$vertex.label.color <- mycols[V(word_gr)$category_index]
#normalise edge width (range from 1 to 4)
E(word_gr)$width <- 1 + 3*E(word_gr)/max(E(word_gr))

png("Googlevision_network_diagram_static_test.png", width=480, height=480)
plot(word_gr, vertex.shape="circle", vertex.size=1, vertex.frame.color="#ffffff", vertex.label.font=1, vertex.label.color="black",
     edge.color="grey70")
dev.off()  
  
  
###################
#plot static network

l <- layout_with_fr(word_gr)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
#l <- layout_with_kk(word_gr)
png("Googlevision_network_diagram_static.png", width=480, height=480)
plot(word_gr, 
     #rescale=F, layout=l*0.5, 
     vertex.shape="circle", vertex.frame.color="#ffffff", 
     vertex.label.font=1, vertex.label.color="black", 
     vertex.label.degree=0, vertex.label.cex=0.5, vertex.label.dist=1,
     edge.color="grey70")
dev.off()



###################
#plot interactive

fastgreedy.community(g)
res_g <- simplify(contract(g, membership(c_g)))
mem <- data.frame(vertices = 1:vcount(g), memeber = as.numeric(membership(c_g)))



m <- graphjs(word_gr, 
             vertex.color = V(word_gr)$color, vertex.size=1, vertex.shape="circle",
             
             
             showLabels=T, stroke=F, 
attraction=0.9, repulsion=0.8, opacity=0.9)

saveWidget(m, file="Googlevision_network_diagram_interactive.html",selfcontained = FALSE)


###################
#Drop the obvious non-ecosystem services
#there are some cases where google vision returned the same word twice for a photo, we drop these
diag(ctbl) <- 0

#Function to find word associations
findAssocsBig <- function(u, term, corlimit){
  suppressWarnings(x.cor <-  gamlr::corr(t(u[ !u$dimnames$Terms == term, ]),        
                                         as.matrix(t(u[  u$dimnames$Terms == term, ]))  ))  
  x <- sort(round(x.cor[(x.cor[, term] > corlimit), ], 2), decreasing = TRUE)
  return(x)
}

#Function to drop words and their associations from the datatable
dropfun <- function(dropword, threshold_freq) {
  a <- ctbl[which(dimnames(ctbl)$Var2==dropword), which(dimnames(ctbl)$Var1!=dropword) ] #pull the 'food' row, drop self reference
  dropwords <- names(a[a > threshold_freq]) #drop words with freq less than threshold_freq
  newctbl <- ctbl[which(!dimnames(ctbl)$Var2 %in% dropwords), which(!dimnames(ctbl)$Var1 %in% dropwords)]
  return(newctbl)
}  

#network diagram
#arc diagram