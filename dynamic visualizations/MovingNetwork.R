
if(FALSE){
library(plyr)
mat = read.csv("SO_R.csv")

nrow(mat)
ncol(mat)
names(mat)

groupColumns = c("WeekId")

result = ddply(mat, groupColumns, function(x) colSums(x[2:316]))
nrow(result)


for(i in 1:nrow(result)) 
{
  for(j in 2:ncol(result))
  {
    if(result[i,j] < 7)  
    {
      result[i,j] = 0
    }
  }
}


write.csv(result, file = "SO_R_Edges.csv")
}

############################################################################################################

# Key packages to install: 
#install.packages("igraph") 
#install.packages("network") 
#install.packages("ndtv")
#install.packages("extrafont")
#install.packages("networkD3")
#install.packages("sna")

#library(igraph) -- NOT REQD.
library(network)
library(ndtv)
library(animation)
library(networkD3)
library(extrafont)
library(tm)
library(networkD3)
library(animation)
library(igraph)
library(ndtv)

# ================ Read the example data ================
#links <- read.csv("SO_R_Edges.csv", header=T, row.names = 1,  as.is=T)
#mat <- as.matrix(links)
#net2 <- graph.incidence(links, weighted  = TRUE)
#g <- cbind(get.edgelist(net2) , round( E(net2)$weight, 3 ))
#write.csv(as.data.frame(g), "FINAL_SO_R_EdgeList.csv")

# ================ Create EdgeList and Nodelist ================
edges <- read.csv("FINAL_SO_R_EdgeList.csv", header=T, as.is=T)
nodes <- read.csv("FINAL_SO_R_NodeList.csv", header=T, as.is=T)




# ================ Create Network ================

l <- layout.fruchterman.reingold(net)
net3 <- network(edges, vertex.attr = nodes, matrix.type="edgelist", 
                loops=F, multiple=F, ignore.eval = F)
net3%v% "label"

timings <- read.csv("SO_R_Timings.csv", header = T)
  vs <- data.frame(onset=timings[,2], terminus=timings[,3], vertex.id=edges[,2])
es <- data.frame(onset=timings[,2], terminus=timings[,4], 
                 head=as.matrix(net3, matrix.type="edgelist")[,1],
                 tail=as.matrix(net3, matrix.type="edgelist")[,2])



net3.dyn <- networkDynamic(base.net=net3, edge.spells=es)

summary(net3.dyn)
#net3.dyn <- network.extract(networkDynamic(base.net=net3, edge.spells=es, vertex.spells = vs), onset = timings[,2], terminus = timings[,3], length = NULL, at = timings[,2],
#                            rule = c("any", "all"), active.default = TRUE, retain.all.vertices = FALSE,
#                            trim.spells=FALSE)

# We can pre-compute the animation coordinates (otherwise they get calculated when 
# you generate the animation). Here animation.mode is the layout algorithm - 
# one of "kamadakawai", "MDSJ", "Graphviz"and "useAttribute" (user-generated).

detach(package:network)
detach(package:igraph)

# ================ Create Animation ================


compute.animation(net3.dyn, animation.mode = "kamadakawai",
                  slice.par=list(start=0, end=64,interval=1, 
                                 aggregate.dur=1, rule='any'))



render.d3movie(net3.dyn, usearrows = F, 
               displaylabels = T, label=net3 %v% "label",
               bg="#ffffff", vertex.border="#111111",
               vertex.cex = 2,  
               vertex.col = nodes[,4],
               edge.length = 100,
               label.cex = 1,
               edge.lwd = (net3.dyn %e% "weight")/3, 
               edge.col = '#55555599',
               vertex.tooltip = paste("<b>Name:</b>", (net3.dyn %v% "label") , "<br>",
                                      "<b>Type:</b>", (net3.dyn %v% "label")),
               edge.tooltip = paste("<b>Edge weight:</b>", (net3.dyn %e% "weight" ) ),
               launchBrowser=T, filename="Test.html",
               render.par=list(tween.frames = 30, show.time = F),
               plot.par=list(mar=c(0,0,0,0)))
