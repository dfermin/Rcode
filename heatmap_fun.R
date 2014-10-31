library(ggplot2);
library(reshape);
library(lattice);
library(grid);

if(!exists("d"))
  d <- read.delim("~/tmp/year2_d0_pseudoDiffExpr_top50.xls", as.is=T);

m <- as.matrix(d[,-c(1,2)]); ## get matrix
rownames(m) <- d[,1];

## get average expression of gene (this will be the 2nd plot)
mu = data.frame(
    gene=rownames(m),
    avgExpr=apply(m, 1, mean),
    stringsAsFactors=FALSE
);

rs <- apply(m, 1, sum);
m <- m/rs;

m2 <- melt(m);
colnames(m2) <- c("gene", "subject", "intensity");

## this is the basic heatmap using ggplot. No clustering has been done
htm <- ggplot(m2, aes(x=subject, y=gene)) +
       geom_tile(aes(fill=intensity), color="white") +
       scale_fill_gradient(low="white", high="blue") +
       theme(legend.position="left");
  

## this is the second plot we will make
bp <- ggplot(mu, aes(x=gene, y=avgExpr)) +
      geom_bar(stat="identity") + 
      theme(axis.ticks=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank()) +
      coord_flip();

#pdf(file="~/Desktop/bebeCakes.pdf", width=10, height=8);

grid.newpage(); ## new blank grid
# Define grid layout to locate plots and print each graph
## the plot area is broken oup into 3 columns with this command
pushViewport(viewport(layout=grid.layout(1,10)));

print(htm, vp=viewport(layout.pos.row=1, layout.pos.col=c(1,8.5)));
print(bp, vp=viewport(layout.pos.row=1, layout.pos.col=c(9,10)));




## Working code
## Define grid layout to locate plots and print each graph
## the plot area is broken oup into 3 columns with this command
# pushViewport(viewport(layout=grid.layout(1,3)));
# print(htm, vp=viewport(layout.pos.row=1, layout.pos.col=c(1,2)));
# print(bp, vp=viewport(layout.pos.row=1, layout.pos.col=3));


#dev.off();










