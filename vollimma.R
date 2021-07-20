## vollimma(res,nb=50,fc=0.5,p=0.05,size=2.5,alpha=0.3)


vollimma<-function(res,nb=30,fc=0.5,p=0.05,size=3,alpha=1){
        # require libraries
        if(!require(ggthemes)){install.packages("ggthemes")}
        library(ggthemes)
        if(!require(ggplot2)){install.packages("ggplot2")}
        library(ggplot2)
        if(!require(ggrepel)){install.packages("ggrepel")}
        library(ggrepel) 
        
        # add a column of NO UP DOWN
        res$diffexpressed <- "NO"
        res$diffexpressed[res$logFC> fc & res$P.Value < p] <- "UP"
        res$diffexpressed[res$logFC < -fc & res$P.Value < p] <- "DOWN"
        
        # grep labels on row.names
        res$delabel<-row.names(res)        
        
        # perform the graph
        ggplot(data=res, aes(x=logFC, y=-log10(P.Value), col=diffexpressed, label=delabel)) +
                geom_point(alpha=alpha) + 
                theme_gdocs() +
                geom_text_repel(data=head(res, nb), aes(label=delabel),size=size) +
                scale_color_manual(values=c("#00FA9A", "darkgray", "#FF00FF")) +
                geom_vline(xintercept=c(-fc, fc), col="orange",linetype="dashed") +
                geom_hline(yintercept=-log10(p), col="orange",linetype="dashed") +
                theme(legend.position = "none") 
        
}


vollimma(res,nb=50,fc=0.5,p=0.05,size=2.5,alpha=0.3)
