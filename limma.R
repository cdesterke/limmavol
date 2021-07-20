## usage res<-deg(data,group,control="Liver") 
## data a matrix of data
## group a vector of group samples


deg<-function(data,group,control="HD")
{
  if(!require(limma)){
    
    if (!requireNamespace("BiocManager", quietly = TRUE))
      install.packages("BiocManager")
    BiocManager::install("limma")}
  
  library(limma)	
  
  # prepare design matrix
  levels <- relevel(as.factor(group),ref=control)
  design <- model.matrix(~levels)
  rownames(design) = colnames(data)
  
  # perform limma analysis
  tmp <- lmFit(data,design=design)
  fit <- eBayes(tmp)
  res = topTable(fit,number = nrow(data),coef=2)
  res
}




res<-deg(data,group,control="Liver")
