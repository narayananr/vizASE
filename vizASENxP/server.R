library(shiny)
require(ggplot2)
require(reshape2)
########################
###### CHANGE ME
###### Point to right directory where the data is located 
########################
setwd("~/Work/Projects/NodPwkF1/data")
nod.exp=read.table("NOD-allele-exp-for-Shiny.txt", header=T)
pwk.exp=read.table("PWK-allele-exp-for-Shiny.txt", header=T)
gene.info=read.table("gene-info-Shiny.txt", header=T)
vmeta = read.table("NODxPWK-vitaD-annotation.txt", header=T)
vita.dsgn = vmeta[,1:4]
vita.cross = vita.dsgn[match(gsub("X","",colnames(nod.exp)),vita.dsgn[,1]),]
vita.cross$Age[which(vmeta$Age==25)]=24
head(vita.cross)
vita.cross$Diet= gsub("normal","CH",vita.cross$Diet)
vita.cross$Diet= gsub("high","EN",vita.cross$Diet)
vita.cross$Cross= gsub("NOD-PWK","NxP",vita.cross$Cross)
vita.cross$Cross= gsub("PWK-NOD","PxN",vita.cross$Cross)
Group= apply(vita.cross,1,function(x){paste(c(x[4],"-",x[3],"-",sub(" ","",x[2])),collapse="")})

################
###############
name="Igf2r"
get.exp <- function(name){
    g.ind = which(gene.info$geneName==name)
    g.ind
    gName= paste(c(as.vector(gene.info$geneName)[g.ind],":",as.vector(gene.info$geneID)[g.ind]), 
                    collapse="")
    head(nod.exp)
    head(pwk.exp)
    
    nodPwk.gene= data.frame(nod=as.numeric(nod.exp[g.ind,]), 
                            pwk=as.numeric(pwk.exp[g.ind,]), 
                            exp=as.numeric(nod.exp[g.ind,])+as.numeric(pwk.exp[g.ind,]),
                            #grp=gsub("-MOE","",Group)
                            grp=Group)
    nodPwk.gene[1:5,]
    colnames(nodPwk.gene)=c("nod","pwk",gName,"group" )
    head(nodPwk.gene)
    return(nodPwk.gene)
}

plot.it <-function(input){
  allele <- input$allele=="Allele"
  log2scale <- input$log=="log2(Cnt+1)"
  exp.data <- get.exp(input$name)
  #exp.data= nodPwk.gene
  nod.data = data.frame(Strain=Group,exp=as.numeric(exp.data$nod), Allele=rep("NOD",nrow(exp.data)) )
  pwk.data = data.frame(Strain=Group,exp=as.numeric(exp.data$pwk), Allele=rep("PWK",nrow(exp.data)) )
  
  age.collapse <- gsub("-8","",nod.data$Strain)
  age.collapse <- gsub("-24","",age.collapse)
  nod.data$age.collapse <- age.collapse
  pwk.data$age.collapse <- age.collapse
  diet.collapse <- gsub("-CH","",nod.data$Strain)
  diet.collapse <- gsub("-EN","",diet.collapse)
  nod.data$diet.collapse <- diet.collapse
  pwk.data$diet.collapse <- diet.collapse
  cross.collapse <- gsub("NxP-","",nod.data$Strain)
  cross.collapse <- gsub("PxN-","",cross.collapse)
  nod.data$cross.collapse <- cross.collapse
  pwk.data$cross.collapse <- cross.collapse
  head(pwk.data)
  head(exp.data)
  np.data= rbind(nod.data,pwk.data)
  np.data$exp = as.numeric(np.data$exp)
  head(np.data)
  if (allele){
    gname= colnames(exp.data)[3]
    head(np.data)
    #scale_y_continuous(trans=log2_trans())
    #df.g <- summarySE(bc.data, measurevar=exp, groupvars=c("Strain","Allele"))
    if (log2scale){
      gg <- ggplot(np.data, aes(x=Allele, y=exp, fill=Allele)) +
        ggtitle(gname) + guides(fill=FALSE) +
        geom_boxplot()+
        geom_jitter(position = position_jitter(w = 0.1, h = 0.2), size=3) +theme_bw()+
        facet_grid(. ~ Strain, scales = "free", space = "free")+
        theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
              axis.title.y = element_text(face="bold", colour="#990000", size=20),
              axis.text.x  = element_text(angle=0, vjust=0.5, size=12),
              axis.text.y  = element_text(angle=0, vjust=0.5, size=12),
              plot.title = element_text(size=22, face="bold"),
              strip.text.x = element_text(size=10, face="bold",angle=0)) 
      gg <-gg + scale_y_continuous(trans="log2")    
      print(gg)
    }else{
      gg <- ggplot(np.data, aes(x=Allele, y=exp, fill=Allele)) +
        ggtitle(gname) +guides(fill=FALSE) +
        geom_boxplot()+
        geom_jitter(position = position_jitter(w = 0.1, h = 0.2), size=3) +theme_bw()+
        facet_grid(. ~ Strain, scales = "free", space = "free")+
        theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
              axis.title.y = element_text(face="bold", colour="#990000", size=20),
              axis.text.x  = element_text(angle=0, vjust=0.5, size=12),
              axis.text.y  = element_text(angle=0, vjust=0.5, size=12),
              plot.title = element_text(size=22, face="bold"),
              strip.text.x = element_text(size=10, face="bold",angle=0)) 
      print(gg)   
    }
  }else{
    gname = colnames(exp.data)[3]
    colnames(exp.data)=c("nod","pwk","exp","group") 
    head(exp.data)
    Strain= gsub("-MOE","",exp.data$group)
    Strain=gsub("-BULB","",Strain)
    exp.data$Strain<-Strain
    if (log2scale){
      #df.g <- summarySE(exp.data, measurevar=exp, groupvars=c("group"))
      gg <- ggplot(exp.data, aes(x=group, y=exp, fill=Strain)) +
        ggtitle(gname) +guides(fill=FALSE) +
        geom_boxplot(size=0.2)+
        facet_grid(. ~ Strain, scales = "free", space = "free")+
        geom_jitter(position = position_jitter(w = 0.1, h = 0.2),size=3) +theme_bw()+
        theme(axis.title.x = element_text(face="bold", colour="#990000", size=25),
              axis.title.y = element_text(face="bold", colour="#990000", size=25),
              axis.text.x  = element_text(angle=0, vjust=0.5, size=12),
              axis.text.y  = element_text(angle=0, vjust=0.5, size=12),
              plot.title = element_text(size=22, face="bold"),
              strip.text.x = element_text(size=10, face="bold",angle=0)) 
      gg <-gg + scale_y_continuous(trans="log2")  
      print(gg)      
    }else{
      #df.g <- summarySE(exp.data, measurevar=exp, groupvars=c("group"))
      gg <- ggplot(exp.data, aes(x=group, y=exp, fill=Strain)) +
        ggtitle(gname) +guides(fill=FALSE) +
        geom_boxplot(size=0.2)+
        facet_grid(. ~ Strain, scales = "free", space = "free")+
        geom_jitter(position = position_jitter(w = 0.1, h = 0.2),size=3) +theme_bw()+
        theme(axis.title.x = element_text(face="bold", colour="#990000", size=25),
              axis.title.y = element_text(face="bold", colour="#990000", size=25),
              axis.text.x  = element_text(angle=0, vjust=0.5, size=12),
              axis.text.y  = element_text(angle=0, vjust=0.5, size=12),
              plot.title = element_text(size=22, face="bold"),
              strip.text.x = element_text(size=10, face="bold",angle=0)) 
      print(gg)      
    }
  }
  
}

shinyServer(function(input, output){
  #### get output plot
  output$result <- renderPlot({plot.it(input)})
  })
  
