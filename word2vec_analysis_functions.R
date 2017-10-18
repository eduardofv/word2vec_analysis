##
# Algunas funciones para trabajar con word embeddings generados por word2vec
##
library(RColorBrewer)
library(wordcloud)

#Carga Embeddings desde el directorio de checkpoints generado por Tensorflow
#  regresa una lista con un data frame de vectores (emb) y un data frame 
#  con el vocabulario incluyendo la frecuencia de cada termino, ordenados por frecuencia
#  dir: directorio. Se concatenan los nombres de los archivos 
#       este directorio puede ser donde se salvan los datos procesados por w2v
#  normalize_vectors: Si los embeddings se regresan normalizados (magnitud=1).
cargar.embeddings <- function(dir, normalize_vectors=T){
  emb <- read.csv(paste0(dir,"/embeddings.csv"),header = F)
  vocab <- read.csv(paste0(dir,"/vocab.txt"),header=F)
  
  #adaptacion de datos
  if(normalize_vectors){
    emb <- normalize(emb) #Para usar por default siempre los vectores normalizados
  }
  vocab$word<-sub("^b'([^']+)'.*$","\\1",vocab$V1)
  vocab$freq<-as.integer(sub("^.* ([0-9]+)$","\\1",vocab$V1))
  return(list(emb=emb,vocab=vocab))
}

#Carga los embeddings originales del Corpus en Espanol
cargar.embeddings.SBW <- function(fn){
  df <- read.table(fn,header=F,sep=" ",encoding="UTF-8",skip=1)
  vocab <- data.frame(word=df[,1:1])
  vocab$freq <- 1 #Simulamos la frecuencia de las palabras ya que no hay info
  return(list(emb=df[,2:ncol(df)],vocab=vocab))
}

#Cargar de RDS, es mas rapido si ya lo tienes creado
cargar.embeddings.rds <- function(dir){
  emb <- readRDS(paste0(dir,"embeddings.rds"))
  vocab <- readRDS(paste0(dir,"vocab.rds"))
  return(list(emb=emb,vocab=vocab))
}

#Usar esto si es la primera vez que se cargan los embeddings de CSV para generar el RDS
guardar.rds <- function(dir,emb,vocab){
  saveRDS(emb,paste0(dir,"embeddings.rds"))
  saveRDS(vocab,paste0(dir,"vocab.rds"))
}

#Cosine Similarity, basica y lenta
#  Construyendo las similtudes,lento y mucha memoria
cos.sim <- function(A,B){
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   

#Normalizar un data.frame de vectores (emb)
normalize <- function(dat){
  return(dat/sqrt(rowSums(dat^2)))  
}

#Obtener el vector para una palabra
#    x: df de vectores (default al llamado emb)
#    v: df de vocabulario, lista de palabras (default al llamado vocab)
wordemb <- function(word,x=emb,v=vocab){
  return(x[v$word==word,][1,])
}

#Obtener las palabras mas similares a una dada
#    word: palabra o vector para buscar
#    n: numero de resultados a regresar
#    x: df de vectores
#    v: df de vocabulario, lista de palabras (default al llamado vocab)
#    vectors: regresar vectores en vez de palabras
similares <- function(word,n=10,x=emb,v=vocab,vectors=F){
  if(class(word)=="character")
    A<-wordemb(word,x = x,v = v)
  else  #Si se le paso un DF, es decir, un vector
    A<-word[1,] 
  #cosine similarity, vectorized as best as I could
  d<-sqrt(rowSums(x^2)*rowSums(A^2))
  c<-as.matrix(x) %*% as.vector(A,"numeric") 
  Asims<-c/d
  Asims.index<-order(Asims,decreasing = T) 
  #Exclude first result which is the same word, sim==1
  Asims.index<-Asims.index[2:length(Asims.index)]
  if(vectors){
    df<-x[Asims.index,] 
  } else {
    df<-data.frame(v,cos.sim=Asims)
    df<-df[Asims.index,] #ordernar por los mas similares
  }
  if(n==0) n<-nrow(df)
  return(df[1:n+1,])
}

#Regresa resultados tipo "w1 es a w2 como w3 es a ..."
#  w1,w2,w3: palabras para la analogia
#    x: df de vectores
#    v: df de vocabulario, lista de palabras (default al llamado vocab)
#  ej:
#analogia("paris","france","greece",x=embn)
#vocab   cos.sim
#3069        athens 0.6869609
#66405    hecataeus 0.6593994 ...
analogia <- function(w1,w2,w3,x=emb,v=vocab){
  v1 <- wordemb(w1,x=x,v=v)
  v2 <- wordemb(w2,x=x,v=v)
  v3 <- wordemb(w3,x=x,v=v)
  
  #No se por que pero esto conserva el "a es a b como c es a x", en vez de v1-v2+v3
  sim <- similares(v2-v1+v3,x=x,v=v)
  sim <- sim[!(sim$word %in% c(w1,w2,w3)),]
  return(sim)
}


#Experimental: Comparar dos vectores en  un barplot
vector.compare.plot <- function(v1,v2,beside=F){
  tmp <- as.matrix(v1)
  tmp <- rbind(tmp,as.matrix(v2))
  barplot(tmp,col=c("red","blue"),beside=beside)
}

#Wordplot: Genera una grafica de palabras en dos dimensiones de sus vectores
#    dims: Dimensiones en las que se grafican las palabras. Por default son las dos primeras.
#          Pueden especificarse por nombre o indice
#    v: df de vocabulario, lista de palabras (default al llamado vocab)
#    max.plot: Numero de palabras a mostrar. 0 == todas, default 100
#    cex: Tamano del font a enviar a text. Default 0.5
#    col: Color del texto a enviar a text. Default, negro con alpha #66
word.plot <- function(d,dims=NULL,v=vocab,max.plot=100,cex=0.5,col="#FF000066"){
  if(is.null(dims)){
    dims = colnames(d)[1:2]
  }
  if(class(dims[1]) == "numeric"){
    dd <- dims
    dims[1] = colnames(d)[dd[1]]
    dims[2] = colnames(d)[dd[2]]
  }
  if(max.plot==0){
    max.plot = length(d[,dims[1]])
  }
  plot(x=d[1:max.plot,dims[1]],y=d[1:max.plot,dims[2]],pch="+",col=col,xlab=dims[1], ylab=dims[2])
  text(x=d[1:max.plot,dims[1]],y=d[1:max.plot,dims[2]],labels=v[rownames(d[1:max.plot,]),"word"],cex=cex,pos=4,col="#00000080")
}

#Word Plot Dimension: Genera un word plot con los valores extremos 
#  en una dimension sobre una segunda dimension. Esto es util para saber
#  que tanto son separables algunos valores sobre una dimension
#  d: df de embeddings 
#  target.dim: indice o nombre de la dimension principal, sobre la que 
#              se seleccionan los valores extremos 
#  sec.dim: dim secundaria para hacer la grafica
#  v: df de vocabulario, lista de palabras (default al llamado vocab)
#  max.plot: Numero maximo de palbras a graficar en cada extremo
word.plot.dimension <- function(d,target.dim=1,sec.dim=2,v=vocab,max.plot=50){
  if(max.plot==0){
    max.plot = length(d[,dims[1]])/2
  }
  o <- order(d[,target.dim])
  oi <- order(d[,target.dim],decreasing = T)
  minx <- min(d[o[1:max.plot],target.dim],x=d[oi[1:max.plot],target.dim])
  maxx <- max(d[o[1:max.plot],target.dim],x=d[oi[1:max.plot],target.dim])
  miny <- min(d[o[1:max.plot],sec.dim],d[oi[1:max.plot],sec.dim])
  maxy <- max(d[o[1:max.plot],sec.dim],d[oi[1:max.plot],sec.dim])
  plot(x=d[o[1:max.plot],target.dim],
       y=d[o[1:max.plot],sec.dim],
       xlim = c(minx,maxx),
       ylim = c(miny,maxy),
       pch="+",col="#0000FF66",xlab=target.dim, ylab=sec.dim)
  points(x=d[oi[1:max.plot],target.dim],
       y=d[oi[1:max.plot],sec.dim],
       type="p",pch="+",col="#FF000066")
  text(x=d[o[1:max.plot],target.dim],
       y=d[o[1:max.plot],sec.dim],
       col="#000000A0",
       labels=v[rownames(d[o[1:max.plot],]),"word"],cex=0.5,pos=4)
  text(x=d[oi[1:max.plot],target.dim],
       y=d[oi[1:max.plot],sec.dim],
       col="#000000A0",
       labels=v[rownames(d[oi[1:max.plot],]),"word"],cex=0.5,pos=2)
}

#Kmeans de palabras similares a traves de un PCA sobre ese subconjunto:
#  obtiene similares a la palabra objetivo
#  ejecuta un PCA sobre ellas
#    - Opcionalmente grafica un analisis de centros de cluster para k-means
#       ref: https://www.r-bloggers.com/exploring-assumptions-of-k-means-clustering-using-r/
#  ejecuta k-means clustering
#  realiza wordplot usando los dos primeros componentes del k-means
#  regresa un df con palabras, similitues, cluster, frecuecias, frecuencias relativas y score
#  (score por refinar)
#    word: palabra objetivo sobre la cual buscar similares y clusterizar
#    x: df de vectores
#    v: df de vocabulario, lista de palabras (default al llamado vocab)
#    n: numero de palabras similares a usar
#    centers: numero de centros para el k-means
#    show.center.analysis: en vez de wordplot crea grafica de analisis de centros
#    pal: paleta de colores para usar.
pca.cluster.similar <- function(word,x=emb,v=vocab,n=100,centers=5,show.center.analysis=F,max.center.analysis=15,pal=NULL){
  d <- similares(word,x=x,v=v,vectors=T,n=n)
  #Prueba a ver que cambios hay
  #d <- prcomp(d)$x
  if( show.center.analysis ){
    sse=vector('numeric')
    for(i in 2:max.center.analysis){
      sse[i-1]=sum(kmeans(d,centers=i)$withinss)/n
    }
    sse=as.data.frame(sse)
    sse$k=seq.int(2,15)
    plot(sse$k,sse$sse,type="b")
    return()
  }
  else {
    dv <- similares(word,x=x,v=v,n=n)
    k <- kmeans(d,centers = centers)
    #plot colors
    if( is.null(pal) ) {
      col = rainbow(centers,v = 0.9)
      center.col = rownames(k$centers)
    } else {
      col = pal[k$cluster]
      center.col = pal[rownames(k$centers)]
    }
    #do plot
    word.plot(d,v=dv,col=col,max.plot = n)
    points(k$centers[,1],k$centers[,2],pch=10,col=center.col)
    #return words clustered and ordered
    dclus<-data.frame(dv,k$cluster)
    dclus$rel.freq<-dclus$freq / max(dclus$freq)
    dclus$score <- dclus$rel.freq * dclus$cos.sim
    dcluso<-dclus[order(dclus$k.cluster,-dclus$freq),]
    return(dcluso)
  }
}

#Experimental. como determinamos la importancia de cada cluster
cluster.importance <- function(dclust){
  ag<-aggregate(score~k.cluster,dclust,sum)
  return(ag[order(-ag$score),])
}

#cluster wordcloud: genera wordclouds por cada cluster.
#  si es en PDF genera un wc por pagina, 
#  si es plot ajusta N wcs por renglon
#    dclust: objeto de clustering
#    pal: paleta de colores 
#    pdf: salida en PDF o plot
#    graphs.per.row: si es plot, como organizar
#    scale: tamano de los fonts, se pasa a wordcloud
#    by.sim: En vez de usar la frecuencia usa la similitud para el tamano del font
cluster.wordcloud <- function(dclust,pal=NULL,pdf=F,graphs.per.row=2,scale=c(4,.5),by.sim=F){
  g <- length(unique(dclust$k.cluster))
  if(is.null(pal)){
    pal = rainbow(g,v=0.7)
  }
  if(pdf){
    pdf()
  } else {
    if(graphs.per.row==0){
      par(mfrow=c(ceiling(g/graphs.per.row),graphs.per.row))
    }
  }
  for(i in 1:g){
    dclust.sub <- dclust[dclust$k.cluster==i,]
    colors=pal[i]
    if(by.sim){
      f <- 2**ceiling(10*dclust$cos.sim)
    } else {
      f <- dclust.sub$freq
    }
    wordcloud(words = dclust.sub$word, freq=f, colors = colors, scale = scale)
    #wordcloud(words = dclust.sub$word, freq = dclust.sub$freq, colors = colors, scale = scale)
  }
  if(pdf){
    dev.off()
  } else {
    if(graphs.per.row==0){
      par(mfrow=c(1,1))
    }
  }
}


