#install.packages(PSSMCOOL)
AATP_TPC <- function(pssm_name){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  p<-x
  #p<-1/(1+exp(-p))
  L<-dim(p)[1]
  X<-apply(p,2,mean)
  names(X)<-NULL
  y<-matrix(0,20,20)
  TPC<-c()
  for(i in 1:20){
    kjey<-rep(0,20)
    for(j in 1:20){
      for(k in 1:(L-1)){
        kjey[j]<-kjey[j]+p[k,i]*p[k+1,j]
      }
    }
    for(j in 1:20){
      y[i,j]<-kjey[j]/sum(kjey)
      TPC<-c(TPC,y[i,j])
    }
  }
  AATP<-c(X,TPC)
  TPC <- round(TPC, digits = 4)
  AATP<-round(AATP,digits = 4)
  return(list(TPC, AATP))
}
AB_PSSM <- function(pssm_name){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  m2<-x
  #m2<-1/(1+exp(-m2))
  L<-dim(m2)[1]
  s<-floor(L/20)
  sm<-rep(0,20)
  p<-0;n<-0
  f<-matrix(0,20,20)
  for(j in 1:20){
    for(t in 1:19){
      r1<-(1+(t-1)*s)
      r2<-(s+(t-1)*s)
      for(i in r1:r2){
        sm<-sm+m2[i,]
        if(m2[i,j]>0){
          p<-p+m2[i,j]
          n<-n+1
        }
      }
      p<-0;n<-0
      f[,t]<-sm/s
      sm<-rep(0,20)
      
    }
  }
  e<-(19*s+1)
  for(i in e:L){
    sm<-sm+m2[i,]
  }
  f[,20]<-sm/(L-19*s)
  
  v<-c()
  for(i in 1:20){
    v<-c(v,f[,i])
  }
  v<-round(v,digits = 4)
  return(v)
}
consunsus_sequence<-function(pssm_name){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  m<-x
  L<-dim(m)[1]
  aaVect<-c("A","R","N","D","C","Q","E","G","H","I","L","K","M","F","P","S","T","W","Y","V")
  d<-c()
  for(i in 1:L){
    d[i]<-which.max(m[i,])
  }
  consunsus<-aaVect[d]
  consunsus<-paste(consunsus,collapse = "")
  return(consunsus)
}
DP_PSSM <- function(pssm_name,a=5){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  m2<-x
  #m2<-1/(1+exp(-m2))
  L<-dim(m2)[1]
  mt<-matrix(0,L,20)
  for(i in 1:L){
    if(sd(m2[i,])==0){
      mt[i,]<-m2[i,]
    }else{
      mt[i,]<-(m2[i,]-mean(m2[i,]))/sd(m2[i,])
    }
  }
  t1<-1:40
  for(j in 1:20){
    p<-mt[,j][mt[,j]>0]
    n<-mt[,j][mt[,j]<0]
    t1[2*j-1]<-mean(p)
    t1[2*j]<-mean(n)
    
  }
  pp<-matrix(0,L-a,20)
  np<-c() #number of positive differences in column j
  nn<-c() #number of negative differences in column j
  
  for(j in 1:20){
    for(i in 1:(L-a)){
      pp[i,j]<-mt[i,j]-mt[i+a,j]
    }
    ss<-pp[,j][pp[,j]>0]
    ff<-pp[,j][pp[,j]<0]
    np[j]<-length(ss)
    nn[j]<-length(ff)
  }
  x<-c()
  smp<-0
  smn<-0
  deljp<-rep(0,a)
  deljn<-rep(0,a)
  G<-c()
  for(j in 1:20){
    for(k in 1:a){
      for(i in 1:(L-k)){
        if((mt[i,j]-mt[i+k,j])>0){
          smp<-smp+(mt[i,j]-mt[i+k,j])^2
        }
        if((mt[i,j]-mt[i+k,j])<0){
          smn<-smn+(mt[i,j]-mt[i+k,j])^2
        }
      }
      deljp[k]<-smp/np[j]
      x<-c(x,deljp[k])
      deljn[k]<-(-smn/nn[j])
      x<-c(x,deljn[k])
      smp<-0
      smn<-0
    }
    G<-c(x,G)
    x<-c()
  }
  H<-c(t1,G)
  H<-round(H,digits = 4)
  return(H)
}

CS_PSe_PSSM <- function(pssm_name,vec_name){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  E<-x
  E<-1/(1+exp(-E))
  L<-dim(E)[1]
  names(L)<-NULL
  v<-c("A","R","N","D","C","Q","E","G","H","I","L","K","M","F","P","S","T","W","Y","V")
  a<-rep(0,L)
  consensus<-c()
  for(i in 1:L){
    a[i]<-which.max(E[i,])
    consensus<-c(consensus,v[a[i]])
  }
  CSAAC<-c()
  CSCM<-c()
  i<-1
  while (i<=20) {
    t<-which(consensus==v[i])
    CSAAC[i]<-length(t)/L
    CSCM[i]<-sum(t)/(L*(L-1))
    i<-i+1
  }
  cspssm<-c(CSAAC,CSCM)
  n<-2
  L1<-round(L/n)
  L2<-L-L1
  a<-b<-matrix(0,5,20)
  o<-u<-v<-matrix(0,3,20)
  AC1<-AC2<-matrix(0,4,20)
  s<-ss<-0
  for(j in 1:20){
    a[1,j]<-sum(E[,j][1:L1])
    a[1,j]<-a[1,j]/L1
    b[1,j]<-sum(E[,j][(L1+1):L])
    b[1,j]<-b[1,j]/(L-L1)
    for(y in 1:4){
      for(i in 1:(L1-y)){
        s<-s+(E[i,j]-E[i+y,j])^2
      }
      a[y+1,j]<-s/(L1-y)
      s<-0
      for(i in (L1+1):(L-y)){
        s<-s+(E[i,j]-E[i+y,j])^2
      }
      b[y+1,j]<-s/(L-L1-y)
      s<-0
      for(i in 1:(L1-y)){
        ss<-ss+(E[i,j]-a[1,j])*(E[i+y,j]-a[1,j])
      }
      AC1[y,j]<-ss/(L1-y)
      ss<-0
      for(i in (L1+1):(L-y)){
        ss<-ss+(E[i,j]-b[1,j])*(E[i+y,j]-b[1,j])
      }
      AC2[y,j]<-ss/(L-L1-y)
    }
  }
  q1<-q2<-c()
  for(i in 1:4){
    q1<-c(q1,AC1[i,])
    q2<-c(q2,AC2[i,])
  }
  q3<-c(q1,q2)
  w1<-w2<-c()
  for(i in 1:5){
    w1<-c(w1,a[i,])
    w2<-c(w2,b[i,])
  }
  w3<-c(w1,w2)
  v1<-v2<-v3<-c()
  n<-3
  s<-ss<-0
  AC3<-AC4<-AC5<-matrix(0,2,20)
  L1<-round(L/3)
  L2<-2*L1
  L3<-(L-2*L1)
  for(j in 1:20){
    o[1,j]<-sum(E[,j][1:L1])
    o[1,j]<-o[1,j]/L1
    u[1,j]<-sum(E[,j][(L1+1):(2*L1)])
    u[1,j]<-u[1,j]/L1
    v[1,j]<-sum(E[,j][(2*L1 +1):L])
    v[1,j]<-v[1,j]/(L-2*L1)
    for(y in 1:2){
      for(i in 1:(L1-y)){
        s<-s+(E[i,j]-E[i+y,j])^2
      }
      o[y+1,j]<-s/(L1-y)
      s<-0
      for(i in (L1+1):(2*L1-y)){
        s<-s+(E[i,j]-E[i+y,j])^2
      }
      u[y+1,j]<-s/(L1-y)
      s<-0
      for(i in (2*L1 +1):(L-y)){
        s<-s+(E[i,j]-E[i+y,j])^2
      }
      v[y+1,j]<-s/(L-2*L1-y)
      for(i in 1:(L1-y)){
        ss<-ss+(E[i,j]-o[1,j])*(E[i+y,j]-o[1,j])
      }
      AC3[y,j]<-ss/(L1-y)
      ss<-0
      for(i in (L1+1):(2*L1-y)){
        ss<-ss+(E[i,j]-u[1,j])*(E[i+y,j]-u[1,j])
      }
      AC4[y,j]<-ss/(L1-y)
      ss<-0
      for(i in (2*L1+1):(L-y)){
        ss<-ss+(E[i,j]-v[1,j])*(E[i+y,j]-v[1,j])
      }
      AC5[y,j]<-ss/(L-2*L1-y)
      ss<-0
    }
  }
  q4<-q5<-q6<-c()
  for(i in 1:2){
    q4<-c(q4,AC3[i,])
    q5<-c(q5,AC4[i,])
    q6<-c(q6,AC5[i,])
  }
  q7<-c(q4,q5,q6)
  for(i in 1:3){
    v1<-c(v1,o[i,])
    v2<-c(v2,u[i,])
    v3<-c(v3,v[i,])
  }
  v4<-c(v1,v2,v3)
  segmented_psepssm<-c(w3,v4)
  segmented_acpssm<-c(q3,q7)
  total<-c(cspssm,segmented_psepssm,segmented_acpssm)
  vector_namse<-c("cspssm","segmented_psepssm","segmented_acpssm","total")
  switch(vec_name,vector_namse)
  dd<-eval(as.name(vec_name))
  dd<-round(dd,digits = 4)
  return(dd)
}
Discrete_Cosine_Transform<- function(pssm_name){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  p<-x
  w<-c()
  colnames(p)<-NULL
  p<-1/(1+exp(-p))
  requireNamespace("dtt",quietly = TRUE)
  p<-dtt::dct(p)
  p<-round(p,digits = 4)
  for(i in 1:20){
    w<-c(w,p[i,])
  }
  return(w)
}
disulfid<-function(pssm_name){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  k<-x[,1]
  k<-as.character(k)
  p<-x[,-1]
  mode(p)<-"integer"
  p<-1/(1+exp(-p))
  L<-dim(p)[1]
  v<-which(k=="C")
  if(length(v)==0|length(v)==1){
    stop("there is no disulfid bond")
  } else {
    if(v[1]<7){
      h<-matrix(0,(7-v[1]),20)
      p<-rbind(h,p)
      v<-v+(7-v[1])
    }
    if(v[length(v)]>(L-6)){
      tt<-matrix(0,(v[length(v)]-(L-6)),20)
      p<-rbind(p,tt)
    }
    w<-matrix(0,length(v),260)
    for(i in v){
      x<-p[(i-6):(i+6),]
      a<-which(v==i)
      ss<-c()
      for(j in 1:dim(x)[1]){
        ss<-c(ss,x[j,])
      }
      w[a,]<-ss
    }
    colnames(w)<-NULL
    az<-c()
    aw<-c()
    for(i in 1:(dim(w)[1]-1)){
      for(j in (i+1):dim(w)[1]){
        az<-c(az,paste0("c",i,"c",j))
        aw<-rbind(aw,c(w[i,],w[j,]))
      }
    }
  }
  aw<-round(aw,digits = 4)
  aw<-as.data.frame(aw)
  aw<-cbind(az,aw)
  colnames(aw)<-1:521
  return(aw)
}

EDP_EEDP_MEDP <- function(pssm_name){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  p<-x
  #p<-1/(1+exp(-p))
  L<-dim(p)[1]
  s<-0
  e<-matrix(0,20,20)
  for(k in 1:20){
    for(t in 1:20){
      for(i in 2:(L-1)){
        edf<-(p[i-1,k]-p[i+1,t])/2
        edf<-edf^2
        s<-s+edf
        
      }
      e[k,t]<-s/(L-2)
      s<-0
    }
  }
  EDP<-apply(e,2,mean)
  names(EDP)<-NULL
  EDP<-round(EDP,digits = 4)
  v<-c()
  for(i in 1:20){
    v<-c(v,e[i,])
  }
  EEDP<-round(v,digits = 4)
  MEDP<-c(EDP,EEDP)
  return(list(EDP,EEDP, MEDP))
}

grey_pssm_pseAAC<-function(pssm_name){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  m<-x
  protein_seq<-m[,1]
  protein_seq<-as.character(protein_seq)
  m2<-m[,-1]
  m2<-as.matrix(m2)
  mode(m2)<-"integer"
  m2<-1/(1+exp(-m2))
  L<-dim(m2)[1]
  feature_vect<-vector(mode = "numeric",length = 100)
  v<-c("A","R","N","D","C","Q","E","G","H","I","L","K","M","F","P","S","T","W","Y","V")
  for(i in 1:20){
    a<-which(protein_seq==v[i])
    feature_vect[i]<-length(a)/length(protein_seq)
    feature_vect[i]<-round(feature_vect[i],digits = 4)
  }
  ss<-apply(m2,2,mean)
  names(ss)<-NULL
  ss<-round(ss,digits = 4)
  feature_vect[21:40]<-ss
  B<-list()
  w<-list()
  s<-rep(0,60)
  for(j in 1:20){
    column3<-rep(1,L-1)
    column2<-rep(0,L-1)
    U<-rep(0,L-1)
    for(i in 1:(L-1)){
      column2[i]<- -(sum(m2[,j][1:i])+0.5*m2[i+1,j])
      U[i]<-m2[i+1,j]-m2[i,j]
    }
    U<-as.matrix(U)
    column1<- -m2[2:L,j]
    B[[j]]<-data.frame(column1,column2,column3)
    colnames(B[[j]])<-NULL
    rownames(B[[j]])<-NULL
    B[[j]]<-as.matrix(B[[j]])
    x<-t(B[[j]])%*%B[[j]]
    while(det(x)==0){
      diag(x)<-diag(x)+1
    }
    y<-t(B[[j]])%*%U
    x2<-solve(x)
    x2<-round(x2,digits = 4)
    w[[j]]<-x2%*%y
    s[3*j-2]<-feature_vect[j]*w[[j]][1,1]
    s[3*j-1]<-feature_vect[j]*w[[j]][2,1]
    s[3*j]<-feature_vect[j]*w[[j]][3,1]
    
  }
  feature_vect[41:100]<-s
  feature_vect<-round(feature_vect,digits = 4)
  return(feature_vect)
}
DFMCA_PSSM<-function(pssm_name,n=7){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  E<-x
  E<-1/(1+exp(-E))
  L<-dim(E)[1]
  DMAA_n<-rep(0,20)
  DMCA_vec<-rep(0,190)
  s<-0
  for(i in 1:20){
    X<-rep(0,L)
    for(k in 1:L){
      X[k]<-sum(E[1:k,i])
    }
    Xhatk_n<-rep(0,(L-(n-1)))
    for(t in 1:(L-(n-1))){
      Xhatk_n[t]<-(1/n)*sum(X[t:(t+(n-1))])
    }
    for(j in i:20){
      Y<-rep(0,L)
      for(k in 1:L){
        Y[k]<-sum(E[1:k,j])
      }
      DMCA_n<-0
      Yhatk_n<-rep(0,(L-(n-1)))
      for(t in 1:(L-(n-1))){
        Yhatk_n[t]<-(1/n)*sum(Y[t:(t+(n-1))])
        DMCA_n<-DMCA_n+(X[t]-Xhatk_n[t])*(Y[t]-Yhatk_n[t])
      }
      DMCA_n<-1/(L-(n-1))*DMCA_n
      if(j==i){
        DMAA_n[i]<-DMCA_n
      }
      else{
        s<-s+1
        DMCA_vec[s]<-DMCA_n
      }
    }
  }
  
  DMAA_n<-round(DMAA_n,digits = 4)
  DMCA_vec<-round(DMCA_vec,digits = 4)
  return(c(DMAA_n,DMCA_vec))
}

LPC_PSSM<-function(pssm_name){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  E<-x
  E<-1/(1+exp(-E))
  requireNamespace("phonTools",quietly = TRUE)
  LPC_vec<-rep(0,280)
  for(i in 1:20){
    LPC_vec[((i-1)*14+1):((i-1)*14+14)]<-phonTools::lpc(E[,i])
  }
  LPC_vec<-round(LPC_vec,digits = 4)
  return(LPC_vec)
}
pssm_ac <- function(pssm_name,lg=10){ #lg smaler than shortest protein length in database
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  s<-x
  #s<-1/(1+exp(-s))
  L<-dim(s)[1]
  sbar<-apply(s,2,mean)
  names(sbar)<-NULL
  sbar<-round(sbar,digits = 4)
  AC<-matrix(0,nrow = lg,ncol = 20)
  g<-0
  for(t in 1:lg){
    for(j in 1:20){
      for (i in 1:(L-t)) {
        g<-g+(s[i,j]-sbar[j])*(s[i+t,j]-sbar[j])
      }
      AC[t,j]<-g/(L-t)
      g<-0
    }
  }
  vec<-c()
  for(i in 1:lg){
    vec<-c(vec,AC[i,])
  }
  vec<-round(vec,digits = 4)
  return(vec)
}

PSSMBLOCK <- function(pssm_name,N=5){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  p<-x
  p<-1/(1+exp(-p))
  L<-dim(p)[1]
  w<-c();i<-1
  for(t in 1:N){
    M<-floor(L/t)
    while (i <=t){
      dd<-p[(1+(i-1)*M):(M+(i-1)*M),]
      v<-apply(dd,2,mean)
      names(v)<-NULL
      w<-c(w,v)
      i<-(i+1)
    }
    i<-1
  }
  w<-round(w,digits = 4)
  return(w)
}
pssm_cc <- function(pssm_name,g=10){ #g smaler than shortest protein length in database
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  s<-x
  L<-dim(s)[1]
  sbar<-apply(s,2,mean)
  names(sbar)<-NULL
  sbar<-round(sbar,digits = 4)
  CC<-matrix(0,nrow = g,ncol = 380)
  
  for(pg in 0:(g-1)){
    lg <- L - pg -1
    for(pj_1 in 0:19){
      sum_j_1 <- 0
      for (i in 0:(L-1)) {
        sum_j_1 <- sum_j_1 + s[i+1,pj_1+1]
      }
      sum_j_1 <- sum_j_1 / L
      
      for(pj_2 in 0:19){
        if(pj_2 != pj_1){
          sum_j_2 <- 0
          for(i in 0:(L-1)){
            sum_j_2 <- sum_j_2 + s[i+1,pj_2+1]
          }
          sum_j_2 <- sum_j_2 / L
          pssm_acjg = 0
          for(i in 0:(lg-1)){
            pssm_acjg = pssm_acjg + (s[i+1,pj_1+1]-sum_j_1) * (s[i+pg+1,pj_2+1]-sum_j_2)
          }
          pssm_acjg = pssm_acjg / lg
          if(pj_1 < pj_2){
            CC[pg,19*pj_1+(pj_2 -1)] = pssm_acjg
          } else {
            CC[pg,19*pj_1+(pj_2)] = pssm_acjg
          }
        }
      }
    }
    
  }
  
  vec<-c()
  for(i in 1:g){
    vec<-c(vec,CC[i,])
  }
  vec<-round(vec,digits = 4)
  return(vec)
}
pssm_composition <- function(pssm_name){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  p<-x
  #p<-1/(1+exp(-p))
  L<-dim(p)[1]
  s<-0
  x<-apply(p,2,mean)
  names(x)<-NULL
  x<-round(x,digits = 4)
  PSSM_AC<-matrix(0,20,20)
  for(g in 1:20){
    for(j in 1:20){
      for(i in 1:(L-g)){
        s<-s+(p[i,j]-x[j])*(p[i+g,j]-x[j])
      }
      PSSM_AC[j,g]<-s/(L-g)
      s<-0
    }
  }
  v<-c()
  for(j in 1:20){
    v<-c(v,PSSM_AC[j,])
  }
  v<-round(v,digits = 4)
  return(v)
}
PSSM_SD <- function(pssm_name){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  p<-x
  #p<-1/(1+exp(-p))
  L<-dim(p)[1]
  t<-apply(p,2,sum)
  names(t)<-NULL
  t<-round(t,digits = 4)
  I<-matrix(0,4,20)
  for(j in 1:20){
    i<-1;r<-L
    s<-p[i,j]
    sr<-p[r,j]
    while(s>(1/4)*t[j]) {
      i<-i+1
      s<-s+p[i,j]
    }
    I[1,j]<- i
    i<-1
    s<-p[i,j]
    while (s>(1/2)*t[j]) {
      i<-i+1
      s<-s+p[i,j]
    }
    I[2,j]<- i
    while (sr>(1/4)*t[j]) {
      r<-r-1
      sr<-sr+p[r,j]
    }
    I[3,j]<- r
    r<-L
    sr<-p[r,j]
    while (sr>(1/2)*t[j]) {
      r<-r-1
      sr<-sr+p[r,j]
    }
    I[4,j]<- r
  }
  vv<-c()
  I<-round(I,digits = 4)
  for(h in 1:4){
    vv<-c(vv,I[h,])
  }
  return(list(I,vv))
}
RPM_PSSM <- function(pssm_name){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  k2<-x[,1]
  k2<-as.character(k2)
  p<-x[,-1]
  mode(p)<-"integer"
  M<-c()
  s<-matrix(0,20,20)
  v<-c("A","R","N","D","C","Q","E","G","H","I","L","K","M","F","P","S","T","W","Y","V")
  for(i in 1:20){
    for(j in 1:20){
      mn<-p[,j][k2==v[i]]
      mn<-mn[mn>=0]
      if(length(mn) ==0){
        mn<-0
      }
      s[i,j] <- round(mean(mn),digits = 3)
    }
    M<-c(M,s[,i])
  }
  return(M)
}
smoothed_PSSM<-function(pssm_name,ws=7,w=50,v=NULL){ #ws in range 3,5,...,11 and w is in range 3,5,...,41
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  m<-x
  #m<-1/(1+exp(-m))
  L<-dim(m)[1]
  smoothed_PSSM<-matrix(0,L,20)
  h<-matrix(0,(ws-1)/2,20)
  k<-matrix(0,(ws-1)/2,20)
  E<-c()
  m<-rbind(h,m,k)
  for(i in 1:L){
    for(j in 0:(ws-1)){
      E<-rbind(E,m[i+j,])
    }
    smoothed_PSSM[i,]<-colSums(E)
    E<-c()
  }
  if ((w %% 2) == 1) {
    mh<-matrix(0,(w-1)/2,20)
    mk<-matrix(0,(w-1)/2,20)
    M<-rbind(mh,smoothed_PSSM,mk)
    d<-(w-1)/2 +1
    a<-1
    x2<-matrix(0,L,20*w)
    w1=w2<-c()
    for(i in (d-(w-1)/2):(d+(w-1)/2)){
      w2<-c(w2,M[i,])
    }
    x2[a,]<-w2
    a<-a+1
    d<-i+1
    while(d<=(L+(w-1))){
      w2<-c(w2[21:length(w2)],M[d,])
      x2[a,]<-w2
      a<-a+1
      d<-d+1
      
    }
  }else {
    mh<-matrix(0,w/2,20)
    mk<-matrix(0,w/2,20)
    M<-rbind(mh,smoothed_PSSM,mk)
    d<-1
    a<-1
    x2<-matrix(0,L,20*w)
    w1=w2<-c()
    for(i in 1:w){
      w2<-c(w2,M[i,])
    }
    x2[a,]<-w2
    a<-a+1
    d<-i+1
    while(d<=(L+w-1)){
      w2<-c(w2[21:length(w2)],M[d,])
      x2[a,]<-w2
      a<-a+1
      d<-d+1
      
    }
  }
  if(length(v)!=0){
    y<-x2[v,]
    #y<-as.data.frame(y)
    #rownames(y)<-v
    #colnames(y)<-1:(w*20)
  }
  else{
    y<-x2[floor(dim(x2)[1]/2),]
    #y<-as.data.frame(y)
    #rownames(y)<-1:L
    #colnames(y)<-1:(w*20)
  }
  y<- 2*(y-min(y))/(max(y)-min(y)) -1
  return(round(y,digits = 4))
}

SOMA_PSSM<-function(pssm_name){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  E<-x
  E<-1/(1+exp(-E))
  L<-dim(E)[1]
  somaPSSM<-rep(0,160)
  s<-0
  for(n in 2:9){
    for(j in 1:20){
      c2MA<-0
      Yhatn_i<-rep(0,L)
      for(i in n:L){
        Yhatn_i[i]<-sum(E[(i-(n-1)):i,j])
        c2MA<-c2MA+(E[i,j]-Yhatn_i[i])^2
      }
      c2MA<-1/(L-n)*c2MA
      s<-s+1
      somaPSSM[s]<-c2MA
    }
  }
  return(round(somaPSSM,digits = 4))
}
SVD_PSSM<-function(pssm_name){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  p<-x
  p<-1/(1+exp(-p))
  SVD_vec<-svd(p)
  SVD_vec<-SVD_vec$d
  SVD_vec<-round(SVD_vec,digits = 3)
  return(SVD_vec)
}
trigrame_pssm<-function(pssm_name){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  p<-x
  p<-1/(1+exp(-p))
  L<-dim(p)[1]
  t<-array(0,dim = c(20,20,20))
  k<-1
  vec<-vector(mode = "numeric",length = 8000)
  for(m in 1:20){
    for(n in 1:20){
      for(r in 1:20){
        for(i in 1:(L-2)){
          t[m,n,r]<-t[m,n,r]+p[i,m]*p[i+1,n]*p[i+2,r]
        }
        vec[k]<-t[m,n,r]
        k<-k+1
      }
    }
  }
  return(round(vec,digits = 4))
}

getwd()
list.files("input/",
           pattern ="*.pssm") -> all_input
#all_input
#install.packages("tidyverse")
library(tidyverse)
aadp2 <- data.frame()
dpc2 <- data.frame()
aatptpc <- data.frame()
DP_PSSM2 <- data.frame()
AB2 <- data.frame()
consunsus2 <- data.frame()
CS_PSe_PSSM2 <- data.frame()
Discrete_Cosine_Transform2 <- data.frame()
#disulfid2 <- data.frame()
grey_pssm_pseAAC2 <- data.frame()
DFMCA_PSSM2 <- data.frame()
LPC_PSSM2 <- data.frame()
pssm_ac2 <- data.frame()
PSSMBLOCK2 <- data.frame()
pssm_cc2 <- data.frame()
pssm_composition2 <- data.frame()
PSSM_SD2 <- data.frame()
RPM_PSSM2 <- data.frame()
smoothed_PSSM2 <- data.frame()
SOMA_PSSM2 <- data.frame()
SVD_PSSM2 <- data.frame()
trigrame_pssm2 <- data.frame()
pse_pssm2<-data.frame()
name<-data.frame()

for(filename in all_input){
  #df <-read.delim(file = paste0("input/",filename))
  #df <-read.delim(file = paste0("input/",filename))
  newname<-t(filename)
  #newname1<-t(newname)
  name<-rbind(name,newname)
  pse_pssm1<-t(pse_pssm(paste0("input/",filename)))
  pse_pssm2<-rbind(pse_pssm2,pse_pssm1)
  
  aadp1<-t(aadp_pssm(paste0("input/",filename)))
  aadp2<-rbind(aadp2,aadp1)
  
  
  dpc1<-t(dpc_pssm(paste0("input/",filename)))
  dpc2<-rbind(dpc2,dpc1)
  #newdata=t(data)
  
  
  AB1<-t(AB_PSSM(paste0("input/",filename)))
  AB2<-rbind(AB2,AB1)
  
  
  DP_PSSM1<-t(DP_PSSM(paste0("input/",filename)))
  DP_PSSM2<-rbind(DP_PSSM2,DP_PSSM1)
  
  Discrete_Cosine_Transform1<-t(Discrete_Cosine_Transform(paste0("input/",filename)))
  Discrete_Cosine_Transform2<-rbind(Discrete_Cosine_Transform2,Discrete_Cosine_Transform1)
  
  
  
  grey_pssm_pseAAC1<-t(grey_pssm_pseAAC(paste0("input/",filename)))
  grey_pssm_pseAAC2<-rbind(grey_pssm_pseAAC2,grey_pssm_pseAAC1)
  
  DFMCA_PSSM1<-t(DFMCA_PSSM(paste0("input/",filename)))
  DFMCA_PSSM2<-rbind(DFMCA_PSSM2,DFMCA_PSSM1)
  
  LPC_PSSM1<-t(LPC_PSSM(paste0("input/",filename)))
  LPC_PSSM2<-rbind(LPC_PSSM2,LPC_PSSM1)
  
  pssm_ac1<-t(pssm_ac(paste0("input/",filename)))
  pssm_ac2<-rbind(pssm_ac2,pssm_ac1)
  
  PSSMBLOCK1<-t(PSSMBLOCK(paste0("input/",filename)))
  PSSMBLOCK2<-rbind(PSSMBLOCK2,PSSMBLOCK1)
  
  pssm_cc1<-t(pssm_cc(paste0("input/",filename)))
  pssm_cc2<-rbind(pssm_cc2,pssm_cc1)
  
  pssm_composition1<-t(pssm_composition(paste0("input/",filename)))
  pssm_composition2<-rbind(pssm_composition2,pssm_composition1)
  
  
  RPM_PSSM1<-t(RPM_PSSM(paste0("input/",filename)))
  RPM_PSSM2<-rbind(RPM_PSSM2,RPM_PSSM1)
  
  smoothed_PSSM1<-t(smoothed_PSSM(paste0("input/",filename)))
  smoothed_PSSM2<-rbind(smoothed_PSSM2,smoothed_PSSM1)
  
  SOMA_PSSM1<-t(SOMA_PSSM(paste0("input/",filename)))
  SOMA_PSSM2<-rbind(SOMA_PSSM2,SOMA_PSSM1)
  
  SVD_PSSM1<-t(SVD_PSSM(paste0("input/",filename)))
  SVD_PSSM2<-rbind(SVD_PSSM2,SVD_PSSM1)
  
  trigrame_pssm1<-t(trigrame_pssm(paste0("input/",filename)))
  trigrame_pssm2<-rbind(trigrame_pssm2,trigrame_pssm1)
  
}


#name
#write.csv(name, file=paste0("output/","name.csv"))
#data2$name<-name
pse_pssm2<- cbind(name,pse_pssm2)
aadp2 <- cbind(name,aadp2)
dpc2 <- cbind(name, dpc2)
AB2 <- cbind(name, AB2)

DP_PSSM2<- cbind(name, DP_PSSM2)
#CS_PSe_PSSM2<- cbind(name, CS_PSe_PSSM2)
Discrete_Cosine_Transform2<- cbind(name, Discrete_Cosine_Transform2)
#disulfid2<- cbind(name, disulfid2)

grey_pssm_pseAAC2<- cbind(name, grey_pssm_pseAAC2)
DFMCA_PSSM2<- cbind(name, DFMCA_PSSM2)
LPC_PSSM2<- cbind(name, LPC_PSSM2)
pssm_ac2<- cbind(name, pssm_ac2)
PSSMBLOCK2<- cbind(name, PSSMBLOCK2)
pssm_cc2<- cbind(name, pssm_cc2)
pssm_composition2<- cbind(name, pssm_composition2)

RPM_PSSM2<- cbind(name, RPM_PSSM2)
smoothed_PSSM2<- cbind(name,smoothed_PSSM2)
SOMA_PSSM2<- cbind(name,SOMA_PSSM2)
SVD_PSSM2<- cbind(name, SVD_PSSM2)
trigrame_pssm2<- cbind(name, trigrame_pssm2)

write.csv(pse_pssm2, file=paste0("output/","Pse_PSSM.csv"))
write.csv(aadp2, file=paste0("output/","aadp_pssm6.csv"))
write.csv(dpc2, file=paste0("output/","dpc_pssm6.csv"))
write.csv(AB2, file=paste0("output/","AB_PSSM6.csv"))

write.csv(DP_PSSM2, file=paste0("output/","DP_PSSM6.csv"))
#write.csv(CS_PSe_PSSM2, file=paste0("output/","CS_PSe_PSSM6.csv"))
write.csv(Discrete_Cosine_Transform2, file=paste0("output/","Discrete_Cosine_Transform6.csv"))
#write.csv(disulfid2, file=paste0("output/","disulfid1.csv"))

write.csv(grey_pssm_pseAAC2, file=paste0("output/","grey_pssm_pseAAC6.csv"))
write.csv(DFMCA_PSSM2, file=paste0("output/","DFMCA_PSSM6.csv"))
write.csv(LPC_PSSM2, file=paste0("output/","LPC_PSSM6.csv"))
write.csv(pssm_ac2, file=paste0("output/","pssm_ac6.csv"))
write.csv(PSSMBLOCK2, file=paste0("output/","PSSMBLOCK6.csv"))
write.csv(pssm_cc2, file=paste0("output/","pssm_cc6.csv"))
write.csv(pssm_composition2, file=paste0("output/","pssm_composition6.csv"))

write.csv(RPM_PSSM2, file=paste0("output/","RPM_PSSM6.csv"))
write.csv(smoothed_PSSM2, file=paste0("output/","smoothed_PSSM6.csv"))
write.csv(SOMA_PSSM2, file=paste0("output/","SOMA_PSSM6.csv"))
write.csv(SVD_PSSM2, file=paste0("output/","SVD_PSSM6.csv"))
write.csv(trigrame_pssm2, file=paste0("output/","trigrame_pssm6.csv"))


#write.csv( newdata, file=paste0("output/","aac_pssm.csv"))





library(PSSMCOOL)
aac_pssm <- function(pssm_name){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  m2<-x
  #m2<-1/(1+exp(-m2))
  L<-dim(m2)[1]
  AAC<-apply(m2,2,mean)
  names(AAC)<-NULL
  AAC<-round(AAC,digits = 4)
  return(AAC)
}
#x<-aac_pssm("input/016205_NYSGRC.pssm")
#y<-t(x)
#write.csv( y, file="output/t016205_NYSGRC.csv")
pse_pssm <- function(pssm_name,g=1){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  E<-x
  #E<-1/(1+exp(-E))
  L<-dim(E)[1]
  Ebar<-apply(E,2,mean)
  names(Ebar)<-NULL
  Ebar<-round(Ebar,digits = 4)
  G<-rep(0,20*g)
  for(lg in 1:g){
    for (j in 1:20) {
      k<-j+20*(lg-1)
      for(i in (1:(L-lg))){
        G[k]<-G[k]+(E[i,j]-E[i+lg,j])^2
      }
      G[k]<-(1/(L-lg))*G[k]
    }
  }
  
  v<-c(Ebar,G)
  v<-round(v,digits = 4)
  
  return(v)
  
}
#X<-pse_pssm("016205_NYSGRC.pssm")
#X
dpc_pssm <- function(pssm_name){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  m2<-x
  #m2<-1/(1+exp(-m2))
  L<-dim(m2)[1]
  y<-matrix(0,nrow = 20,ncol = 20)
  for (i in 1:20) {
    for (j in 1:20) {
      for (k in 1:(L-1)) {
        y[i,j]<-y[i,j]+m2[k,i]*m2[k+1,j]
      }
    }
  }
  y<-(1/(L-1))*y
  dipep<-c()
  for(i in 1:20){
    dipep<-c(dipep,y[i,])
  }
  dipep<-round(dipep,digits = 4)
  return(dipep)
}
#X<-dpc_pssm("016205_NYSGRC.pssm")
#X
aadp_pssm <- function(pssm_name){
  x<-read.delim(pssm_name,skip = 2,sep = "",header = FALSE)
  x<-x[-1,-c(1,23:44)]
  d<-which(x=="Lambda")
  if(length(d)!=0){
    x<-x[-c(d:dim(x)[1]),]
  }
  x<-x[,-1]
  colnames(x)<-NULL
  rownames(x)<-NULL
  x<-as.matrix(x)
  mode(x)<-"integer"
  m2<-x
  #m2<-1/(1+exp(-m2))
  L<-dim(m2)[1]
  AAC<-apply(m2,2,mean)
  names(AAC)<-NULL
  AAC<-round(AAC,digits = 4)
  y<-matrix(0,nrow = 20,ncol = 20)
  for (i in 1:20) {
    for (j in 1:20) {
      for (k in 1:(L-1)) {
        y[i,j]<-y[i,j]+m2[k,i]*m2[k+1,j]
      }
    }
  }
  y<-(1/(L-1))*y
  vec<-c()
  for(i in 1:20){
    vec<-c(vec,y[i,])
  }
  vec<-round(vec,digits = 4)
  AADP<-c(AAC,vec)
  return(AADP)
}
