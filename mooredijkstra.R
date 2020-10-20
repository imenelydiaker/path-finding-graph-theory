A= rbind(c(0,7,1,0,0,0), 
         c(0,0,0,4,0,1),
         c(0,5,0,0,2,7),
         c(0,0,0,0,0,0),
         c(0,2,0,5,0,0),
         c(0,0,0,0,3,0))
A
pi=c(5,1,2,Inf,3,7)
i=which.min(pi)
i
pi[i]

X=1:6
S=c(1,2)
Sb=setdiff(X,S)
pi[Sb]
j=which.min(pi[Sb])
j
Sb[j]
pi[Sb[j]]

X
S
Sb

moore_dijkstra = function(X,A,s){
  #INPUT
  #A : matrice d'adjacence
  #X : liste des sommets
  #s : sommet initial
  
  #INTIALISATION
  S = c(s)
  Sb = setdiff(X,S)
  B = A
  pi = list()
  ind = which(B[s,]==0)
  B[s,ind] = Inf
  B[s,s]=0
  pi = B[s,]
  
  #Plus court chemin
  while(length(Sb)>0){
    pi_ietoile = which.min(pi[Sb])
    ietoile = Sb[pi_ietoile[1]]
    
    succ = which(A[ietoile,] != 0)
    j = intersect(succ,Sb)
    if(length(j) != 0){
      valmin = min(pi[[j]][1],pi[[ietoile]][1] + A[ietoile,j])
      pi[[j]][1] = valmin
    }
    
    Sb = setdiff(Sb,Sb[pi_ietoile[1]])
    print(pi)
    print(Sb)
  }
}

moore_dijkstra(X,A,2)

ford_bellman = function(X,A,s){
  #INPUT 
  # X : liste des sommets
  # A : matrice d'adjacence
  # s : sommet initial
  
  #INTIALISATION
  S = c(s)
  Sb = setdiff(X,S)
  pi = c()
  pi[s] = 0
  pi[Sb] = Inf
  
  #Calcul du plus court chemin
  repeat{
    for(i in Sb){
      pi_init = pi
      j = which(A[,i]!=0)
      pi[i] = min(pi[i], pi[j] + A[j,i])
      print(pi[i])
    }
    print(pi)
    if(length(setdiff(pi_init, pi))==0){
      break
    }
  }
  return(pi)
}

C= rbind( c(0,7,8,0,0,0)
          ,c(0,0,0,4,1,2)
          ,c(0,2,0,0,0,2)
          ,c(0,0,0,0,0,0)
          ,c(0,0,-2,2,0,0)
          ,c(0,0,0,0,3,0))
  
ford_bellman(X,C,2)

