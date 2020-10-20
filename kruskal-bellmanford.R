A = cbind(c(0,6,5,1,10),
          c(6,0,4,2,8),
          c(5,4,0,7,0),
          c(1,2,7,0,0),
          c(10,8,0,0,0))

X = 1:5

listes_aretes = function(A){
  # Etant donnée A la matrice d'adjacence d'un graphe n'est pas orienté 
  # alors A est symétrique => A[i,j]=A[j,i]
  # alors on ne considère que la partie triangulaire supérieure ou inférieure 
  # de la matrice (diagonale non comprise)
  U = which((upper.tri(A, diag = FALSE) != 0) & (A!=0), arr.ind = TRUE)
  U = cbind(U,A[U])
  return(U)
}
print(listes_aretes(A))

symetrisation = function(A){
  A = 0.5*(A + t(A))
  A[which(A!=0)] = 1
  return(A)
}

test_cycle = function(A,s){
  # transformer a en une matrice non pondérée
  A[which(A>0)]=1
  
  # symétrisation de A
  if (isSymmetric.matrix(A) == FALSE) {
    A = symetrisation(A)
  }
  
  d = c()
  P = c(s) # P: pile de sommets
  d[1:dim(A)[1]] = -1 # Initialisation de d à -1
  d[s] = 0 # d(sommet_initial) = 0
  i=s
  
  while (length(P)>0){
    # S : successeurs de i non découverts dans d
    S = intersect(which(d == -1), which(A[i,] != 0))
    
    if(length(S)>0){
      j = S[1]
      d[j] = 0
      P = c(j,P) # ajouter j à la pile
      i = j
    }
    else{
      t = intersect(which(d != -1) , which(A[i,] != 0)) 
      Pprim = setdiff(P,c(P[1],P[2]))
      if (length(intersect(intersect(t,Pprim),s)) > 0) {
        return(TRUE)
      }
      else{
        d[i] = 1
        P = setdiff(P,c(P[1]))
        i = P[1]
      }
    }
  }
  return(FALSE)
}

test_cycle(B,5)

kruskal = function(A){
  # Tri de la liste
  U = listes_aretes(A)
  U = U[order(U[,3]),]
  N = nrow(U)
  Uprim = U[1,]
  k = 1
  
  # B : matrice d'adjacence du graphe Gprim=[X,Uprim]
  B = matrix(0, nrow = dim(A)[1], ncol = dim(A)[2])
  B[U[1,1],U[1,2]] = U[1,3]
  
  while((length(Uprim)/3 <= N) & k<=N){
    k = k + 1
    # matrice du sous-graphe partiel Gprim induit par Uprim et X
    B[U[k,1],U[k,2]] = U[k,3]
    # tester le cycle dans Gprim
    while(test_cycle(B, U[k,1]) & k<=N){
      # supprimer l'arête qui cause un cycle
      B[U[k,1],U[k,2]] = 0
      # passer à la prochaine arête dans la liste
      k = k + 1
      # ajouter l'arête à Gprim
      if(k>N){
        break
      }
      B[U[k,1],U[k,2]] = U[k,3]
    }
    if(k>N){
      break
    }
    Uprim = rbind(Uprim, U[k,])
  }
  return(Uprim)
}

kruskal(A)

B = rbind(c(0,8,13,6,9,10,7),
         c(8,0,0,0,0,0,5),
         c(12,0,0,17,0,0,0),
         c(6,0,17,0,0,0,0),
         c(9,0,0,0,0,2,0),
         c(10,0,0,0,2,0,0),
         c(7,5,0,0,0,0,0))
Y = 1:7

print(listes_aretes(B))
kruskal(B)

ford_bellman = function(X,A,s){
  #INPUT 
  # X : liste des sommets
  # A : matrice d'adjacence
  # s : sommet initial
  
  #régler le problème des circuits absorbants
  
  #INTIALISATION
  S = c(s)
  Sb = setdiff(X,S)
  pi = c()
  pi[s] = 0
  pi[Sb] = Inf
  
  #Calcul du plus court chemin
  repeat{
    pi_init = pi
    for(i in Sb){
      j = which(A[,i] != 0)
      pi[i] = min(pi[i], min(pi[j] + A[j,i]))
    }
    if(length(setdiff(pi, pi_init))==0){
      break
    }
  }
  return(pi)
}


