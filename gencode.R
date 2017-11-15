
codetargets <- read.table("codetargets.txt")
codeneighbors <- read.table("codeneighbors.txt")

Ncodons <- 61
Npairs <- 526

isTransition <- function(n1,n2) {

    ((n1 == 'A') && (n2 == 'G')) ||
    ((n1 == 'G') && (n2 == 'A')) ||
    ((n1 == 'C') && (n2 == 'T')) ||
    ((n1 == 'T') && (n2 == 'C'))
}

isTransversion <- function(n1,n2)   {
    ! isTransition(n1,n2)
}

neighborsSynNonSyn <- function(s)    {

    ret <- c(0,0)
    for (i in 1:Ncodons)    {
        if (s == codetargets[i,1])  {
            ret <- c(codetargets[i,2]+codetargets[i,3],codetargets[i,4]+codetargets[i,5])
        }
    }
    ret
}

diffSynNonSyn <- function(s1,s2)    {

    ret <- c(0,0)
    for (i in 1:Npairs) {
        if ((s1 == codeneighbors[i,1]) && (s2 == codeneighbors[i,2]))   {
            ret <- c(codeneighbors[i,3]+codeneighbors[i,4],codeneighbors[i,5]+codeneighbors[i,6])
        }
    }
    ret
}

neighborsSynNonSynTsTv <- function(s)    {

    ret <- c(0,0,0,0)
    for (i in 1:Ncodons)    {
        if (s == codetargets[i,1])  {
            ret <- c(codetargets[i,2],codetargets[i,3],codetargets[i,4],codetargets[i,5])
        }
    }
    ret
}

diffSynNonSynTsTv <- function(s1,s2)    {

    ret <- c(0,0,0,0)
    for (i in 1:Npairs) {
        if ((s1 == codeneighbors[i,1]) && (s2 == codeneighbors[i,2]))   {
            ret <- c(codeneighbors[i,3],codeneighbors[i,4],codeneighbors[i,5],codeneighbors[i,6])
        }
    }
    ret
}

