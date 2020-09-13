######## PART 1: FACTORIAL FUNCTION##########

############ FATORIAL LOOP###########
factorial_loop<-function(n){
    total_n=1
        if(n>0){
            numeros=c(seq(1:n))
            for (i in numeros){
                total_n=numeros[i]*total_n
            }
            return(total_n)
        }
        else if(n==0){
            return(total_n)
        }
    else if(n<0){
        message('Only positive numbers')
    }}
############ FACTORIAL REDUCE###########
library(purrr)
factorial_reduce <- function(n) {
    total_n = 1
    if (n > 0) {
        numeros = c(seq(1:n))
        total_n = reduce(numeros, function(x, y) {
            x * y
        })
        return(total_n)
    }
    else if (n == 0) {
        return(total_n)
    }
    else if (n < 0) {
        message('Only positive numbers')
    }
}
############ FACTORIAL RECURSION#######
factorial_recursion <- function(n) {
    total_n = 1
    if (n > 0) {
        total_n = n * factorial_recursion(n - 1)
        return(total_n)
    }
    else if (n == 0) {
        return(total_n)
    }
    else if (n < 0) {
        message('Only positive numbers')
    }
}

############ FACTORIAL MEMOIZATION########
#TABLA 
factorial_table=c(rep(NA,10))
factorial_memoization <-function(n){
    total_n=1
    if (n > 0) {
        factorial_table[n]<<- n * factorial_memoization(n - 1)
        return(factorial_table[n])
    }
    else if (n == 0) {
        return(total_n)
    }
    else if (n < 0) {
        message('Only positive numbers')
    }
}
############ MICROBENCHMARK ########
library(microbenchmark)
micro_bench<-function(n){
    microbenchmark(
        a=factorial_loop(n),
        b=factorial_recursion(n),
        c=factorial_reduce(n),
        d=factorial_memoization(n)
    )
}
###for 20
factorial_table=c(rep(NA,20))
micro_bench(20)
###for 100
factorial_table=c(rep(NA,100))
micro_bench(100)
###for 2000
factorial_table=c(rep(NA,2000))
micro_bench(2000)


