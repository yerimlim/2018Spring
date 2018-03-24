Week 3
================
Yerim Lim
March 23, 2018

3주차 수업
==========

지난 시간 복습 : 자료의 복습
----------------------------

-   벡터에 대해 배웠다. 벡터의 생성과 관련 함수들을 배웠다.
-   객체의 종류에 대해서도 배웠다.
-   숫자형(정수 실수 복소수)
-   문자형
-   논리형
-   특수한 상태를 나타내는 상수 (NULL, NA, NaN)

``` r
x <- 2
x <- 3
1:10
c(2,3,4)
seq(1,3,by=0.5)
seq(1,3)
seq(1,7, length=3)
```

이번 시간에는 행렬을 배울 것이다. 객체의 종류

``` r
matrix(1:10, 2,5, byrow = T)
x <- sample(1:100, 20, replace = T)
x
sort(x)
x[20]=NA
x
length(x)
length(unique(x))
```

``` r
x <- sample(1:100, 20, replace = T)
sort(x, decreasing = TRUE)
-sort(-x) #sort의 decreasing옵션과 동일한 결과값을 도출한다. 
```

``` r
x <- 1:10
x[10] <- NA
# rank의 결과값이 sort와 다르다는 것에 주의하라. 
rank(x, na.last="keep")
rank(x, na.last=NA)
rank(x, na.last=F)
rank(x, na.last=TRUE)
```

### 헷갈리기 쉬운 `sort` `rank` `order`헷갈리지 말자.

``` r
order(x)
x <- sample(1:100,10)
x
order(x)
x[order(x)]
```

이번엔 결측치가 추가된 데이터로 해보자.

``` r
x[10] <- NA
x[order(x, na.last=NA)]
x[order(x, na.last="keep")]
x[order(x, na.last=TRUE)]
```

Subset 함수

``` r
z <- c(1,8,9,2,7,10,4,6,3,2)
z[z%%2==1] #홀수만 뽑아내는 조건
index <- (z%%2==1)
index
subset(z, index)
```

IF ELSE함를 이용할 수 있다.

``` r
tmp <- ifelse(z>0, 1, 2*z)
tmp
```

``` r
x <- 1:3
x
is.integer(x)
```

``` r
x <- 1:3
y <- c(1,2,3)
x==y
all(x==y)
any(x==y)
```

``` r
x
replace(x,1,3)
x[1] <- 3
x
```

행렬을 알아보자.

``` r
z <- 1:10
M <- matrix(z, ncol=2)
M
M2 <- matrix(z, ncol=2, byrow = TRUE)
M2
is.vector(M)
m3 <- as.vector(M)
m3
```

행렬에서 indexing을 이용하면 원하는 부분을 추출할 수 있다.

``` r
x <- seq(1,10)
x
x <- matrix(x,4,10,byrow=T)
x
x[3:4, 4:6]
```

벡터를 행렬로 바꾸는 또 다른 방법

``` r
x <- 1:10
dim(x) <- c(2,5)
x
```

paste를 이용해보자.

``` r
paste("y", "2001":"2006", sep=",")
```

    ## [1] "y,2001" "y,2002" "y,2003" "y,2004" "y,2005" "y,2006"

``` r
z <- 1:27
dim(z) <- c(3,3,3)
z
```

    ## , , 1
    ## 
    ##      [,1] [,2] [,3]
    ## [1,]    1    4    7
    ## [2,]    2    5    8
    ## [3,]    3    6    9
    ## 
    ## , , 2
    ## 
    ##      [,1] [,2] [,3]
    ## [1,]   10   13   16
    ## [2,]   11   14   17
    ## [3,]   12   15   18
    ## 
    ## , , 3
    ## 
    ##      [,1] [,2] [,3]
    ## [1,]   19   22   25
    ## [2,]   20   23   26
    ## [3,]   21   24   27
