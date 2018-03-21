
# About R --------------------------------------------------------------

# C, fortran, Java와 달리 Interactive 한 특징을 가지고 있다. 
# 시각화 등을 가능하게 하는 GUI를 가지고 있음. 

# starting R --------------------------------------------------------------
# GUI preference를 설정하는 방법 알아둘 것

plot(runif(10))


# R function --------------------------------------------------------------
# function()
function(save="default", status=0, runlast=TRUE){} # 인자와 인자디폴트 값, 인자값의 개념에 대해 알아두자. 
  
  

# 함수 예제 (종료함수)
q
# 에러창이 뜨는 것을 볼 수 있음. q라는 함수 자체에 대한 정보를 보여준다.
# 함수의 이름을 알려주는 기능이라 생각하면 된다.

#help 함수의 여러s형태
# help(q)
#
##keyword식 검색법. 뭘 검색해야할지 모를 때 사용하면 좋음
??"normal distribution"
help.search("normal distribution")

## 함수의 예제를 보고 싶을 때
example(mean)

## 패키지 관련 설명을 보고 싶을 때. 일반 함수 검색과 달리 옵션을 추가함에 집중하자. 
help(package = "datasets")
help(package = datasets) #those 2 reveal same result


# R packages --------------------------------------------------------------
getOption("defaultPackages")
update.packages()


# R objects(character integer factor etc.) --------------------------------
box <- 100
box

object # atomic, vector, matrix, dataframe list 등의 형태로 저장할 수 있음
mode #각 객체는 정해진 형태로 저장됨 
# #(numeric(integer realnumber complex), character logical NULL NA NaN)

# 각 객체의 자료형 조회하려면?
class()
typeof()
mode()

# 객 체의 자료형을 변경하고 싶으면?
is.integer() ; is.numeric(); is.character()
is.complex()
is.na()

# R for mathematical function ---------------------------------------------
# 논리 연산자에 대해 공부해보자. 
# %%  ; %/%
# + = == != <= >= & |

abs(-3.14) #절대값 산출
sqrt(4)
e <- exp(1); e #자연상수
log(4)
log10(10)
factorial(3)
choose(5,3) # combination
prod(2,3,5) ; prod(c(1,2,3)) ; prod(1:3)
sum(2,3,5) ; sum(1:10)
sin(pi/2) # sin 45도를 구하는 코드
sin(pi)#정확하게 1은 안 나오고 1에 근접한 값이 나옴
sin(30) ; sin(pi/30)#오류가 뜸
cos(45)
Inf ; NaN

round(3.14, 1)
round(3144, -2)
round(3144, -1)
round(1234567,-3)

trunc(5.88) #잘라내기
ceiling(5.88) #올림
floor(5.88) #내림 

# script editor & wd ------------------------------------------------------
# 스크립트 편집기를 오픈하는 방법. 
# 스크립트　상의　코드를　콘솔로　옮기는　방법．
#　ｗｏｒｋｓｐａｃｅ에　대해서는　다음주에．．　
source(file="c:/users/cau/desktop/week1.R")


# saving workspace --------------------------------------------------------


