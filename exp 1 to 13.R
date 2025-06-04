#exp 1a
number<-as.integer(readline(prompt="Please enter a number for Table"))
for(t in 1:10)
{
  print(paste(number,'*',t,'=',number*t))
}

#exp 1b
num=as.integer(readline(prompt='Enter a number:'))
if((num%%2)==0){
  print(paste(num,'is Even Number'))
}else{
  print(paste(num,'is Odd number'))
}

#exp 2a
year = as.integer(readline(prompt="Enter a year: "))
if((year %% 4) == 0) {
  if((year %% 100) == 0) {
    if((year %% 400) == 0) {
      print(paste(year,"is a leap year"))
    } else {
      print(paste(year,"is not a leap year"))
    }
  } else {
    print(paste(year, "is a leap year"))
  }
} else {
  print(paste(year,"is not a leap year"))
}

#exp 2b
number = as.double(readline(prompt="Enter a number: "))
if(number > 0) {
  print("It is a Positive number")
} else {
  if(number == 0) {
    print("number is Zero")
  } else {
    print("It is a Negative number")
  }
}

#exp 3a
n <- as.integer(readline(prompt = "Enter a number: "))
if(n < 0){
  print("Enter a positive number")
} else {
  sum <- 0
  while(n > 0){
    sum <- sum + n
    n <- n - 1
  }
  print(paste("The sum of numbers up to the given limit is", sum))
}

#exp 3b
total_terms <- as.integer(readline(prompt = "How many terms? "))
num1 <- 0
num2 <- 1  
count <- 2
if (total_terms == 0) {
  print("Please enter a positive integer")
} else{ 
  if (total_terms == 1) {
    print("Fibonacci sequence:")
    print(num1)
    } else {
      print("Fibonacci sequence:")
      print(num1)
      print(num2)
      while (count < total_terms) {
        nxt <- num1 + num2
        print(nxt)
        num1 <- num2
        num2 <- nxt
        count <- count + 1
      }
    }
}

#exp 4a
num <- as.integer(readline(prompt="Enter a number: "))
sum <- 0
temp <- num
while (temp > 0) {
  digit <- temp %% 10
  sum <- sum + (digit ^ 3)
  temp <- floor(temp / 10)
}
if (num == sum) {
  print(paste(num, "is an Armstrong number"))
} else {
  print(paste(num, "is not an Armstrong number"))
}

#exp 4b
num <- as.integer(readline(prompt = "Enter a number: "))
flag <- 0
if (num > 1) {
  flag <- 1
  for (i in 2:(num - 1)) {
    if ((num %% i) == 0) {
      flag <- 0
      break
    }
  }
}
if (num == 2)
  flag <- 1
if (flag == 1) {
  print(paste(num, "is a prime number"))
} else {
  print(paste(num, "is not a prime number"))
}

#exp 5a
print_factors <- function(k) { 
  print(paste("The factors of given number",k,"are:")) 
  for(i in 1:k) { 
    if((k %% i)==0) { 
      print(i)
    }
  }
}
print_factors(7)

#exp 5b
recur_fact <- function(num) {
  if (num <= 1) {
    return(1)
  } else {
    return(num * recur_fact(num - 1))
  }
}
print(paste("The factorial of 5 is", recur_fact(5)))

#exp 6a
Convert_to_binary<-function(decnum){
  if(decnum>1){
    Convert_to_binary(as.integer(decnum/2))
  }
  cat(decnum%%2)
}
Convert_to_binary(10)

#exp 6b
fib<-function(n){
  if(n<=1){
    return(n)
  }else{
    return(fib(n-1)+fib(n-2))
  }
}
total=as.integer(readline(prompt = "how many terms?"))
if(total<=0){
  print("Please enter the positive integer")
}else{
  print("Fibonacci sequence")
  for(i in 0:(total-1)){
    print(fib(i))
  }
}

#exp 7a
hcf<-function(x,y){
  if(x>y){
    smaller=y
  }else{
    smaller=x
  }
  for(i in 1:smaller){
    if((x%%i==0)&&(y%%i==0)){
      hcf=i
    }
  }
  return(hcf)
}
n1=as.integer(readline(prompt = "enter first number:"))
n2=as.integer(readline(prompt = "enter second number:"))
print(paste("the hcf",n1,"and",n2,"is",hcf(n1,n2)))

#exp 7b
lcm<-function(x,y){
  if(x>y){
    greater=y
  }else{
    greater=x
  }
  while(TRUE){
    if((greater%%x==0)&&(greater%%y==0)){
      lcm=greater
      break
    }
    greater=greater+1
  }
  return(lcm)
}
n1=as.integer(readline(prompt = "enter first number:"))
n2=as.integer(readline(prompt = "enter second number:"))
print(paste("the hcf",n1,"and",n2,"is",lcm(n1,n2)))

#exp 8
add<-function(x,y){
  return(x+y)
}
subtract<-function(x,y){
  return(x-y)
}
multiply<-function(x,y){
  return(x*y)
}
divide<-function(x,y){
  return(x/y)
}
print("select Operation")
print("1.Add")
print("2.Substract")
print("3.Multiply")
print("4.Divide")
choice=as.integer(readline(prompt = "enter choice[1/2/3/4]:"))
n1=as.integer(readline(prompt = "enter first number:"))
n2=as.integer(readline(prompt = "enter the second number:"))
operator<-switch(choice,"+","-","*","/") 
result<-switch(choice,add(n1,n2),subtract(n1,n2),multiply(n1,n2),divide(n1,n2))
print(paste(n1,operator,n2,"=",result))

#exp 9a
numeric_vector<-c(1,2,3,4,5)
cat("Numeric vector:",numeric_vector,"\n")
character_vector<-c("apple","banana","orange","grape")
cat("Character vector:",character_vector,"\n")
sequence_vector<-1:5
cat("Sequence vector (using colon operator):",sequence_vector,"\n")
seq_vecor<-seq(from=1,to=10,by=2)
cat("sequence vector (using seq() function):",seq_vecor,"\n")
len_vector<-seq_len(5)
cat("Sequence vector (using seq_len()function):",len_vector,"\n")

#exp 9b
#manipulating data frames
roll_no<-c(1,2,3,4,5,6)
student_name<-c("ram","krishna","sai","ganga","balaji","siva")
grade<-c("B","A","0","A","C","A")
D=data.frame(roll_no,student_name,grade)
print(D)
#manipulating lists
Student_name<-c("sai","ram","krishna")
student_rollno<-c(501,502,503)
class<-c("cse")
section<-c("A","B","C")
item<-c("singing","playing","dance")
student_list=list(Student_name,student_rollno,class,section,item)
print(student_list)

#exp 10a
check_number<-function(x)
{
  if(x>0)
  {
    result<-"Positive"
  }else if(x<0){
    result<-"Negative"
  }else
  {
    result<-"Zero"
  }
  return(result)
}
number<-as.numeric(readline("enter number:"))
if(is.na(number))
{
  cat("Invalid input.please enter valid number.\n")
}else
{
  result<-check_number(number)
  cat("the entered number is:",result,"\n")
}

#exp 10b
sum_of_natural_numbers<-function(N)
{
  total_sum<-0
  for(i in 1:N)
  {
    total_sum<-total_sum+i
  }
  return(total_sum)
}
N<-as.integer(readline("Enter a positive integer N:"))
if(is.na(N)||N<=0)
{
  cat("Invalid input.please enter positive integer.\n")
}else
{
  result<-sum_of_natural_numbers(N)
  cat("The sum of the first",N,"natural numbers is:",result,"\n")
}

#exp 11a
sample_matrix<-matrix(1:12,nrow=3,byrow=TRUE)
sum_of_rows<-apply(sample_matrix,1,sum)
cat("Original Matrix:\n")
print(sample_matrix)
cat("\n Sum of Each Row:\n")
print(sum_of_rows)

#exp 11b
custom_environment <- new.env()
custom_environment$x <- 5
custom_environment$y <- 10
calculate_sum_product <- function(env) {
  with(env, {
    sum_result <- x + y
    product_result <- x * y
    
    cat("sum:", sum_result, "\n")
    cat("product:", product_result, "\n")
  })
}
calculate_sum_product(custom_environment)

#exp 12a
install.packages("plyr")
library(plyr)
data(mtcars)
summary_stats <- function(x) {
  mean_val <- mean(x)
  sd_val <- sd(x)
  return(c(mean = mean_val, sd = sd_val))
}
result <- ddply(mtcars, .(gear, cyl), summarise,
                mpg_mean = summary_stats(mpg)['mean'],
                mpg_sd = summary_stats(mpg)['sd'],
                hp_mean = summary_stats(hp)['mean'],
                hp_sd = summary_stats(hp)['sd'])
print(result)

#exp 12b
install.packages("ggplot2")
library(ggplot2)
data(iris)
scatter_plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  labs(
    title = "Scatter Plot: Sepal Length vs Sepal Width",
    x = "Sepal Length",
    y = "Sepal Width",
    color = "Species"  
  )
print(scatter_plot)

#exp 12b-2
data(mpg)
bar_chart<-ggplot(mpg,aes(x=class,y=hwy,fill=class))+geom_bar(stat = "summary",fun="mean",position = "dodge")+labs(title = "Average highway MPG by vehicle class",
                                                                                                                   x="vehicle Class",
                                                                                                                   y="Average Highway MPG",
                                                                                                                   fill="Vehicle Class")
print(bar_chart)

#exp 13a
set.seed(500)
x<-rnorm(1200)
qqnorm(x,main="qqline")
qqline(x,col="darkgreen",)

#exp 13a-2
y<-rlogis(800)
qqnorm(y,main="qqline")
qqline(y,col="darkgreen")

#exp 13b
df <- data.frame(points = c(7, 7, 9, 10, 13, 14, 12, 10, 16, 19, 22, 18),
                 hours = c(0, 1, 2, 3, 2, 6, 4, 3, 4, 5, 8, 6),
                 program = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3))
df$program <- as.factor(df$program)
fit <- lm(points ~ hours + program, data = df)
summary(fit)

#exp 13b-2
set.seed(123)
data <- data.frame(
  Age = c(25, 30, 35, 40, 45, 50),
  Gender = c("Male", "Female", "Male", "Female", "Male", "Female"),
  Income = c(50000, 60000, 75000, 80000, 90000, 100000)
)
data$Gender <- as.factor(data$Gender)
dummy_gender <- model.matrix(~ Gender - 1, data = data)
data <- cbind(data, dummy_gender)
model <- lm(Income ~ Age + GenderMale + GenderFemale, data = data)
summary(model)