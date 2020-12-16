# set.seed(1000)
# x <- rcauchy(100, 1, 2)
# x = sort(x)
# y = qqnorm (x)
# f <- approx (x , y$x )
# qqnorm(x)
# qqnorm(f$y)

# different methods leads to same result
# just checking
# histogram of final result look normal
set.seed(1000)
x <- rgamma(100, 2, 1)
x = sort(x)
p = ppoints(x)
new_data = qnorm(p)
new_data1 <- qqnorm(sort(x))
new_data2 <- qqnorm(x)
all.equal(new_data, new_data1$x, new_data2$x)
hist(new_data)

