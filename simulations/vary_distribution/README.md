# Scripts for designs where only x categories var

Scripts for running several simulations of raw and normalized distance measures to understand their distribution. Objective is to understand how for different values $\omega$ and $\mu$ affects the value of WPD and $lambda$


## Normal Distribution

- How the mean changes across x-categories in the simulation design?

The mean varies across x-axis categories in the following way:
$$\mu_{j.} = \mu + j\omega$$ where,   
$j$ in the index of the x-category  
$\mu$ is the mean of the first x-axis category  
Values of $\omega$ range from $1, 2, \dots, 25$ 

Vector Equation (used in code):  $$\mu_* = \mu + seq(0, (nx -1 ), by = 1)\omega$$

- The distribution of the first x-axis category considered are $N(0,1)$, $N(\mu, 1)$,$N(0, \sigma)$, $N(\mu, \sigma)$,

