
# from rethinking installation instructions:
install.packages(c("coda","mvtnorm","devtools","loo","dagitty"))
library(devtools)
devtools::install_github("rmcelreath/rethinking")
# I get an error that Rstan was build under R version 3.6. 


# This code was proposed as a solution:
#https://github.com/rmcelreath/rethinking/issues/174
install.packages(c("mvtnorm","loo","coda"), repos="https://cloud.r-project.org/",dependencies=TRUE)
options(repos=c(getOption('repos'), rethinking='http://xcelab.net/R'))
install.packages('rethinking',type='source')
# It worked for me