# Dataset from pracuj.pl website

How to install and use this package?

```
install_github("mi2-warsaw/pracuj/pracuj")

library(pracuj)
short <- get_offers(description=FALSE)
head(short)
long <- get_offers(description=TRUE)
head(long)
```

## UseCase

https://rawgit.com/mi2-warsaw/pracuj/master/analizy_05_15/czestosci.html

