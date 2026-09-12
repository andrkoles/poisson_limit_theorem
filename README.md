## Poisson Limit Theorem

### Poisson and Binomial Distributions

Probability mass functions of Binomial and Poisson distributions respectively:

$$Pr(X=k)=\binom{n}{k}p^{k}q^{n-k}$$

$$Pr(X=k)=\lambda^{k}\frac{\mathrm{e}^{-\lambda}}{k!}$$

Where $n$ is the number of independent Bernoulli trials and $p$ is the probability

of success of a single Bernoulli trial while $\lambda$ is the parameter of the

Poisson distribution.

### Theorem

If $n\rightarrow\infty$ and $p\rightarrow0$ and the product of $n$ and $p$

remains fixed and equal to $\lambda$ then the Binomial distribution is well

approximated by the Poisson distribution with parameter $\lambda=np$.

_Reference_:

Exploring University Mathematics with Python, Siri Chongchitnan, Springer, 2023, p. 393

### Shiny App

In this Shiny app, the number of trials is controlled by a slider input which

takes values from 10 to 2000. Behind the scenes the value of $p$ changes so that

the product $np$ remains fixed and equal to $\lambda$ which here is 5. As the

number of trials grows larger the Binomial distribution converges to the
Poisson

distribution, as shown in the interactive plot on the right. You can view an

animation of the convergence by pressing the play button below the slider.

Given that you have `shiny`, `ggplot2`, `tibble`, `bslib` and `tidyr` packages

installed, you can run the app by executing the following command in R / RStudio

console:

```r
R> shiny::runGitHub(repo = "andrkoles/poisson_limit_theorem")
```

A screenshot of the app:

![](screenshots/screenshot.png)
