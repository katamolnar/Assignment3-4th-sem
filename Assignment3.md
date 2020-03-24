### Riccardo intro video

univariate regession: 1 pred, 1 outcome, multiv: more preds, 1 outcome
======================================================================

Assignment 3 - Exploring causal inference issues
------------------------------------------------

In this assignment we explore some issues related to multiple
regressions (regressions with more than one predictor), and inferred
(causal) relations between variables. N.B. the data is simulated (to
make sure I know the actual mechanism generating it), but it’s based on
a real study. So bear with a longish introduction to get into the
details of what we are doing and why it is important.

### Altercentric intrusion in schizophrenia

People with schizophrenia often report altered control and distinction
of self-other representations: intrusive thoughts, hearing of voices,
delusions of mind reading, paranoia, etc (a substantial portion of the
psychotic symptoms experienced in schizophrenia). These have been
variously attributed to hypermentalizing (over attribution of mental
states to others), social impairment (over preoccupation with own
thought processes), hyper socialization (inability to inhibit
information from others), etc.

The current study investigates 1) whether schizophrenia is indeed
related to altered control and distinction of self-other
representations, in particular altercentric intrusions (inability to
inhibit social information), and 2) whether these are related to the
relevant psychotic symptoms. N.B. the actual study also investigates
egocentric intrusion, do check the papers below if interested.

The task is a slightly modified version of this:
<a href="https://www.ncbi.nlm.nih.gov/pubmed/20731512" class="uri">https://www.ncbi.nlm.nih.gov/pubmed/20731512</a>
You look at a picture with some dots visible to you, as well as with a
different person with a different set of dots visible to them. The
number of dots you see and that the other sees can be the same
(congruent condition) or not (incongruent condition). You are tasked to
indicate whether a given number (e.g. 3) matches the number of dots you
see (and the dots visible to the other person are irrelevant to the
task).

The tasks investigates altercentric intrusion: will your reaction time
change according to whether the other person is seeing the same amount
of dots as you, or not? The idea is that if you correctly inhibit social
information, your reaction time should not change, as the information
about the other person is not relevant. On the contrary, if you
nevertheless use task irrelevant social information, you’ll be slower at
indicating whether 3 is the right number of dots when the other person
sees a different amount of dots than you (conflicting information). The
bigger the difference between RTs in the congruent and incongruent
condition the bigger the altercentric intrusion effect.

For each participant you have 6 variables: 1) ID, 2)
AltercentricIntrusion (continuous score), 3) Diagnosis (schizophrenia
vs. control), 4) VoiceHearing (severity of voice hearing symptoms,
continuous score of the severity of the symptom as measured by a
clinician), 5) MindReading (severity of delusions of mind reading,
continuous score of the severity of the symptom as measured by a
clinician); 6) Apathy (severity of lack of motivation in taking care of
oneself, from washing to showing up at work, continuous score of the
severity of the symptom as measured by a clinician).

The research questions you have to answer are the following:

VoiceHearing and Mindreading should be related to altercentric intrusion, apathy is not.
----------------------------------------------------------------------------------------

First part
----------

Q1.1) Does schizophrenia involved altercentric intrusion? Define model
and priors. Test the implications of your priors (prior predictive
checks) and if needed adjust them. Run the model. Test the quality of
the fitted model (posterior predictive checks). Assess the evidence in
favor of an increased altercentric intrusion in schizophrenia. Report
the model and the results, including plots.

``` r
pacman::p_load(tidyverse, brms, patchwork)

# Load and prepare the data

d <- read.csv("Ass3.csv")
#summary(d)

d$Diagnosis <- plyr::revalue(as.character(d$Diagnosis), 
                             c("0"="Controls", "1"="Schizophrenia"))

d <- d %>% # here we make id and diagnosis a factor
  mutate(
    ID = as.factor(ID),
    Diagnosis = as.factor(Diagnosis)
  )

# Define the formula

AltercentricDiagnosis_f0 <- bf(
  AltercentricIntrusion ~ 1 + Diagnosis
)

AltercentricDiagnosis_f <- bf( #tells that it's a bayesian formula
  AltercentricIntrusion ~ 0 + Diagnosis
)

# Design the priors

get_prior(AltercentricDiagnosis_f0, family = gaussian, d) # gaussian bc outcome is continuous variable
```

    ##                 prior     class                   coef group resp dpar nlpar
    ## 1                             b                                             
    ## 2                             b DiagnosisSchizophrenia                      
    ## 3 student_t(3, 4, 10) Intercept                                             
    ## 4 student_t(3, 0, 10)     sigma                                             
    ##   bound
    ## 1      
    ## 2      
    ## 3      
    ## 4

``` r
get_prior(AltercentricDiagnosis_f, family = gaussian, d) # 3 betas, 1 for both, or one separate for each diagnosis and a sigma, but we only define the 1st beta which is for both diagnoses, and the sigma
```

    ##                 prior class                   coef group resp dpar nlpar bound
    ## 1                         b                                                   
    ## 2                         b      DiagnosisControls                            
    ## 3                         b DiagnosisSchizophrenia                            
    ## 4 student_t(3, 0, 10) sigma

``` r
priorDiagnosis <- c(
  prior(normal(4, 1), class = b), # b is beta, get mean and sd from summary of alterc intr, 1 sd
  prior(normal(1, 2), class = sigma) # mean is baseline model, 2 sd, can go up to 5
) 
#summary(d$AltercentricIntrusion) # 1 sd: 1*1.95 = 2, 2 to the left, 2 to the right

# Test the priors

AltercentricDiagnosis_PriorCheck_m <- brm(
  formula = AltercentricDiagnosis_f,
  data = d,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = "only" # we sample the prior we test want to test them now, and not the model
)
```

    ## Compiling the C++ model

    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '58ee1ee27bbbc25da51be768bc98b3fb' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 0.03 seconds (Warm-up)
    ## Chain 1:                0.026 seconds (Sampling)
    ## Chain 1:                0.056 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '58ee1ee27bbbc25da51be768bc98b3fb' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 0.031 seconds (Warm-up)
    ## Chain 2:                0.025 seconds (Sampling)
    ## Chain 2:                0.056 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '58ee1ee27bbbc25da51be768bc98b3fb' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 0.028 seconds (Warm-up)
    ## Chain 3:                0.027 seconds (Sampling)
    ## Chain 3:                0.055 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '58ee1ee27bbbc25da51be768bc98b3fb' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 0.026 seconds (Warm-up)
    ## Chain 4:                0.024 seconds (Sampling)
    ## Chain 4:                0.05 seconds (Total)
    ## Chain 4:

``` r
pp_check(AltercentricDiagnosis_PriorCheck_m, nsamples = 100) # light blue: simulated samples from the prior, dark: observed data
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
## Fitting the model
AltercentricDiagnosis_m <- brm(
  formula = AltercentricDiagnosis_f,
  data = d,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = T
)
```

    ## Compiling the C++ model
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '250368a6e8ef8b9b74282e648bfa3499' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 0.032 seconds (Warm-up)
    ## Chain 1:                0.033 seconds (Sampling)
    ## Chain 1:                0.065 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '250368a6e8ef8b9b74282e648bfa3499' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 0.031 seconds (Warm-up)
    ## Chain 2:                0.036 seconds (Sampling)
    ## Chain 2:                0.067 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '250368a6e8ef8b9b74282e648bfa3499' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 0.034 seconds (Warm-up)
    ## Chain 3:                0.035 seconds (Sampling)
    ## Chain 3:                0.069 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '250368a6e8ef8b9b74282e648bfa3499' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 0.032 seconds (Warm-up)
    ## Chain 4:                0.03 seconds (Sampling)
    ## Chain 4:                0.062 seconds (Total)
    ## Chain 4:

``` r
# Posterior predictive check
pp_check(AltercentricDiagnosis_m, nsamples = 100) # posterior looks much better than the prior from before
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
# we have the betas in the posterior and not the actual values
# you sample a value, make predictions with it and the predictions will have a distribution -> you do it 100 times here
# light blue: distributions of simulated data sets from the posterior, dark: distribution of actual/observed data

## Check the model for warnings
AltercentricDiagnosis_m # 2 estimates, for controls and for SCZ, sigma was estimated as about 1, makes sense, it's the sd in our data
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 0 + Diagnosis 
    ##    Data: d (Number of observations: 300) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## DiagnosisControls          3.86      0.06     3.74     3.98 1.00     4321
    ## DiagnosisSchizophrenia     4.23      0.10     4.03     4.43 1.00     3965
    ##                        Tail_ESS
    ## DiagnosisControls          2561
    ## DiagnosisSchizophrenia     2445
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.92      0.04     0.84     0.99 1.00     4246     2967
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# Hypothesis testing + updating check
plot(hypothesis(AltercentricDiagnosis_m,
           "DiagnosisSchizophrenia > DiagnosisControls")) #bc we can plot what the prior for this Hypothesis looks like and also the posterior
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-1-3.png)

``` r
# posterior is much more certain, learned from the data

hypothesis(AltercentricDiagnosis_m,
           "DiagnosisSchizophrenia > DiagnosisControls")
```

    ## Hypothesis Tests for class b:
    ##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (DiagnosisSchizop... > 0     0.36      0.12     0.16     0.57    1332.33
    ##   Post.Prob Star
    ## 1         1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
# evidence ratio: posterior is a bunch of samples, we want to count how many of them are compatible with our H, how may is above 0
# we count the incompatible ones (below 0)
# SCZ has a higher level of alt intr
# estimate: diff between SCZ and controls

# plot with error bars
conditional_effects(AltercentricDiagnosis_m)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-1-4.png)

``` r
plot(conditional_effects(AltercentricDiagnosis_m), points=T) # how true data looks like
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-1-5.png) The
model indicates a credible difference in altercentric intrusion in the
two groups supporting our hypothesis (b = 0.36, CIs = 0.16, 0.57, ER =
1332). Controls showed on average an altercentric intrusion effect of
3.86 (CIs 3.74, 3.98), and schizophrenia of 4.22 (CIs = 4.01, 4.43).
\[Add plot of the effects\]

SI (in methods part)
====================

The model had no divergences, a Rhat of 1, and Effective Sample Sizes
above 2000 for both Bulk and Tail. \[Add prior and posterior checks
plots; add updating check plot\]

\#SI Will cover this part: The model had no divergences (about warnings,
gonna cover them in next weeks), Rhat of 1, and effectove samples sizes
about 2000, for both Bulk and Tail. \[add prior and posterior check
plots and updating check plots\]

Q1.2) Is altercentric intrusion related to specific symptoms *in the
patients*? Identify which of the symptoms could be relevant. Should you
include more than one symptom? Build models, priors, predictive checks.
Assess the evidence and report models and results, including plots.
Discuss whether the results make sense. \# with only patients with
diagnosis of 1 (SCZ) \# whether the levels of alt intr relate to the
symptoms \# build the 3 models: alt intr is predicted by apathy or by
voice hearing or by mind reading, THEN start to think what should we
iclude for more symptoms \# single symptoms,then multiple regression

Voice hearing and Mind reading should be related to altercentric intrusion, but apathy should not.
--------------------------------------------------------------------------------------------------

``` r
# Scale variables that we are going to use
d <- d %>% 
  mutate(
    AltercentricIntrusion = scale(AltercentricIntrusion),
    VoiceHearing = scale(VoiceHearing),
    MindReading = scale(MindReading),
    Apathy = scale(Apathy)
  )

# Make subset only with SCZ patients
scz <- subset(d, Diagnosis=="Schizophrenia")


### Voice Hearing

# Define the formula
 VoiceHearing_f<- bf(
  AltercentricIntrusion ~ 1 + VoiceHearing
)
 
# Designing the priors
get_prior(VoiceHearing_f, family = gaussian, scz)
```

    ##                 prior     class         coef group resp dpar nlpar bound
    ## 1                             b                                         
    ## 2                             b VoiceHearing                            
    ## 3 student_t(3, 0, 10) Intercept                                         
    ## 4 student_t(3, 0, 10)     sigma

``` r
priorVoiceHearing <- c(
  prior(normal(0,.3), class = b), # beta can be negative here if there is a negative relationship # Riccardo: 0, .3
  prior(normal(1,2), class = sigma), # Riccardo: 1,2
  prior(normal(0,1), class = Intercept)) # where Voice hearing is 0 # for Riccardo: 0,1
# bc we scaled, they are centered around 0

# Test prior
VoiceHearing_PriorCheck_m <- brm(
  formula = VoiceHearing_f,
  data = scz,
  family = gaussian,
  prior = priorVoiceHearing,
  sample_prior = "only", # we sample the prior we test want to test them now, and not the model
  file = "AltercentricVoiceHearing_PC"
)
plot_1.1 <- pp_check(VoiceHearing_PriorCheck_m, nsamples = 100)

# Fitting the model
VoiceHearing_m <- brm(
  formula = VoiceHearing_f,
  data = scz,
  family = gaussian,
  prior = priorVoiceHearing,
  sample_prior = T,
  file = "AltercentricVoiceHearing"
)
plot_2.1 <- pp_check(VoiceHearing_m, nsamples = 100)
VoiceHearing_m
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + VoiceHearing 
    ##    Data: scz (Number of observations: 75) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept        0.26      0.18    -0.08     0.61 1.00     4322     2797
    ## VoiceHearing     0.08      0.15    -0.21     0.37 1.00     4209     3009
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.90      0.07     0.77     1.06 1.00     3559     2847
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# Hypothesis testing + updating check
plot(hypothesis(VoiceHearing_m,
           "VoiceHearing > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
# model has learned, post is more confident than prior

hypothesis(VoiceHearing_m,
           "VoiceHearing > 0") # the CIs overlap with 0, which means that there is a possibility that the effect is 0. But we don't know. Not what we hypothesized (we hypothesized positive relationship)
```

    ## Hypothesis Tests for class b:
    ##           Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (VoiceHearing) > 0     0.08      0.15    -0.17     0.32       2.37       0.7
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
# 2.25 as many samples that are compatible compared to those that are not (evidence ratio)

# Plot with error bars
conditional_effects(VoiceHearing_m)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
plot(conditional_effects(VoiceHearing_m), points=T)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-3.png)

``` r
# kinda bad
```

### Mind Reading

``` r
# Define formula
MindReading_f <- bf(
  AltercentricIntrusion ~ 1 + MindReading
)

# Design prior
get_prior(MindReading_f, family = gaussian, scz)
```

    ##                 prior     class        coef group resp dpar nlpar bound
    ## 1                             b                                        
    ## 2                             b MindReading                            
    ## 3 student_t(3, 0, 10) Intercept                                        
    ## 4 student_t(3, 0, 10)     sigma

``` r
priorMindReading <- c(
  prior(normal(0,.3), class = b),
  prior(normal(1,2), class = sigma),
  prior(normal(0,1), class = Intercept))
# look at summary of outcome and look at the mean to define mean for prior beta
#summary(scz$AltercentricIntrusion)

# Test prior
MindReading_PriorCheck_m <- brm(
  formula = MindReading_f,
  data = scz,
  family = gaussian,
  prior = priorMindReading,
  sample_prior = "only", # we sample the prior we test want to test them now, and not the model
  file = "AltercentricMindReading_PC"
)
plot_1.2 <- pp_check(MindReading_PriorCheck_m, nsamples = 100) 

# Fitting he model
MindReading_m <- brm(
  formula = MindReading_f,
  data = scz,
  family = gaussian,
  prior = priorMindReading,
  sample_prior = T,
  file = "AltercentricMindReading"
)
MindReading_m
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + MindReading 
    ##    Data: scz (Number of observations: 75) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept       0.24      0.12    -0.00     0.49 1.00     4072     2954
    ## MindReading     0.07      0.11    -0.14     0.28 1.00     3938     2507
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.90      0.07     0.77     1.06 1.00     4411     2846
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
plot_2.2 <- pp_check(MindReading_m, nsamples = 100)

# Hypothesis testing + updating check
plot(hypothesis(MindReading_m,
           "MindReading > 0")) 
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
hypothesis(MindReading_m,
           "MindReading > 0") # the CIs overlap with 0, which means that we don't actually know if it really has an effect, because it can be 0. Not what we hypothesized (we hypothesized positive relationship)
```

    ## Hypothesis Tests for class b:
    ##          Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (MindReading) > 0     0.07      0.11     -0.1     0.25       3.09      0.76
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
## Plot with error bars
conditional_effects(VoiceHearing_m)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
plot(conditional_effects(VoiceHearing_m), points=T)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-3.png)

### Apathy

``` r
# Define formula
Apathy_f <- bf(
  AltercentricIntrusion ~ 1 + Apathy
)

# Design prior
get_prior(Apathy_f, family = gaussian, scz)
```

    ##                 prior     class   coef group resp dpar nlpar bound
    ## 1                             b                                   
    ## 2                             b Apathy                            
    ## 3 student_t(3, 0, 10) Intercept                                   
    ## 4 student_t(3, 0, 10)     sigma

``` r
priorApathy <- c(
  prior(normal(0,.3), class = b),
  prior(normal(1,2), class = sigma),
  prior(normal(0,1), class = Intercept))

# Test prior
Apathy_PriorCheck_m <- brm(
  formula = Apathy_f,
  data = scz,
  family = gaussian,
  prior = priorApathy,
  sample_prior = "only", # we sample the prior we test want to test them now, and not the model
  file = "AltercentricApathy_PC"
)
plot_1.3 <-pp_check(Apathy_PriorCheck_m, nsamples = 100)

# Fitting the model
Apathy_m <- brm(
  formula = Apathy_f,
  data = scz,
  family = gaussian,
  prior = priorApathy,
  sample_prior = T,
  file = "AltercentricApathy"
)
Apathy_m
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + Apathy 
    ##    Data: scz (Number of observations: 75) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     0.48      0.16     0.17     0.79 1.00     4112     3065
    ## Apathy       -0.20      0.13    -0.46     0.06 1.00     4054     2924
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.89      0.08     0.75     1.05 1.00     3951     2961
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
pp_check(Apathy_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# Hypothesis testing + updating check
plot(hypothesis(Apathy_m,
           "Apathy < 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
hypothesis(Apathy_m,
           "Apathy < 0")
```

    ## Hypothesis Tests for class b:
    ##     Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
    ## 1 (Apathy) < 0     -0.2      0.13    -0.41     0.01      16.62      0.94     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
# quite substantial evidence that the effect is negative
plot(conditional_effects(Apathy_m), points=T)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-4-3.png)

``` r
# seems like it has a negative effect/beta (we hypothesized that there should be none)

# square estimate: get Rsquared: variance explained in %
```

### Voice Hearing and Mind Reading

``` r
# Define formula
VoiceMind_f <- bf(
  AltercentricIntrusion ~ 1 + VoiceHearing + MindReading
)

# Design prior
get_prior(VoiceMind_f, family = gaussian, scz)
```

    ##                 prior     class         coef group resp dpar nlpar bound
    ## 1                             b                                         
    ## 2                             b  MindReading                            
    ## 3                             b VoiceHearing                            
    ## 4 student_t(3, 0, 10) Intercept                                         
    ## 5 student_t(3, 0, 10)     sigma

``` r
priorVoiceMind <- c(
  prior(normal(0,.3), class = b),
  prior(normal(1,2), class = sigma),
  prior(normal(0,1), class = Intercept))

# Test prior
VoiceMind_PriorCheck_m <- brm(
  formula = VoiceMind_f,
  data = scz,
  family = gaussian,
  prior = priorVoiceMind,
  sample_prior = "only",
  file = "AltercentricVoiceMind_PC"
)
plot_1.4 <- pp_check(VoiceMind_PriorCheck_m, nsamples = 100) 

# Fitting the model
VoiceMind_m <- brm(
  formula = VoiceMind_f,
  data = scz,
  family = gaussian,
  prior = priorVoiceMind,
  sample_prior = T,
  file = "AltercentricVoiceMind"
)
VoiceMind_m
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + VoiceHearing + MindReading 
    ##    Data: scz (Number of observations: 75) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept        0.12      0.21    -0.30     0.51 1.00     3053     2651
    ## VoiceHearing     0.11      0.14    -0.17     0.39 1.00     3356     3078
    ## MindReading      0.10      0.11    -0.12     0.33 1.00     3682     3195
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.90      0.08     0.77     1.07 1.00     4119     2908
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
plot_2.4 <- pp_check(VoiceMind_m, nsamples = 100)

plot_1.4 + plot_2.4
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
# Hypothesis testing + updating check
plot(hypothesis(VoiceMind_m,
           "VoiceHearing > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-5-2.png)

``` r
hypothesis(VoiceMind_m,
           "VoiceHearing > 0")
```

    ## Hypothesis Tests for class b:
    ##           Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (VoiceHearing) > 0     0.11      0.14    -0.13     0.34       3.61      0.78
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
plot(hypothesis(VoiceMind_m,
           "MindReading > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-5-3.png)

``` r
hypothesis(VoiceMind_m,
           "MindReading > 0")
```

    ## Hypothesis Tests for class b:
    ##          Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (MindReading) > 0      0.1      0.11    -0.08     0.29       4.21      0.81
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
plot(conditional_effects(VoiceMind_m), points=T)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-5-4.png)![](Assignment3_files/figure-markdown_github/unnamed-chunk-5-5.png)

Voice hearing, mind reading and apathy
======================================

``` r
# Define formula
VoiceMindAp_f <- bf(
  AltercentricIntrusion ~ 1 + VoiceHearing + MindReading + Apathy
)

# Design prior
get_prior(VoiceMindAp_f, family = gaussian, scz)
```

    ##                 prior     class         coef group resp dpar nlpar bound
    ## 1                             b                                         
    ## 2                             b       Apathy                            
    ## 3                             b  MindReading                            
    ## 4                             b VoiceHearing                            
    ## 5 student_t(3, 0, 10) Intercept                                         
    ## 6 student_t(3, 0, 10)     sigma

``` r
priorVoiceMindAp <- c(
  prior(normal(0,.3), class = b),
  prior(normal(1,2), class = sigma),
  prior(normal(0,1), class = Intercept))

# Test prior
VoiceMindAp_PriorCheck_m <- brm(
  formula = VoiceMindAp_f,
  data = scz,
  family = gaussian,
  prior = priorVoiceMindAp,
  sample_prior = "only", # we sample the prior we test want to test them now, and not the model
  file = "AltercentricVoiceMindAp_PC"
)
plot_1.4 <- pp_check(VoiceMindAp_PriorCheck_m, nsamples = 100) 

# Fitting the model
VoiceMindAp_m <- brm(
  formula = VoiceMindAp_f,
  data = scz,
  family = gaussian,
  prior = priorVoiceMindAp,
  sample_prior = T,
  file = "AltercentricVoiceMindAp"
)
VoiceMindAp_m
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + VoiceHearing + MindReading + Apathy 
    ##    Data: scz (Number of observations: 75) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept        0.37      0.30    -0.22     0.95 1.00     3377     2695
    ## VoiceHearing     0.06      0.15    -0.23     0.35 1.00     3925     3141
    ## MindReading      0.04      0.12    -0.20     0.28 1.00     3981     3154
    ## Apathy          -0.17      0.14    -0.45     0.12 1.00     3490     3070
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.90      0.07     0.77     1.06 1.00     4070     2838
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
plot_2.4 <- pp_check(VoiceMindAp_m, nsamples = 100)

plot_1.4 + plot_2.4
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
# Hypothesis testing + updating check
plot(hypothesis(VoiceMindAp_m,
           "VoiceHearing > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-6-2.png)

``` r
hypothesis(VoiceMindAp_m,
           "VoiceHearing > 0")
```

    ## Hypothesis Tests for class b:
    ##           Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (VoiceHearing) > 0     0.06      0.15    -0.18     0.31       1.92      0.66
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
plot(hypothesis(VoiceMindAp_m,
           "MindReading > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-6-3.png)

``` r
hypothesis(VoiceMindAp_m,
           "MindReading > 0")
```

    ## Hypothesis Tests for class b:
    ##          Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (MindReading) > 0     0.04      0.12    -0.16     0.24       1.62      0.62
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
plot(hypothesis(VoiceMindAp_m,
           "Apathy < 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-6-4.png)

``` r
hypothesis(VoiceMindAp_m,
           "Apathy < 0")
```

    ## Hypothesis Tests for class b:
    ##     Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
    ## 1 (Apathy) < 0    -0.17      0.14     -0.4     0.07       7.64      0.88     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
plot(conditional_effects(VoiceMindAp_m), points=T)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-6-5.png)![](Assignment3_files/figure-markdown_github/unnamed-chunk-6-6.png)![](Assignment3_files/figure-markdown_github/unnamed-chunk-6-7.png)

Model comparison
================

``` r
VoiceHearing_m <- add_criterion(VoiceHearing_m, criterion = "loo")
```

    ## Automatically saving the model object in 'AltercentricVoiceHearing.rds'

``` r
MindReading_m <- add_criterion(MindReading_m, criterion = "loo")
```

    ## Automatically saving the model object in 'AltercentricMindReading.rds'

``` r
Apathy_m <- add_criterion(Apathy_m, criterion = "loo")
```

    ## Automatically saving the model object in 'AltercentricApathy.rds'

``` r
VoiceMind_m <- add_criterion(VoiceMind_m, criterion = "loo")
```

    ## Automatically saving the model object in 'AltercentricVoiceMind.rds'

``` r
VoiceMindAp_m <- add_criterion(VoiceMindAp_m, criterion = "loo")
```

    ## Automatically saving the model object in 'AltercentricVoiceMindAp.rds'

``` r
loo_compare(VoiceHearing_m,
            MindReading_m,
            Apathy_m,
            VoiceMind_m,
            VoiceMindAp_m)
```

    ##                elpd_diff se_diff
    ## Apathy_m        0.0       0.0   
    ## MindReading_m  -1.0       1.3   
    ## VoiceMindAp_m  -1.3       0.4   
    ## VoiceMind_m    -1.5       1.2   
    ## VoiceHearing_m -1.5       1.5

``` r
# model with apathy seems to be best

# if we assume that these are the only models possible models and one of them is true, what's the probability that any of them is true?
loo_model_weights(VoiceHearing_m,
            MindReading_m,
            Apathy_m,
            VoiceMind_m,
            VoiceMindAp_m)
```

    ## Method: stacking
    ## ------
    ##                weight
    ## VoiceHearing_m 0.000 
    ## MindReading_m  0.000 
    ## Apathy_m       1.000 
    ## VoiceMind_m    0.000 
    ## VoiceMindAp_m  0.000

``` r
# apathy is the right one (it is also possible that we don't ahve the true model, bc here we just assume that)
```

R’s answer: given our domain knowledge (blabla), we expect VH and MR to
be related to AI, but not A. Models predicting AU from single symptoms
do not support these H. VH, MR, A numbers A model comparison approach
indicates that the model predicting AI from apathy is the best model
minimizing estimated out of sample error (stacking weight of 1). Adding
other symptoms to the model A does not omprove generalizability of the
model (stacking weights of 0). The results do not support our H, and
would require a rethinking of the theoretical assumptions.

The model does not indicate a credible difference in altercentric
intrusion in the models, thus, it does not support our hypothesis (b =
0.36, CIs = 0.16, 0.57, ER = 1332). Controls showed on average an
altercentric intrusion effect of 3.86 (CIs 3.74, 3.98), and
schizophrenia of 4.22 (CIs = 4.01, 4.43). \[Add plot of the effects\]

SI (in methods part)
====================

The model had no divergences, a Rhat of 1, and Effective Sample Sizes
above 2000 for both Bulk and Tail. \[Add prior and posterior checks
plots; add updating check plot\]

Second part
-----------

Q2.1) However, we know that the diagnosis is based on symptom
assessment: if the overall sum of symptoms is severe enough, the
participant gets a diagnosis. In other words, by selecting the patients,
and including the symptoms in the model we might have inadvertently
introduced an issue in our inference. Do try to draw a causal graph
(Directed Acyclical Graph) of the variables and compare it with the
types of causal graphs presented in the slides. Discuss which biases you
might have introduced. \# if apathy + voice hearing + mind reading is
high enough, patient gets a diagnosis

Q2.2.) Redesign your analysis following the graph and report how the
results change \# what should we do to avoid issues we infer from the
graph? follow up video coming on this

### Voice Hearing

``` r
# Designing the priors
get_prior(VoiceHearing_f, family = gaussian, d)
```

    ##                 prior     class         coef group resp dpar nlpar bound
    ## 1                             b                                         
    ## 2                             b VoiceHearing                            
    ## 3 student_t(3, 0, 10) Intercept                                         
    ## 4 student_t(3, 0, 10)     sigma

``` r
# Test prior
VoiceHearing_PriorCheck_m_all <- brm(
  formula = VoiceHearing_f,
  data = d,
  family = gaussian,
  prior = priorVoiceHearing,
  sample_prior = "only", # we sample the prior we test want to test them now, and not the model
  file = "AltercentricVoiceHearing_PC_all"
)
pp_check(VoiceHearing_PriorCheck_m_all, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
# Fitting the model
VoiceHearing_m_all <- brm(
  formula = VoiceHearing_f,
  data = d,
  family = gaussian,
  prior = priorVoiceHearing_all,
  sample_prior = T,
  file = "AltercentricVoiceHearing_all"
)
pp_check(VoiceHearing_m_all, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-8-2.png)

``` r
VoiceHearing_m_all 
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + VoiceHearing 
    ##    Data: d (Number of observations: 300) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept       -0.00      0.06    -0.11     0.11 1.00     4248     3097
    ## VoiceHearing     0.19      0.06     0.08     0.30 1.00     4009     2832
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.99      0.04     0.91     1.07 1.00     4074     2907
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# Hypothesis testing + updating check
plot(hypothesis(VoiceHearing_m_all,
           "VoiceHearing > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-8-3.png)

``` r
# model has learned, post is more confident than prior

hypothesis(VoiceHearing_m_all,
           "VoiceHearing > 0") # b = 0.19, 95% CIs = 0.1, 0.28, ER = Inf(?)
```

    ## Hypothesis Tests for class b:
    ##           Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (VoiceHearing) > 0     0.19      0.06      0.1     0.28        Inf         1
    ##   Star
    ## 1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
# Plot with error bars
conditional_effects(VoiceHearing_m_all)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-8-4.png)

``` r
plot(conditional_effects(VoiceHearing_m_all), points=T)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-8-5.png)

### Mind Reading

``` r
# Design prior
get_prior(MindReading_f, family = gaussian, d)
```

    ##                 prior     class        coef group resp dpar nlpar bound
    ## 1                             b                                        
    ## 2                             b MindReading                            
    ## 3 student_t(3, 0, 10) Intercept                                        
    ## 4 student_t(3, 0, 10)     sigma

``` r
# Test prior
MindReading_PriorCheck_m_all <- brm(
  formula = MindReading_f,
  data = d,
  family = gaussian,
  prior = priorMindReading,
  sample_prior = "only",
  file = "AltercentricMindReading_PC_all"
)
pp_check(MindReading_PriorCheck_m_all, nsamples = 100) 
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
# Fitting he model
MindReading_m_all <- brm(
  formula = MindReading_f,
  data = d,
  family = gaussian,
  prior = priorMindReading,
  sample_prior = T,
  file = "AltercentricMindReading_all"
)
MindReading_m_all
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + MindReading 
    ##    Data: d (Number of observations: 300) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept       0.00      0.06    -0.11     0.12 1.00     4250     2872
    ## MindReading     0.19      0.06     0.08     0.30 1.00     3802     3297
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.99      0.04     0.91     1.07 1.00     3981     3122
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
pp_check(MindReading_m_all, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-9-2.png)

``` r
# Hypothesis testing + updating check
plot(hypothesis(MindReading_m_all,
           "MindReading > 0")) 
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-9-3.png)

``` r
hypothesis(MindReading_m_all,
           "MindReading > 0") # b = 0.19, 95% CIs = 0.9, 0.28, ER = 3999
```

    ## Hypothesis Tests for class b:
    ##          Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (MindReading) > 0     0.19      0.06     0.09     0.28       3999         1
    ##   Star
    ## 1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
## Plot with error bars
conditional_effects(VoiceHearing_m_all)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-9-4.png)

``` r
plot(conditional_effects(VoiceHearing_m_all), points=T)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-9-5.png)

### Apathy

``` r
# Design prior
get_prior(Apathy_f, family = gaussian, d)
```

    ##                 prior     class   coef group resp dpar nlpar bound
    ## 1                             b                                   
    ## 2                             b Apathy                            
    ## 3 student_t(3, 0, 10) Intercept                                   
    ## 4 student_t(3, 0, 10)     sigma

``` r
# Test prior
Apathy_PriorCheck_m_all <- brm(
  formula = Apathy_f,
  data = d,
  family = gaussian,
  prior = priorApathy,
  sample_prior = "only", # we sample the prior we test want to test them now, and not the model
  file = "AltercentricApathy_PC_all"
)
pp_check(Apathy_PriorCheck_m_all, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
# Fitting the model
Apathy_m_all <- brm(
  formula = Apathy_f,
  data = d,
  family = gaussian,
  prior = priorApathy,
  sample_prior = T,
  file = "AltercentricApathy_all"
)
Apathy_m_all
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + Apathy 
    ##    Data: d (Number of observations: 300) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     0.00      0.06    -0.11     0.12 1.00     4197     2855
    ## Apathy        0.08      0.06    -0.02     0.19 1.00     3652     2916
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     1.00      0.04     0.93     1.09 1.00     3575     2969
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
pp_check(Apathy_m_all, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-10-2.png)

``` r
# Hypothesis testing + updating check
plot(hypothesis(Apathy_m_all,
           "Apathy < 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-10-3.png)

``` r
hypothesis(Apathy_m_all,
           "Apathy < 0") # b = 0.08, 95% CIs = -0.01, 0.18, ER = 0.07
```

    ## Hypothesis Tests for class b:
    ##     Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
    ## 1 (Apathy) < 0     0.08      0.06    -0.01     0.18       0.07      0.07     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
hypothesis(Apathy_m_all,
           "Apathy = 0") # b = 0.08, 95% CIs = -0.02, 0.19, ER = 1.85
```

    ## Hypothesis Tests for class b:
    ##     Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
    ## 1 (Apathy) = 0     0.08      0.06    -0.02     0.19       1.85      0.65     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
hypothesis(Apathy_m_all,
           "Apathy > 0") # b = 0.08, 95% CIs = -0.01, 0.18, ER = 13.81
```

    ## Hypothesis Tests for class b:
    ##     Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
    ## 1 (Apathy) > 0     0.08      0.06    -0.01     0.18      13.81      0.93     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
plot(conditional_effects(Apathy_m_all), points=T)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-10-4.png)

### Voice Hearing and Mind Reading

``` r
# Design prior
get_prior(VoiceMind_f, family = gaussian, d)
```

    ##                 prior     class         coef group resp dpar nlpar bound
    ## 1                             b                                         
    ## 2                             b  MindReading                            
    ## 3                             b VoiceHearing                            
    ## 4 student_t(3, 0, 10) Intercept                                         
    ## 5 student_t(3, 0, 10)     sigma

``` r
priorVoiceMind <- c(
  prior(normal(0,.3), class = b),
  prior(normal(1,2), class = sigma),
  prior(normal(0,1), class = Intercept))

# Test prior
VoiceMind_PriorCheck_m_all <- brm(
  formula = VoiceMind_f,
  data = d,
  family = gaussian,
  prior = priorVoiceMind,
  sample_prior = "only",
  file = "AltercentricVoiceMind_PC_all"
)
pp_check(VoiceMind_PriorCheck_m_all, nsamples = 100) 
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
# Fitting the model
VoiceMind_m_all <- brm(
  formula = VoiceMind_f,
  data = d,
  family = gaussian,
  prior = priorVoiceMind,
  sample_prior = T,
  file = "AltercentricVoiceMind_all"
)
VoiceMind_m_all
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + VoiceHearing + MindReading 
    ##    Data: d (Number of observations: 300) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept        0.00      0.06    -0.11     0.11 1.00     4136     2982
    ## VoiceHearing     0.16      0.06     0.05     0.28 1.00     4636     3005
    ## MindReading      0.16      0.06     0.05     0.27 1.00     4796     3418
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.97      0.04     0.90     1.06 1.00     4500     2937
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
pp_check(VoiceMind_m_all, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-11-2.png)

``` r
# Hypothesis testing + updating check
plot(hypothesis(VoiceMind_m_all,
           "VoiceHearing > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-11-3.png)

``` r
hypothesis(VoiceMind_m_all,
           "VoiceHearing > 0")
```

    ## Hypothesis Tests for class b:
    ##           Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (VoiceHearing) > 0     0.16      0.06     0.07     0.26     362.64         1
    ##   Star
    ## 1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
plot(hypothesis(VoiceMind_m_all,
           "MindReading > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-11-4.png)

``` r
hypothesis(VoiceMind_m_all,
           "MindReading > 0")
```

    ## Hypothesis Tests for class b:
    ##          Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (MindReading) > 0     0.16      0.06     0.07     0.25     570.43         1
    ##   Star
    ## 1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

Voice hearing, mind reading and apathy
======================================

``` r
# Design prior
get_prior(VoiceMindAp_f, family = gaussian, d)
```

    ##                 prior     class         coef group resp dpar nlpar bound
    ## 1                             b                                         
    ## 2                             b       Apathy                            
    ## 3                             b  MindReading                            
    ## 4                             b VoiceHearing                            
    ## 5 student_t(3, 0, 10) Intercept                                         
    ## 6 student_t(3, 0, 10)     sigma

``` r
# Test prior
VoiceMindAp_PriorCheck_m_all <- brm(
  formula = VoiceMindAp_f,
  data = d,
  family = gaussian,
  prior = priorVoiceMindAp,
  sample_prior = "only", # we sample the prior we test want to test them now, and not the model
  file = "AltercentricVoiceMindAp_PC"
)
plot_1.4 <- pp_check(VoiceMindAp_PriorCheck_m_all, nsamples = 100) 

# Fitting the model
VoiceMindAp_m_all <- brm(
  formula = VoiceMindAp_f,
  data = d,
  family = gaussian,
  prior = priorVoiceMindAp,
  sample_prior = T,
  file = "AltercentricVoiceMindAp_all"
)
VoiceMindAp_m_all
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + VoiceHearing + MindReading + Apathy 
    ##    Data: d (Number of observations: 300) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept        0.00      0.06    -0.11     0.11 1.00     4048     3116
    ## VoiceHearing     0.16      0.06     0.05     0.27 1.00     4065     3305
    ## MindReading      0.16      0.06     0.05     0.27 1.00     4203     3203
    ## Apathy           0.02      0.06    -0.10     0.13 1.00     4000     3275
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.98      0.04     0.90     1.06 1.00     4217     3140
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
plot_2.4 <- pp_check(VoiceMindAp_m_all, nsamples = 100)

plot_1.4 + plot_2.4
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
# Hypothesis testing + updating check
plot(hypothesis(VoiceMindAp_m_all,
           "VoiceHearing > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-12-2.png)

``` r
hypothesis(VoiceMindAp_m_all,
           "VoiceHearing > 0")
```

    ## Hypothesis Tests for class b:
    ##           Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (VoiceHearing) > 0     0.16      0.06     0.07     0.26     443.44         1
    ##   Star
    ## 1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
plot(hypothesis(VoiceMindAp_m_all,
           "MindReading > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-12-3.png)

``` r
hypothesis(VoiceMindAp_m_all,
           "MindReading > 0")
```

    ## Hypothesis Tests for class b:
    ##          Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (MindReading) > 0     0.16      0.06     0.07     0.26     306.69         1
    ##   Star
    ## 1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
plot(hypothesis(VoiceMindAp_m_all,
           "Apathy < 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-12-4.png)

``` r
hypothesis(VoiceMindAp_m_all,
           "Apathy < 0")
```

    ## Hypothesis Tests for class b:
    ##     Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
    ## 1 (Apathy) < 0     0.02      0.06    -0.08     0.11       0.63      0.39     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
plot(conditional_effects(VoiceMindAp_m_all), points=T)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-12-5.png)![](Assignment3_files/figure-markdown_github/unnamed-chunk-12-6.png)![](Assignment3_files/figure-markdown_github/unnamed-chunk-12-7.png)

Model comparison
================

``` r
VoiceHearing_m_all <- add_criterion(VoiceHearing_m_all, criterion = "loo")
```

    ## Automatically saving the model object in 'AltercentricVoiceHearing_all.rds'

``` r
MindReading_m_all <- add_criterion(MindReading_m_all, criterion = "loo")
```

    ## Automatically saving the model object in 'AltercentricMindReading_all.rds'

``` r
Apathy_m_all <- add_criterion(Apathy_m_all, criterion = "loo")
```

    ## Automatically saving the model object in 'AltercentricApathy_all.rds'

``` r
VoiceMind_m_all <- add_criterion(VoiceMind_m_all, criterion = "loo")
```

    ## Automatically saving the model object in 'AltercentricVoiceMind_all.rds'

``` r
VoiceMindAp_m_all <- add_criterion(VoiceMindAp_m_all, criterion = "loo")
```

    ## Automatically saving the model object in 'AltercentricVoiceMindAp_all.rds'

``` r
loo_compare(VoiceHearing_m_all,
            MindReading_m_all,
            Apathy_m_all,
            VoiceMind_m_all,
            VoiceMindAp_m_all)
```

    ## Warning: Not all models have the same y variable. ('yhash' attributes do not
    ## match)

    ##                    elpd_diff se_diff
    ## VoiceMind_m_all     0.0       0.0   
    ## VoiceMindAp_m_all  -1.0       0.3   
    ## MindReading_m_all  -3.4       3.0   
    ## VoiceHearing_m_all -3.5       2.9   
    ## Apathy_m_all       -8.0       4.3

``` r
# model VoiceMind seems to be best

loo_model_weights(VoiceHearing_m_all,
            MindReading_m_all,
            Apathy_m_all,
            VoiceMind_m_all,
            VoiceMindAp_m_all)
```

    ## Method: stacking
    ## ------
    ##                    weight
    ## VoiceHearing_m_all 0.088 
    ## MindReading_m_all  0.130 
    ## Apathy_m_all       0.000 
    ## VoiceMind_m_all    0.782 
    ## VoiceMindAp_m_all  0.000

``` r
# now VoiceMind is the most probable to be the true model: 0.782
```

Third part - more theoretical
-----------------------------

These issues are very difficult to think through, and not knowing the
causal mechanisms generating the data in advance makes our inferences
even more unreliable. To explore these issues, I recommend using
simulations. In other words, defining a “true” model, generating data
from it and assessing what different analyses would lead you to infer
(and therefore which biases they might introduce). You can find the code
I used to simulate your data below.

Q3.1) Look through the code and identify whether the results you have
match the underlying truth. Discuss what you have learned. \# hypotheses
for the underlying mechanisms \# betas matches the direction of effect
tha R put in simulation?

Q3.2) OPTIONAL: is this a general pattern? Try varying the parameters
(e.g. correlation values) and assess whether the new dataset(s) leads to
the same biases in your analysis.

Simulated data (Riccardo)
=========================

``` r
## DO NOT RUN THIS, DATA IS DOWNLOADED
pacman::p_load(MASS, tidyverse, psych)

seed <- 1981 # Defining a seed so the results are always the same
n <- 300 # Defining the amount of participants

SymptomCorr <- .2 # Defining the correlation of symptoms (as they tend to co-occur)
EffectCorrRel <- .2 # Defining the correlation between relevant symptoms and effect (Some symptoms are positively correlated with the effect)
EffectCorrIrrel <- 0 # Defining the correlation between irrelevant symptoms and effect (none)

# Creating the variance-covariance matrix for the variables we want to generate (3 symptoms, 1 effect)
Sigma <- matrix(data=c(1,SymptomCorr,SymptomCorr,EffectCorrRel,
                       SymptomCorr,1,SymptomCorr,EffectCorrRel,
                       SymptomCorr,SymptomCorr,1,EffectCorrIrrel,
                       EffectCorrRel,EffectCorrRel,EffectCorrIrrel,1),
                       nrow=4,ncol=4)

## Generate data from a multivariate (mvr) normal (n) distribution
d0 <- mvrnorm(n = n, # number of participant
        mu = c(1.2, 1.2, 1.2, 4), # mean of each variable
        Sigma) # variance co-variance matrix

# Giving meaningful names to variables and add ID
d0 <- data.frame(
  VoiceHearing = d0[,1], 
  MindReading =  d0[,2],
  Apathy =  d0[,3], 
  AltercentricIntrusion = d0[,4],
  ID = seq(nrow(d0)))

# Assessing whether the participant has schizophrenia (high enough sum of symptoms)
# Here we choose participants scoring above 75% percentile (the most severe ones)
d0$Diagnosis <- 0
d0$Diagnosis[(d0$VoiceHearing + d0$MindReading + d0$Apathy) > 
              quantile(d0$VoiceHearing + d0$MindReading + d0$Apathy, .75)] <-1

## Plotting the relation between variables in schizophrenia
d1 <- d0 %>% subset(Diagnosis==1) %>% dplyr::select(-Diagnosis, -ID)
pairs.panels(d1)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
# if selecting only scz, we start to get weird results, we are not going to find a true relation if we only select people with scz diagnosis

## Plotting the relation between variables all participants
pairs.panels(dplyr::select(d0,-Diagnosis, -ID))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-14-2.png)

``` r
write_csv(d,path="C:/Users/molna/Desktop/AU/4th_semester/CompModeling4CogSci/Assignments(except1)/Assignment3-4th-sem/Ass3_sim.csv")
```
