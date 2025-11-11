## ----include = FALSE, message=FALSE-------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(pwr4exp)

## -----------------------------------------------------------------------------
 df1 <- expand.grid(
   fA = factor(1:2), # factor A with 2 levels
   fB = factor(1:2), # factor B with 2 levels
   fC = factor(1:3), # factor C with 3 levels
   fD = factor(1:3), # factor D with 3 levels
   subject = factor(1:10)  # 10 subjects
 )
 df1$x <- rnorm(nrow(df1))  # Numerical variable x
 df1$z <- rnorm(nrow(df1))  # Numerical variable z

## -----------------------------------------------------------------------------
mkdesign( ~ fA * fB + x, df1)$fixeff$beta

## -----------------------------------------------------------------------------
mkdesign(~ fA + fB, df1, template = TRUE)$fixeff$means

## -----------------------------------------------------------------------------
mkdesign(~ fA * fB * fC, df1)$fixeff$means

## -----------------------------------------------------------------------------
mkdesign(~ x + z, df1)$fixeff$means

## -----------------------------------------------------------------------------
mkdesign(~ x * z, df1)$fixeff$means

## -----------------------------------------------------------------------------
mkdesign(~ fA * x, df1)$fixeff$means

## -----------------------------------------------------------------------------
mkdesign(~ fA * fB * fC + fD * x + z, df1)$fixeff$means

## -----------------------------------------------------------------------------
mkdesign(~ fA * fB * fC * fD + (1 + fA | subject), df1)$varcov

## -----------------------------------------------------------------------------
crd <- designCRD(
  treatments = 4,
  replicates = 8,
  means = c(35, 30, 37, 38),
  sigma2 = 15
)

## -----------------------------------------------------------------------------
pwr.anova(crd)

## -----------------------------------------------------------------------------
pwr.contrast(crd, which =  "trt", contrast = "pairwise")

## -----------------------------------------------------------------------------
pwr.contrast(crd, which =  "trt", contrast = "poly")

## -----------------------------------------------------------------------------
pwr.contrast(crd, which =  "trt", contrast = "trt.vs.ctrl")

## -----------------------------------------------------------------------------
pwr.contrast(crd, which =  "trt", contrast = list(trts.vs.ctrl = c(-1, 1/3, 1/3, 1/3)))

## -----------------------------------------------------------------------------
pwr.contrast(crd, which =  "trt", contrast = "pairwise", sig.level = 0.01)

## -----------------------------------------------------------------------------
pwr.contrast(crd, which =  "trt", contrast = "pairwise", sig.level = 0.05, p.adj = TRUE)

## -----------------------------------------------------------------------------
designRCBD(treatments = c(2, 2), blocks = 8, template = TRUE)

## -----------------------------------------------------------------------------
rcbd <- designRCBD(
  treatments = c(2, 2),
  blocks = 8,
  # beta = c(35, 5, 3, -2), # identical to means
  means = c(35, 40, 38, 41),
  vcomp = 11,
  sigma2 = 4
)

## -----------------------------------------------------------------------------
unique(rcbd$deStruct$fxTrms$fixedfr)
rcbd$deStruct$formula

## -----------------------------------------------------------------------------
designRCBD(treatments = c(2, 2), 
           label = list(factorA = c("A1", "A2"), factorB = c("B1", "B2")), 
           blocks = 8, 
           formula = ~ factorA + factorB + (1|block), 
           template = TRUE)

## -----------------------------------------------------------------------------
pwr.anova(rcbd)

## -----------------------------------------------------------------------------
pwr.contrast(rcbd, which = "facA", by = "facB")

## -----------------------------------------------------------------------------
designLSD(
  treatments = c(2, 2),
  squares = 4,
  reuse = "none",
  template = TRUE
)

## -----------------------------------------------------------------------------
lsd <- designLSD(
  treatments = c(2, 2),
  label = list(temp = c("T1", "T2"), dosage = c("D1", "D2")),
  squares = 4,
  reuse = "none",
  means = c(35, 40, 38, 41),
  vcomp = c(11, 2),
  sigma2 = 2
)

## -----------------------------------------------------------------------------
designSPD(
  trt.main = 2,
  trt.sub = 3, 
  replicates = 10, 
  template = TRUE
)

## -----------------------------------------------------------------------------
spd <- designSPD(
  trt.main = 2,
  trt.sub = 3, 
  replicates = 10, 
  means = c(20, 22, 22, 24, 24, 28),
  vcomp = 4,
  sigma2 = 11
)

## -----------------------------------------------------------------------------
n_subject = 6 # Subjects per treatment
n_trt = 3 # Number of treatments
n_hour = 8 # Number of repeated measures (time points)
trt = c("CON", "TRT1", "TRT2")

df.rep <- data.frame(
  subject = as.factor(rep(seq_len(n_trt*n_subject), each = n_hour)),
  hour = as.factor(rep(seq_len(n_hour), n_subject*n_trt)),
  trt = rep(trt, each = n_subject*n_hour)
)

## -----------------------------------------------------------------------------
mkdesign(formula = ~ trt*hour, data = df.rep)

## -----------------------------------------------------------------------------
design.rep <- mkdesign(
formula = ~ trt*hour,
data = df.rep,
means =  c(1, 2.50, 3.5,
           1, 3.50, 4.54,
           1, 3.98, 5.80,
           1, 4.03, 5.4,
           1, 3.68, 5.49,
           1, 3.35, 4.71,
           1, 3.02, 4.08,
           1, 2.94, 3.78),
sigma2 = 2,
correlation = corAR1(value = 0.6, form = ~ hour|subject)
)

## -----------------------------------------------------------------------------
pwr.anova(design.rep)

## -----------------------------------------------------------------------------
pwr.contrast(design.rep, which = "trt", by = "hour", contrast = "trt.vs.ctrl", p.adj = TRUE)[1:2]

