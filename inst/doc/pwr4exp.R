## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(pwr4exp)

## -----------------------------------------------------------------------------
crd <- designCRD(
  treatments = 4,
  replicates = 8,
  beta = c(35, -5, 2, 3),
  sigma2 = 15
)

## -----------------------------------------------------------------------------
pwr.anova(design = crd)

## -----------------------------------------------------------------------------
pwr.contrast(design = crd, specs =  ~ trt, method = "trt.vs.ctrl")

## -----------------------------------------------------------------------------
pwr.contrast(design = crd, specs =  ~ trt, method = "poly")

## -----------------------------------------------------------------------------
rcbd <- designRCBD(
  treatments = c(2, 2),
  blocks = 8,
  beta = c(35, 5, 3, -2),
  VarCov = 11,
  sigma2 = 4
)

## -----------------------------------------------------------------------------
pwr.anova(design = rcbd)

## -----------------------------------------------------------------------------
# across all levels of facB
pwr.contrast(design = rcbd, specs = ~ "facA", method = "pairwise")
# at each level of facB
pwr.contrast(design = rcbd, specs = ~ facA|facB, method = "pairwise")

## -----------------------------------------------------------------------------
rcbd_quote <- quote(
  designRCBD(
  treatments = c(2, 2),
  blocks = n,
  beta = c(35, 5, 3, -2),
  VarCov = 11,
  sigma2 = 4
  )
)

## -----------------------------------------------------------------------------
find_sample_size(design.quote = rcbd_quote, n_init = 2, n_max = 99)

## -----------------------------------------------------------------------------
lsd <- designLSD(
  treatments = c(2, 2),
  label = list(temp = c("T1", "T2"), dosage = c("D1", "D2")),
  squares = 4,
  reuse = "both",
  beta = c(35, 5, 3, -2),
  VarCov = list(11, 2),
  sigma2 = 2
)

## -----------------------------------------------------------------------------
pwr.anova(design = lsd)

## -----------------------------------------------------------------------------
# the effect of dosage across all levels of temp
pwr.contrast(design = lsd, specs = ~ "dosage", method = "pairwise")
# the effect of dosage at each level of temp
pwr.contrast(design = lsd, specs = ~ dosage|temp, method = "pairwise")

## -----------------------------------------------------------------------------
lsd_quote <- quote(
  designLSD(
  treatments = c(2, 2),
  squares = n,
  reuse = "both",
  beta = c(35, 5, 3, -2),
  VarCov = list(11, 2),
  sigma2 = 2
  )
)

## -----------------------------------------------------------------------------
find_sample_size(design.quote = lsd_quote, n_init = 2, n_max = 99)

## -----------------------------------------------------------------------------
spd <- designSPD(
  trt.main = 2,
  trt.sub = 3, 
  replicates = 10, 
  label = list(Main = c("Main1", "Main2"), Sub = c("Sub1", "Sub2", "Sub3")),
  beta = c(20, 2, 2, 4, 0, 2),
  VarCov = list(4),
  sigma2 = 11
)

## -----------------------------------------------------------------------------
pwr.anova(spd)

## -----------------------------------------------------------------------------
pwr.contrast(design = spd, specs = ~ Sub|Main, method = "trt.vs.ctrl")

## -----------------------------------------------------------------------------
df_spd_cod <- pwr4exp:::df.cod(
  treatments = c(2, 2),
  squares = 4
)
## Create main plot factor, i.e., breed
df_spd_cod$Breed <- rep(c("1", "2"), each = 32)

## Check data structure
head(df_spd_cod, n = 4); tail(df_spd_cod, n = 4)

## -----------------------------------------------------------------------------
formula <- y ~ Breed*facA*facB + (1|subject) + (1|period)

## -----------------------------------------------------------------------------
beta = c(
  `(Intercept)` = 35,        # Baseline (mean of Breed1_A1_B1)
  Breed2 = -5,           # Effect of the second breed alone
  facA2 = -5,               # Effect of A2 alone
  facB2 = 1,                # Effect of B2 alone
  `Breed2:facA2` = 1,         # Interaction between Breed2 and A2
  `Breed2:facB2` = 0,         # Interaction between Breed2 and B2
  `facA2:facB2` = 2,             # Interaction between A2 and B2
  `Breed2:facA2:facB2` = 1       # Three-way interaction between Breed2, A2, and B2
)


## -----------------------------------------------------------------------------
SPD_COD <- designCustom(
  design.df = df_spd_cod,
  formula = formula,
  beta = beta,
  VarCov = list(7, 4),
  sigma2 = 4,
  design.name = "hybrid SPD COD"
)

## -----------------------------------------------------------------------------
pwr.anova(SPD_COD)

## -----------------------------------------------------------------------------
pwr.contrast(SPD_COD, ~facA|facB|Breed, "pairwise")

