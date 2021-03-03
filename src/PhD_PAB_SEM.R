# libraries --------------------------------------------------------------------
library(piecewiseSEM)

# sample data from parametric distributions ------------------------------------

set.seed(1234)

sample <- 60
id <- 1:sample

## invasibility ----------------------------------------------------------------

# unified metric of invasibility (sensu Guo et al. 2015. Ecology)
invasion <- rbeta(sample, shape1 = 2, shape2 = 2)

## propagule pressure ----------------------------------------------------------

# seed bank germination (sensu Eschtruth and Battles. 2011. Ecology)
seed_bank <- rpois(sample, lambda = 100)

# seed rain index (sensu Eschtruth and Battles. 2011. Ecology)
seed_rain <- rbeta(sample, shape1 = 4, shape2 = 4)

# distance to roads or parks 
human_act <- rpois(sample, lambda = 20)

## disturbance -----------------------------------------------------------------

# mowing frequency
disturbance <- rbinom(sample, size = 1, prob = 0.5)

# photosynthetic active radiation
light <- rpois(sample, lambda = 100)

# volumetric water content
soil <- rbeta(sample, shape1 = 4, shape2 = 5)

## plant diversity -------------------------------------------------------------

# biotic resistance: community-based index (functional richness)
func_rich <- rgamma(sample, shape = 4)

# limiting similarity: species-based index (MNDS)
trait_func <- rbeta(sample, shape1 = 1, shape2 = 3)

# make a data frame ------------------------------------------------------------

df <- data.frame(
  id,
  invasion, 
  seed_bank, 
  seed_rain,
  human_act, 
  disturbance,
  light,
  soil, 
  func_rich, 
  trait_func
)

# local estimation -------------------------------------------------------------

sem <- psem(
  
  glm(
    seed_bank ~ human_act + seed_rain,
    family = poisson(link = "log")
    ),
  
  glm(
    seed_rain ~ human_act + disturbance, 
    family = quasibinomial(link = "logit")
    ),
  
  glm(
    invasion ~ 
      seed_rain + 
      seed_bank +
      func_rich + 
      trait_func +
      light +
      soil, 
    family = quasibinomial(link = "logit")
    ),
  
  lm(
    light ~ disturbance
  ),
  
glm(
  soil ~ disturbance
  ),

  data = df
)

# summary ----------------------------------------------------------------------

summary(sem, conserve = TRUE)
