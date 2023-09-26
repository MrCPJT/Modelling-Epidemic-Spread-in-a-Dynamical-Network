# Update script

# Implementation of Masks: Act rate: 9.6 -> 4.8
# Self-Isolating: Mean Degree 1.5 -> 1
# Lock-down: Lower Mean Degree
# Vaccination: Infection probability: 0.05 -> 0.03 -> 0.01
# Variants: Delta: Infection probability +0.03 ::: Omicron: Infection probability: + 0.9

# Create a `list.of.updaters`
list.of.updaters <- list(
  
  # Mask Mandate + First Lockdown
  list( 
    at = 100,
    param = list(
      inf.prob = 0.03,
      act.rate = 4.8
    )
  ),
  
  # First Lockdown Over
  list( 
    at = 280,
    param = list(
      act.rate = 9.6
    )
  ),
  
  # Second Lockdown
  list( 
    at = 340,
    param = list(
      act.rate = 6.2,
      inf.prob = 0.4
    )
  ),
  
  # Christmas 2020
  list( 
    at = 365,
    param = list(
      act.rate = 45
    )
  ),
  
  # End of Christmas 2020
  list( 
    at = 380,
    param = list(
      act.rate = 15
    )
  ),
  
  # Second Lockdown Over
  list( 
    at = 400,
    param = list(
      act.rate = 9.6,
      inf.prob = 0.5
    )
  ),
  
  # First Vaccine Dose
  list( 
    at = 440,
    param = list(
      inf.prob = 0.03
    )
  ),
  
  # Second Vaccine Dose
  list( 
    at = 560,
    param = list(
      inf.prob = 0.01
    )
  ),
  
  # Delta Variant
  list( 
    at = 660,
    param = list(
      inf.prob = 0.04
    )
  ),
  
  # Christmas 2021
  list( 
    at = 730,
    param = list(
      act.rate = 100
    )
  ),
  
  # End of Christmas 2021
  list( 
    at = 745,
    param = list(
      act.rate = 20
    )
  ),
  
  # Omicron Variant
  list( 
    at = 760,
    param = list(
      inf.prob = 0.13
    )
  ),
  
  # Fist Booster Jab
  list( 
    at = 770,
    param = list(
      inf.prob = 0.1
    )
  ),
  
  # Restrictions Lifted
  list( 
    at = 820,
    param = list(
      inf.prob = 0.1,
      act.rate = 15
    )
  )
)
