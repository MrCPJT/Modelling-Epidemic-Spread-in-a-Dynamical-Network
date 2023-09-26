# Update

# Create a `list.of.updaters`
list.of.updaters <- list(
  # this is one updater
  list( # Mask Mandate + Lockdown
    at = 100,
    param = list(
      inf.prob = 0.03,
      act.rate = 4.8
    )
  ),
  # this is another updater
  list( # Lockdown over
    at = 280,
    param = list(
      act.rate = 9.6
    )
  ),
  # this is another updater
  list( # Lockdown 2
    at = 340,
    param = list(
      act.rate = 6.2,
      inf.prob = 0.4
    )
  ),
  # this is another updater
  list( # Christmas 2020
    at = 365,
    param = list(
      act.rate = 45
    )
  ),
  # this is another updater
  list( # Christmas 2020
    at = 380,
    param = list(
      act.rate = 15
    )
  ),
  # this is another updater
  list( # Lockdown 2 Over
    at = 400,
    param = list(
      act.rate = 9.6,
      inf.prob = 0.5
    )
  ),
  # this is another updater
  list( # Vaccine
    at = 440,
    param = list(
      inf.prob = 0.03
    )
  ),
  # this is another updater
  list( # Vaccine 2
    at = 560,
    param = list(
      inf.prob = 0.01
    )
  ),
  # this is another updater
  list( # Delta
    at = 660,
    param = list(
      inf.prob = 0.04
    )
  ),
  # this is another updater
  list( # Christmas 2021
    at = 730,
    param = list(
      act.rate = 100
    )
  ),
  # this is another updater
  list( # christmas cooldown
    at = 745,
    param = list(
      act.rate = 20
    )
  ),
  # this is another updater
  list( # Omicron
    at = 760,
    param = list(
      inf.prob = 0.13
    )
  ),
  # this is another updater
  list( # Booster
    at = 770,
    param = list(
      inf.prob = 0.1
    )
  ),
  # this is another updater
  list( # All restrictions scrapped
    at = 820,
    param = list(
      inf.prob = 0.1,
      act.rate = 15
    )
  )
)

# Implementation of Masks: Act rate: 9.6 -> 4.8
# Self-Isolating: Mean Degree 1.5 -> 1
# Lock-down: Lower Mean Degree
# Vaccination: Infection probability: 0.05 -> 0.03 -> 0.01
# Variants: Delta: Infection probability +0.03 ::: Omicron: Infection probability: + 0.9



