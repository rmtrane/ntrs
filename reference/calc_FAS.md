# Calcaulate Functional Assessment Score summary

Calculates the Functional Assessment Score summary (FAS) from the ten
scores of

- bills

- taxes

- shopping

- games

- stove

- meal prep

- events

- paying attention

- remembering appointments

- travel

Each score is on the scale 0 (Normal), 1 (Has difficulty, but does by
self), 2 (Requires assistance), 3 (dependent). Further, error codes of 8
(Not applicable, e.g. never did) and 9 (Unknown) are also allowed, while
-4 indicates data not collected. The FAS is calculated as the sum of the
ten scores where any 8s or 9s are counted as 0.

## Usage

``` r
calc_FAS(
  BILLS,
  TAXES,
  SHOPPING,
  GAMES,
  STOVE,
  MEALPREP,
  EVENTS,
  PAYATTN,
  REMDATES,
  TRAVEL
)
```

## Arguments

- BILLS:

  score for ability to write checks, pay bills, or balancing check book.

- TAXES:

  score for ability to assemble tax records, business affairs, or other
  papers.

- SHOPPING:

  score for shopping alone for clothes, household necessities, or
  groceries.

- GAMES:

  score for playing a game of skill such as bridge or chess, and working
  on a hobby

- STOVE:

  score for ability to heat water, make a cup of coffee, turn off the
  stove

- MEALPREP:

  score for ability to prepare a balanced meal

- EVENTS:

  score for keeping track of current events

- PAYATTN:

  score for ability to pay attention to and understand a TV program, a
  book, or a magazine

- REMDATES:

  score for remembering appointments, family occasions, holidays, and
  medications.

- TRAVEL:

  score for traveling out of the neighborhood, driving, or arranging to
  take public transportation
