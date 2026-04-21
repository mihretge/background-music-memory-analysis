# Background Music and Memory Analysis

This project studies whether background music affects memory performance in a 24-card memory puzzle experiment.

## Project Overview

The experiment examined whether three music conditions changed memory performance:

- no music
- soft calming music
- loud rock music

Performance was measured using:
- misses (primary endpoint)
- moves (secondary endpoint)

A total of 30 participants completed the puzzle under all three music conditions, resulting in 90 observations.

## Experimental Design

The analysis considered multiple statistical designs:

- Completely Randomized Design (CRD)
- Randomized Complete Block Design (RCBD) using participant as a block
- 3-factor factorial design using music, age, and gender
- Factorial design with blocking

## Methods Used

- one-way ANOVA
- randomized block ANOVA
- factorial ANOVA
- model diagnostics
- Tukey HSD post hoc analysis

## Key Findings

- Background music was not statistically significant for either misses or moves.
- Participant-level differences were important, which supported the use of blocking.
- Age was statistically significant, with older participants showing poorer memory performance on average.
- Gender and interaction terms were not statistically significant.

## Files

- `Data_Collected.xlsx` — collected experimental dataset
- `R_Code.R` — R code for all analyses
- `Project Report.pdf` — final project report

## Skills Demonstrated

- experimental design
- ANOVA
- randomized block design
- factorial analysis
- post hoc testing
- residual diagnostics
- statistical interpretation
