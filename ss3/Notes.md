## Notes for SS3 runs

-   Use version 3.30.20 optimized executable
-   Sex-specific natural mortality predicted from max age
-   Fecundity (pup production) is linear with length `F = 0.214 * L - 14.7`, estimated from dogfish in Puget Sound. See equation 6 in <https://doi.org/10.1139/F08-211>
-   Currently using Beverton-Holt stock-recruit relationship but Ian Taylor has developed a dogfish specific SRR that is built into ss3 that we can use. See <https://doi.org/10.1016/j.fishres.2012.04.018>

## Outstanding issues

-   Standardizing the length distribution from the four surveys
-   Potential fleets as area approach to account for stock and sex distribution between north and south

### Model 1

-   First attempt at building an outside dogfish model. QH March 7, 2023
-   No convergence yet, need to continue tinkering with the selectivity options and downweight the length comps
