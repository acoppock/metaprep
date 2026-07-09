# dplyr methods for estimates_vcov objects

These methods allow standard dplyr operations on estimates_vcov objects
while keeping the variance-covariance matrix synchronized with the data.

Supported operations:

- Row operations that maintain sync:
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
  [`slice()`](https://dplyr.tidyverse.org/reference/slice.html),
  [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)

- Column operations that don't change rows:
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
  [`rename()`](https://dplyr.tidyverse.org/reference/rename.html),
  [`select()`](https://dplyr.tidyverse.org/reference/select.html)

- Grouping operation:
  [`nest_by()`](https://dplyr.tidyverse.org/reference/nest_by.html)
  (returns rowwise tibble with nested estimates_vcov)
