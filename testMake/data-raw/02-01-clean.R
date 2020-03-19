# ---------------------------------- #
# Layout del código:
# └── Datos necesarios para el script ===
# ├ Sección ===
# │ ├─ Sub 1 ---
# │ │ ├─ Sub 2 ---
# └ Sección final
# ---------------------------------- #

# ├ Librerías ====
library(tidyverse)
library(here)

# ├ Lectura ====
flights = fst::read_fst(here('data', 'flights.fst')) %>% as_tibble()

# ├ Limpieza ====
flights = flights %>%
    select(dep_time, sched_arr_time)

# └ Escritura ====
fst::write_fst(flights, here('data', 'flights_clean.fst'))




