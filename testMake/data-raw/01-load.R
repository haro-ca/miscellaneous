# ---------------------------------- #
# Layout del código:
# └── Datos necesarios para el script ===
# ├ Sección ===
# │ ├─ Sub 1 ---
# │ │ ├─ Sub 2 ---
# └ Sección final
# ---------------------------------- #

# └── nycflights13::flights ===
# ├ Librerías ====
library(tidyverse)
library(here)

# ├ Lectura ====
flights = nycflights13::flights
flights = flights %>% add_column(col_nueva = 'columna_nueva', otra_col = 'col')

# └ Escritura ====
fst::write_fst(flights, here('data', 'flights.fst'))



