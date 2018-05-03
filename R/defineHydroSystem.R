defineHydrosystem <- function(){

# defineHydroSystem.R

# HYDRO SYSTEM CHARACTERISTICS --------------------------------------------
# This needs to be done inside the outer loop in case it is redefined by draws
# from a distribution for sensitivity analysis
#
# ^^ JMS NOTE: The above advice is ignored in the original source, thus I move these
# definitions here!
#
# For the Penobscot, there are two potential migration routes, so this needs
# to be accommodated. FOR ALL OF THE HABITAT VARIABLES AND THE PASSAGE ROUTE
# VARIABLES, GROUP 1 CORRESPONDS TO THE PISCATAQUIS ROUTE AND GROUP 2 TO THE
# UPPER MAINSTEM PENOBSCOT GROUP
nRoutes <- 4

# Need separate habitat values for each migration route
# Number of dams- In the Penobscot River we are treating the confluence of the
# Stillwater Branch and the Penobscot River as a dam because it has the
# Potential to cause delay to fish that must migrate up the mainstem of the
# river. The passage rate at that dam will be derived based on delay.
# Group 1: Mainstem to Piscataquis
# Group 2: Mainstem to mainstem
# Group 3: Stillwater to Piscataquis
# Group 4: Stillwater to mainstem
shad$nDams <- c(6, 4, 7 , 5)

# Define number of production units- dams plus two in this system
shad$nPU <- shad$nDams + 1

# Define maximum rkm for the system. In the Piscataquis, Guilford Dam is fine,
# and for the mainstem use Mattaceunk
shad$maxrkm <- c(182, 165, 182, 165)

# Define rkms for each of the dams for each migration group
shad$damRkms <- list(
c(40, 52, 60, 99, 165, 166, 179),     # Dam rkms for group 1
c(40, 52, 60, 100, 150),              # Dam rkms for group 2
c(40, 52, 56, 62, 99, 165, 166, 179), # Dam rkms for group 3
c(40, 52, 56, 62, 100, 150)          # Dam rkms for group 4
)

}
