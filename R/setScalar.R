#setScalar()

# Function to set population scalar
setScalar <- function(){
    if (sum(spawningPool) < 1e3) {
      scalar = 10
    }
    if (sum(spawningPool) >= 1e3) {
      scalar = 100
    }
    if (sum(spawningPool) >= 1e4) {
      scalar = 1000
    }
    if (sum(spawningPool) >= 1e5) {
      scalar = 10000
    }
    if (sum(spawningPool) >= 1e6) {
      scalar = 100000
    }
 
return(list(scalar=scalar))
  
}  