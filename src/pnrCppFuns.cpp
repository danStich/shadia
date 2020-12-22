#include <Rcpp.h>
using namespace Rcpp;

// This file contains all of the Cpp functions needed for the PNR shad model
// in an effort to reduce command-line entry for cluster submission

//Calculations for predicting temperature from mvnorm
//[[Rcpp::export]]
NumericMatrix tempC(NumericVector days, NumericVector years,
                    NumericMatrix coeffs){

    int n = days.size();             //Iterator based on number of fish
    int m = years.size();            //Iterator for number of routes
    NumericMatrix predTemps(n, m);   //Output container for max rkm of fish

    for(int t=0; t < n; ++t){          //For each day
        for(int i=0; i < m; ++i){      //In each year
          predTemps(t, i) = coeffs(i, 0)
            + (coeffs(i, 1) * days[t])
            + (coeffs(i, 2) * pow(days[t], 2))
            + (coeffs(i, 3) * pow(days[t], 3))
            + (coeffs(i, 4) * pow(days[t], 4))
            + (coeffs(i, 5) * pow(days[t], 5))
            + (coeffs(i, 6) * pow(days[t], 6))
            + (coeffs(i, 7) * pow(days[t], 7))
            + (coeffs(i, 8) * pow(days[t], 8))
            + (coeffs(i, 9) * pow(days[t], 9));
        } //i
    } //t

    //Return the max rkm for each fish in a vector.
    return predTemps;

  } //end


// Delay header file
//[[Rcpp::export]]
List rleC(NumericVector x){

  std::vector<int> lengths;
  std::vector<double> values;

  // Initialise first value
  int i = 0;
  double prev = x[0];
  values.push_back(prev);
  lengths.push_back(1);

  NumericVector::iterator it;
  for(it = x.begin() + 1; it != x.end(); ++it) {
    if (prev == *it) {
      lengths[i]++;
    } else {
      values.push_back(*it);
      lengths.push_back(1);

      i++;
      prev = *it;
    }
  }

  return List::create(
    _["lengths"] = lengths,
    _["values"] = values
  );
}

// Main function declaration for delayC
//[[Rcpp::export]]
Rcpp::NumericMatrix delayC(NumericMatrix x, NumericVector damRkms){

    //Define iterators for total number of fish (n rows in moves matrix) and
    //for total number of dams
    int n = x.nrow();
    int f = damRkms.size();

    //Pre-allocate a matrix to hold delay (in days) at each dam for each fish
    Rcpp::NumericMatrix delay(n, f);

    //Now, for each fish, calculate how long they were delayed at each dam in
    //days.
    for(int i = 0; i < n; i++){
        /* Create a vector of each ith row of the matrix so it can be passed to
         * to the function rleC that is being called from the delayHeaderFile.
         * We need to do this because that function only does run-length
         * calculations on atomic vectors in R.
         */
        Rcpp::NumericVector y = x(i, _);

        /* The rleC function gives a list of outputs. We need to turn these into
         * vectors using the indices within the lists. So, the run length is
         * at the first position in the list (index zero) and the value to which
         * the run length corresponds is stored in the second position in the
         * list (index 1).
         */
        Rcpp::NumericVector lengths = rleC(y)[0];
        Rcpp::NumericVector rkm = rleC(y)[1];

        /* Define an iterator based on the size of the vector holding all day-
         * end rkms for each of the fish. This will be overwritten each time.
         */
        int m = rkm.size();

        /* Now, we have to add 1 to each of the values in rkm. This is because
         * the values in rkm are dam rkms minus one where the fish actually get
         * delayed. We can do this fast in C++ because we don't have overhead.
         */
        for(int k = 0; k < m; k++){
            rkm[k] += 1.0;
        }

        /* Now that we have the correct rkms, we can do a comparison to see if
         * each of the rkms at which fish were located were at dams. If the fish
         * were located below a dam at the end of a given day, then we can
         * calculate the total delay incurred at each dam. If the fish were not
         * located at the base of a dam at the end of the day, then they were
         * either 1) not delayed, or 2) in a free-flowing reach so a zero is
         * assigned to the vector in the index corresponding to that dam
         */

        /* Create a vector to hold the delay fish incurred at each of the day-
         * end rkms they experienced.
         */
        Rcpp::NumericVector dels(f);        //Container for delay (in days)
        Rcpp::NumericVector locs(f);        //Container for dam rkm

        for(int t = 0; t < f; t++){         //At each dam rkm
            for(int j = 0; j < m; j++){     //For each day-end rkm
                if(rkm[j]==damRkms[t]){     //If dam rkm = day-end km
                    dels[t] = lengths[j];   //Then delay is the run length
                } else {                    //Otherwise
                    //dels[t] = 0;          //There is no delay
                }
            }  //j
        } //t

        //Assign the delay for each fish at each dam to the output matrix
        delay(i, _) = dels;

    } //i

    //Return the matrix of delay experienced by each fish (row)
    //at each dam (column) to the R work space.
    return delay;

} //End


//Function for drawing entry date
// [[Rcpp::export]]
NumericMatrix entryC(NumericVector entry_prob,
    NumericMatrix entryDate, int x) {

    int n = entry_prob.size();

    for(int i = 0; i < n; i++){
            entryDate(_, i) = rbinom(x, 5, entry_prob[i]);
    }

    return entryDate;
}



//Motivation penalty
//[[Rcpp::export]]
NumericMatrix motivationPenaltyC(NumericVector eFFs, NumericVector newTU,
    NumericMatrix ppPenalty){

    int n = ppPenalty.nrow();       //Iterator based on number of days
    int m = ppPenalty.ncol();       //Iterator for number of rkm

    NumericMatrix penalty(n, m);    //Output container for adjusted passage

    for(int i=0; i < n; ++i){
        for(int t=0; t < m; ++t){

            penalty(i, t)  = (1-(newTU[i]-min(newTU))/(max(newTU)-min(newTU)))*eFFs[t];

        } // t

    } // i

    //Return the adjusted passage efficiency for each rkm on each day.
    return penalty;

  } //end


//ABM for fish migration
//[[Rcpp::export]]
NumericVector moveC(NumericVector day, NumericVector entryDate,
  IntegerVector dailyMove, NumericVector maxR, NumericMatrix eFFs,
  NumericVector rkm1, NumericMatrix rkm2, NumericVector spawnDate){

    int n = entryDate.size();          //Iterator based on number of fish
    int m = day.size();                //Iterator for day of year
    //int maxR = 182;                  //Maximum river kilometer in system

    for(int t=0; t < m; ++t){          //On each day
        for(int i=0; i < n; ++i){      //For each fish

            int move = 0;              //Initialize movement variable to zero

            for(int j=0; j < dailyMove[i]; ++j){ //For each moveable rkm

                double x = R::rbinom(1, eFFs(t , rkm1[i])); //Draw Bernoulli passage

                /*Conditional check to see if the fish successfully moves
                 *forward given that the fish has not already reached the
                 *the maximum possible rkm and given that the current date is
                 *is not less than the entry date for that fish. Each rkm in the
                 *river has a passage rate associated with it. For free-flowing
                 *reaches, the Bernoulli draw for passage always has p=1, but
                 *for each of the dams, they have their own passage efficiencies
                 *that were stored in damEffs object in R script. The passage
                 *rates for free-flowing and dam reaches are combined in the
                 *R object eFFs, which has 1 entry for each rkm possible (280).
                 */
                if((x > 0) & (rkm1[i] < maxR[i]) & ((day[t] >= entryDate[i])) &
                  ((day[t] < spawnDate[i]))){
                    move = 1; //If passage successful, fish advances by one km
                } else {      //Otherwise
                    move= 0;  //Fish stays where it is
                    break;    //And the loop moves to the next day and next fish
                } //end if-else

                rkm1[i] = rkm1[i] + move; //Update current position of fish

            } //j

            rkm2(i, t) = rkm1[i]; //Matrix with day-end rkms for each fish

        } //i

    } //t

    //Return the day-end rkm matrix back to R workspace. The matrix has fish
    //in rows and the ending rkm on each day in the columns.
    return rkm2;

  } //end


//Calculations for max RKM reached by each fish
//[[Rcpp::export]]
NumericVector maxrkmC(CharacterVector fishAges, NumericVector maxrkm,
    NumericVector upstream_path, NumericVector routes){

    int n = fishAges.size();          //Iterator based on number of fish
    int m = maxrkm.size();            //Iterator for number of routes
    NumericVector maxR(n);            //Output container for max rkm of fish

    for(int t=0; t < n; ++t){          //For each fish
        for(int i=0; i < m; ++i){      //For each route

            if(upstream_path[t]==routes[i]){
                maxR[t] = maxrkm[i];
            } else {         //Otherwise
                continue;    //The loop moves to the next day and next step
            } //end if-else

        } //i
    } //t

    //Return the max rkm for each fish in a vector.
    return maxR;

  } //end


//Extract final PU for each fish
//[[Rcpp::export]]
CharacterVector fishPU(NumericVector puRkm, NumericVector finalRKM,
  CharacterVector puNames){

    int n = puNames.size();   //Iterator for production units
    int m = finalRKM.size();  //Iterator for fish
    CharacterVector pu(m);    //Character vector to hold output

    /*Check to see what production unit each fish is in by comparing its final
     *rkm to the rkms delineating the start of each PU. Then, assign a PU to
     *each of the fish based on where they are.
     */
    for(int i = 0; i < m; i++){
        for(int t = 0; t < n; t++){
            if((finalRKM[i] > puRkm[t]) & (finalRKM[i] <= puRkm[t+1])){
                pu[i] = puNames[t];
            } else {
                continue;
            }
        }
    }
    return pu;
}


//Get spawn date for each fish
//[[Rcpp::export]]
NumericVector spawnDateC(NumericMatrix predTemps, NumericVector spawnTemp,
    NumericVector entryDate){

    int m = spawnTemp.size();    //Iterator for fish
    NumericVector spawnDate(m);  //Character vector to hold output
    int n = predTemps.nrow();

    /*Use the predTemps matrix to determine on which day the temperature
     * threshold for river entry is exceeded for each fish, retain that value
     * and append it to a numeric vector of entry dates for each fish.
     */
    for(int i = 0; i < m; i++){

        for (int t = 0; t < n; t++){

            if((predTemps(t,1) > spawnTemp[i]) & (predTemps(t,0) > entryDate[i])){

                spawnDate[i] = predTemps(t, 0);

                break;

            } else {

                continue;

            }
        }
    }

    return spawnDate;
}


// Draw upstream migration route for each fish
// [[Rcpp::export]]
NumericVector upstreamPathC(NumericMatrix x){

    int n = x.ncol();
    NumericVector y(n);

    for(int i=0; i < x.ncol( ); i++){

        y[i] = sum(x(_, i));

        }

    return y;
}
