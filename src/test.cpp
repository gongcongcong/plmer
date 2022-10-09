#include <Rcpp.h>
using namespace Rcpp;
#include <algorithm>
//[[Rcpp::export]]
double vectorSum(NumericVector x) {
        return std::accumulate(x.begin(), x.end(), 0.0);
}

int main(){
        NumericVector a = {1,3, 4,5,6,7};
        double b;
        b = vectorSum(a);
        Rcout << b << std::endl;
        return 0;
}



/*** R
vectorSum(c(3,4,6,13,34))
*/
