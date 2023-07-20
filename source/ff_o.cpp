#include <Rcpp.h>
using namespace Rcpp;
#include <iostream>
using namespace std;

// turmite function to be called from R
// [[Rcpp::export]]
NumericMatrix flame(int iter, int layers) {
  
  NumericMatrix points(iter, 3); // initially zero
  NumericMatrix coeffs(9, layers);
  
  // set coefficients
  for(int i = 0; i < 9; i++) {
    for(int j = 0; j < layers; j++) {
      coeffs(i,j) = R::runif(-1,1);
    }
  }
  
  // initial values
  points(0, 0) = R::runif(-1, 1);
  points(0, 1) = R::runif(-1, 1);
  points(0, 2) = R::runif(-1, 1);
  
  // iterate
  int r;
  int f;
  double x;
  double y;
  double z;
  double s;
  double u;

  const double pi = 3.1415926535;
  
  for(int t = 1; t < iter; t++) {
    
    r = rand() % layers; // which affine transform to use?
    f = rand() % 2;      // which variant function to use?
    
    // co-ordinates after random transform
    x = coeffs(0, r) * points(t-1, 0) + coeffs(1, r) * points(t-1, 1) + coeffs(2, r);
    y = coeffs(3, r) * points(t-1, 0) + coeffs(4, r) * points(t-1, 1) + coeffs(5, r);
    z = coeffs(6, r) * points(t-1, 0) + coeffs(7, r) * points(t-1, 1) + coeffs(8, r);
  
    if(f == 0) {
      s = tan(y / x);
      u = cos(z);
      x = u * (16 * pow(sin(s),3)) / 17;
      y = u * (13 * cos(s) - 5 * cos(2 * s) - 2 * cos(3 * s) - 
        cos(4 * s))/17;
      z = s * u;
    } else if(f == 1) {
      x = x * cos(x);
      y = y * sin(y);
      z = z * sin(z);
    } else {
      s = pow(x*x + y*y + z*z, 1/3);
      x = x/s;
      y = y/s;
      z = z/s;
    }
    
    // store results
    points(t, 0) = x;
    points(t, 1) = y;
    points(t, 2) = (z + points(t-1, 2))/2;
    
  }
  
  return points;
}


