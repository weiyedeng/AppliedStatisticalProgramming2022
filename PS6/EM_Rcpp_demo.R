library(Rcpp)

cppFunction('List get_num_den(NumericVector y, NumericVector ftk, double sd, NumericVector weights) {
  int n = y.size();
  int m = ftk.size();
  NumericVector v = NumericVector(n*m, NumericVector::get_na());
  NumericMatrix num_mat(n, m, v.begin());
  NumericVector den_vec(0);
  
  for (int i=0; i<n; i++){
    NumericVector num_vec(0);
  
    for (int j=0; j<m; j++) {
      double num = R::dnorm(y[i], ftk[j], sd, FALSE) * weights[j];
      num_vec.push_back(num);
    }
    num_mat(i,_) = num_vec;
    double den = sum(num_vec);
    den_vec.push_back(den);
  }
  
  List L=List::create(Named("numerator_matrix")=num_mat, Named("denominator_vector")=den_vec);
  return L;
}')


cppFunction('NumericMatrix est_ztk(NumericVector y, NumericVector ftk, double sd, NumericVector weights) {
  int n = y.size();
  int m = ftk.size();
  NumericVector v = NumericVector(n*m, NumericVector::get_na());
  NumericMatrix ztk_mat(n, m, v.begin());
  Function get_num_den("get_num_den");
  
  List L = get_num_den(y, ftk, sd, weights);
  NumericMatrix num_mat = L[0];
  NumericVector den_vec = L[1];
  
  for (int i=0; i<n; i++){
    ztk_mat(i,_) = num_mat(i,_) / den_vec[i];
  }
  return ztk_mat;
}')

cppFunction('NumericVector update_weights(NumericMatrix ztk_mat) {
  NumericVector new_weights = colMeans(ztk_mat);
  return new_weights;
}')


cppFunction('List est_finalWeights(NumericVector y, NumericVector ftk, double sd, NumericVector weights, double threshold) {
  Function get_num_den("get_num_den");
  Function est_ztk("est_ztk");   
  Function update_weights("update_weights");
  
  List L = get_num_den(y, ftk, sd, weights);
  NumericVector den_vec = L[1];
  
  NumericVector new_weights = update_weights(est_ztk(y, ftk, sd, weights));
  List new_L = get_num_den(y, ftk, sd, new_weights);
  NumericVector new_den_vec = new_L[1];
  
  NumericVector diff_den_vec = abs(new_den_vec - den_vec);
  NumericVector threshold_vec = NumericVector(diff_den_vec.size(), threshold);
  double iter = 1;
  
  while(is_true(any(diff_den_vec >= threshold_vec))) { // if the diff is larger than the threshold
    den_vec = new_den_vec; // replace the den_vec from last iteration to the current iteration
    new_weights = update_weights(est_ztk(y, ftk, sd, new_weights)); // similarly, replace the new_weights
    new_L = get_num_den(y, ftk, sd, new_weights); // similarly, replace the new_L by inputting the new_weights
    new_den_vec = new_L[1]; // Extract the next iteration of den_vec
    diff_den_vec = abs(new_den_vec - den_vec); // Extract the new diff_den_vec
    iter++;
  }
  
  List out_L = List::create(Named("final_weights")=new_weights, 
                            Named("final_density")=new_den_vec,
                            Named("iterations")=iter);
  return out_L;
}')

y <- c(0.1,0.2,0.5)
ftk <- seq(0,1,0.1)
sd <- 1
weights <- rep(1/length(ftk), length(ftk))

get_num_den(y=y, ftk=ftk, weights=weights, sd=1)
est_ztk(y=y, ftk=ftk, weights=weights, sd=1)

new_weights <- update_weights(est_ztk(y=y, ftk=ftk, sd=1, weights=weights))
new_weights
get_num_den(y=y, ftk=ftk, sd=1, weights=new_weights)

sum(new_weights)
est_finalWeights(y=y, ftk=ftk, weights=weights, sd=1, threshold=0.00001)
