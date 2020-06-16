#### C++ function to estimate true diversity ####

cppFunction('double Dcpp(std::vector<unsigned> v, NumericMatrix b) {
// [[Rcpp::plugins("cpp11")]]         
  unsigned cnt;
    unsigned n = b.nrow();
    for (size_t i=0; i<n; i++) {
      cnt = std::count(v.begin(), v.end(), b(i,0));
      if (cnt > 0) {
        std::replace (v.begin(), v.end(), b(i,0), b(i,1));
        for (size_t j=0; j<cnt; j++) {
          v.push_back(b(i,2));
        }
      }  
    }

    n = v.size();
    unsigned m = n;
    std::map<unsigned,unsigned> counts;
    for (size_t i = 0; i < n; i++) {
    //if (std::isnan(v[i])) {
      if (v[i] == 0) {
          m--; 
      } else {
        counts[v[i]]++;
      }
    }
    if (m == 0) { return std::numeric_limits<double>::quiet_NaN();}
    
    double H = 0;
    double d = m;
    for(auto const& imap: counts) {
      double p = imap.second / d ;
      H += p * std::log(p);
    }
    // return  -1 * H;
    return  std::exp(-1 * H);
}')

