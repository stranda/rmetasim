/**

$Modified: astrand $

Copyright (C) 1999 Allan E. Strand

This file is part of Metasim

Instantiates an object that implements a GNU Scientific Library RNG

set_seed() takes a single long and sets the seed.

*/

#ifndef RANDLIB_H
#define RANDLIB_H

/*includes

*/

#include <metasim.h>
#include <random>
#include <random>
#include <vector>

using namespace std;

class RandLib {

  std::vector <double> lp;
  
public:
  RandLib();
  ~RandLib () ;

  void init();

  void FreeDiscreteLookup();
  ///checks to see if the discrete lookup table has been initialized
  ///returns 1 if yes 0 if no
  int CheckDiscreteLookup();

  /// returns a number from 0 to n from a multinomial distribution.  *p points to an array of class proportions.  
  /// n is the size of the class.
  int multinomial(double *p, int n);

  ///This function sets a lookup table for the Gnu Sci. Lib random number generators
  void SetDiscreteLookup(double *p, int ncat);

  ///This function picks a number from the lookup table set in SetDiscreteLookup
  int PickMultinomial();

  ///returns an integer on the interval between 0 and maxval at random from a uniform dist.
  int unirange(int maxval = 1);

  ///returns a value between 0 and 1 from a uniform distribution
  double uniform();

  ///returns a value from a Poisson Distribution with mean equal to mu.
  int poisson(double mu);
  
  ///Takes a xy coordinate and returns a new xy coordinate derived from 
  ///choosing a direction uniformly and a distance based upon a negative exponential 
  ///distribution 
  void negexp_xy(double ix, double iy, double mu, double &newx, double &newy);

  ///Returns the value of the neg exponential with mu for dist.
  double negexp(double dist, double mu);

  ///sets the seed
  void SetSeed(long int sd=0);

  
}; // end RandLib



class ShuffleLib {
private:
    std::mt19937 generator;

public:
    ShuffleLib(unsigned int seed = std::random_device{}()) : generator(seed) {}

    template <typename Iterator>
    void shuffle(Iterator first, Iterator last) {
        std::shuffle(first, last, generator);
    }
};

extern RandLib RandLibObj;

/**
UTILITY INLINE USED IN SEVERAL FUNCTIONS

*/



#endif /*RANDLIB*/



/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; minor-mode: font-lock ***
;;; End: ***
*/
