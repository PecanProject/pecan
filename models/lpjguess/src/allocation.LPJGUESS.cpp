#include <Rcpp.h>
using namespace Rcpp;

/************************************************************************/
/************************ NASTY HARD CODING *****************************/
/************************************************************************/


bool ifcdebt = true;


/************************************************************************/
/************************ HELPER FUNCTIONS  *****************************/
/************************************************************************/


inline bool largerthanzero(double dval, int limit = 0) {
  // Returns true if |dval| < EPSILON, otherwise false
  return limit ? dval > pow(10.0, limit) : dval > 1.0e-30;
}





/**
 *  @brief This does what you think it does.
 *  @ingroup sorting_algorithms
 *  @param  __a  A thing of arbitrary type.
 *  @param  __b  Another thing of arbitrary type.
 *  @return   The lesser of the parameters.
 *
 *  This is the simple classic generic implementation.  It will work on
 *  temporary expressions, since they are only evaluated once, unlike a
 *  preprocessor macro.
 */
template<typename _Tp>
_GLIBCXX14_CONSTEXPR
  inline const _Tp&
    min(const _Tp& __a, const _Tp& __b)
    {
      // concept requirements
      __glibcxx_function_requires(_LessThanComparableConcept<_Tp>)
      //return __b < __a ? __b : __a;
      if (__b < __a)
        return __b;
      return __a;
    }

/**
*  @brief This does what you think it does.
*  @ingroup sorting_algorithms
*  @param  __a  A thing of arbitrary type.
*  @param  __b  Another thing of arbitrary type.
*  @return   The greater of the parameters.
*
*  This is the simple classic generic implementation.  It will work on
*  temporary expressions, since they are only evaluated once, unlike a
*  preprocessor macro.
*/
template<typename _Tp>
_GLIBCXX14_CONSTEXPR
  inline const _Tp&
    max(const _Tp& __a, const _Tp& __b)
    {
      // concept requirements
      __glibcxx_function_requires(_LessThanComparableConcept<_Tp>)
      //return  __a < __b ? __b : __a;
      if (__a < __b)
        return __b;
      return __a;
    }

/**
*  @brief This does what you think it does.
*  @ingroup sorting_algorithms
*  @param  __a  A thing of arbitrary type.
*  @param  __b  Another thing of arbitrary type.
*  @param  __comp  A @link comparison_functors comparison functor@endlink.
*  @return   The lesser of the parameters.
*
*  This will work on temporary expressions, since they are only evaluated
*  once, unlike a preprocessor macro.
*/
template<typename _Tp, typename _Compare>
_GLIBCXX14_CONSTEXPR
  inline const _Tp&
    min(const _Tp& __a, const _Tp& __b, _Compare __comp)
    {
      //return __comp(__b, __a) ? __b : __a;
      if (__comp(__b, __a))
        return __b;
      return __a;
    }

/**
 *  @brief This does what you think it does.
 *  @ingroup sorting_algorithms
 *  @param  __a  A thing of arbitrary type.
 *  @param  __b  Another thing of arbitrary type.
 *  @param  __comp  A @link comparison_functors comparison functor@endlink.
 *  @return   The greater of the parameters.
 *
 *  This will work on temporary expressions, since they are only evaluated
 *  once, unlike a preprocessor macro.
 */
template<typename _Tp, typename _Compare>
_GLIBCXX14_CONSTEXPR
  inline const _Tp&
    max(const _Tp& __a, const _Tp& __b, _Compare __comp)
    {
      //return __comp(__a, __b) ? __b : __a;
      if (__comp(__a, __b))
        return __b;
      return __a;
    }

///////////////////////////////////////////////////////////////////////////////////////
// ALLOCATION
// Function allocation is an internal function (do not call directly from framework);
// function allocation_init may be called to distribute initial biomass among tissues
// for a new individual.

// File scope global variables: used by function f below (see function allocation)

static double k1, k2, k3, b;
static double ltor_g;
static double cmass_heart_g;
static double cmass_leaf_g;

inline double f(double& cmass_leaf_inc) {
  
  // Returns value of f(cmass_leaf_inc), given by:
  //
  // f(cmass_leaf_inc) = 0 =
  //   k1 * (b - cmass_leaf_inc - cmass_leaf_inc/ltor + cmass_heart) -
  //   [ (b - cmass_leaf_inc - cmass_leaf_inc/ltor)
  //   / (cmass_leaf + cmass_leaf_inc )*k3 ] ** k2
  //
  // See function allocation (below), Eqn (13)
  
  return k1 * (b - cmass_leaf_inc - cmass_leaf_inc / ltor_g + cmass_heart_g) -
    pow((b - cmass_leaf_inc - cmass_leaf_inc / ltor_g) / (cmass_leaf_g + cmass_leaf_inc) * k3,
        k2);
}


// [[Rcpp::export]]
List allocation(double bminc,double cmass_leaf,double cmass_root,double cmass_sap,
                double cmass_debt,double cmass_heart,double ltor,double height,double sla,
                double wooddens,int lifeform,double k_latosa,double k_allom2,
                double k_allom3,double& cmass_leaf_inc,double& cmass_root_inc,
                double& cmass_sap_inc,
                double& cmass_debt_inc,
                double& cmass_heart_inc,double& litter_leaf_inc,
                double& litter_root_inc,double& exceeds_cmass) {
  
  // DESCRIPTION
  // Calculates changes in C compartment sizes (leaves, roots, sapwood, heartwood)
  // and litter for a plant individual as a result of allocation of biomass increment.
  // Assumed allometric relationships are given in function allometry below.
  
  // INPUT PARAMETERS
  // bminc       = biomass increment this time period on individual basis (kgC)
  // cmass_leaf  = leaf C biomass for last time period on individual basis (kgC)
  // cmass_root  = root C biomass for last time period on individual basis (kgC)
  // cmass_sap   = sapwood C biomass for last time period on individual basis (kgC)
  // cmass_heart = heartwood C biomass for last time period on individual basis (kgC)
  // ltor        = leaf to root mass ratio following allocation
  // height      = individual height (m)
  // sla         = specific leaf area (PFT-specific constant) (m2/kgC)
  // wooddens    = wood density (PFT-specific constant) (kgC/m3)
  // lifeform    = life form class (TREE or GRASS)
  // k_latosa    = ratio of leaf area to sapwood cross-sectional area (PFT-specific
                                                                       //               constant)
  // k_allom2    = constant in allometry equations
  // k_allom3    = constant in allometry equations
  
  // OUTPUT PARAMETERS
  // cmass_leaf_inc  = increment (may be negative) in leaf C biomass following
  //                   allocation (kgC)
  // cmass_root_inc  = increment (may be negative) in root C biomass following
  //                   allocation (kgC)
  // cmass_sap_inc   = increment (may be negative) in sapwood C biomass following
  //                   allocation (kgC)
  // cmass_heart_inc = increment in heartwood C biomass following allocation (kgC)
  // litter_leaf_inc = increment in leaf litter following allocation, on individual
  //                   basis (kgC)
  // litter_root_inc = increment in root litter following allocation, on individual
  //                   basis (kgC)
  // exceeds_cmass      = negative increment that exceeds existing biomass (kgC)
  
  // MATHEMATICAL DERIVATION FOR TREE ALLOCATION
  // Allocation attempts to distribute biomass increment (bminc) among the living
  // tissue compartments, i.e.
  //   (1) bminc = cmass_leaf_inc + cmass_root_inc + cmass_sap_inc
  // while satisfying the allometric relationships (Shinozaki et al. 1964a,b; Waring
                                                    // et al 1982, Huang et al 1992; see also function allometry, below) [** =
                                                                                                                            // raised to the power of]:
    //   (2) (leaf area) = k_latosa * (sapwood xs area)
    //   (3) cmass_leaf = ltor * cmass_root
    //   (4) height = k_allom2 * (stem diameter) ** k_allom3
    // From (1) and (3),
    //   (5) cmass_sap_inc = bminc - cmass_leaf_inc -
      //         (cmass_leaf + cmass_leaf_inc) / ltor + cmass_root
    // Let diam_new and height_new be stem diameter and height following allocation.
    // Then (see allometry),
    //   (6) diam_new = 2 * [ ( cmass_sap + cmass_sap_inc + cmass_heart )
                              //         / wooddens / height_new / PI ]**(1/2)
    // From (4), (6) and (5),
    //   (7) height_new**(1+2/k_allom3) =
      //         k_allom2**(2/k_allom3) * 4 * [cmass_sap + bminc - cmass_leaf_inc
                                               //         - (cmass_leaf + cmass_leaf_inc) / ltor + cmass_root + cmass_heart]
    //         / wooddens / PI
    // Now,
    //   (8) wooddens = cmass_sap / height / (sapwood xs area)
    // From (8) and (2),
    //   (9) wooddens = cmass_sap / height / sla / cmass_leaf * k_latosa
    // From (9) and (1),
    //  (10) wooddens = (cmass_sap + bminc - cmass_leaf_inc -
                           //         (cmass_leaf + cmass_leaf_inc) / ltor + cmass_root)
    //          / height_new / sla / (cmass_leaf + cmass_leaf_inc) * k_latosa
    // From (10),
    //  (11) height_new**(1+2/k_allom3) =
      //         [ (cmass_sap + bminc - cmass_leaf_inc - (cmass_leaf + cmass_leaf_inc)
                    //           / ltor + cmass_root) / wooddens / sla
                   //           / (cmass_leaf + cmass_leaf_inc ) * k_latosa ] ** (1+2/k_allom3)
    //
      // Combining (7) and (11) gives a function of the unknown cmass_leaf_inc:
      //
      //  (12) f(cmass_leaf_inc) = 0 =
        //         k_allom2**(2/k_allom3) * 4/PI * [cmass_sap + bminc - cmass_leaf_inc
                                                    //         - (cmass_leaf + cmass_leaf_inc) / ltor + cmass_root + cmass_heart]
      //         / wooddens -
        //         [ (cmass_sap + bminc - cmass_leaf_inc - (cmass_leaf + cmass_leaf_inc)
                      //           / ltor + cmass_root) / (cmass_leaf + cmass_leaf_inc)
                     //           / wooddens / sla * k_latosa] ** (1+2/k_allom3)
      //
        // Let k1 = k_allom2**(2/k_allom3) * 4/PI / wooddens
        //     k2 = 1+2/k_allom3
        //     k3 = k_latosa / wooddens / sla
        //     b  = cmass_sap + bminc - cmass_leaf/ltor + cmass_root
        //
          // Then,
        //  (13) f(cmass_leaf_inc) = 0 =
          //         k1 * (b - cmass_leaf_inc - cmass_leaf_inc/ltor + cmass_heart) -
          //         [ (b - cmass_leaf_inc - cmass_leaf_inc/ltor)
                       //         / (cmass_leaf + cmass_leaf_inc )*k3 ] ** k2
        //
          // Numerical methods are used to solve Eqn (13) for cmass_leaf_inc
        
        const int NSEG=20; // number of segments (parameter in numerical methods)
        const int JMAX=40; // maximum number of iterations (in numerical methods)
        const double XACC=0.0001; // threshold x-axis precision of allocation solution
        const double YACC=1.0e-10; // threshold y-axis precision of allocation solution
        const double CDEBT_MAXLOAN_DEFICIT=0.8; // maximum loan as a fraction of deficit
        const double CDEBT_MAXLOAN_MASS=0.2; // maximum loan as a fraction of (sapwood-cdebt)
        
        double cmass_leaf_inc_min;
        double cmass_root_inc_min;
        double x1,x2,dx,xmid,fx1,fmid,rtbis,sign;
        int j;
        double cmass_deficit,cmass_loan;
        
        // initialise
        litter_leaf_inc = 0.0;
        litter_root_inc = 0.0;
        exceeds_cmass   = 0.0;
        cmass_leaf_inc  = 0.0;
        cmass_root_inc  = 0.0;
        cmass_sap_inc   = 0.0;
        cmass_heart_inc = 0.0;
        cmass_debt_inc = 0.0;
        
        if (!largerthanzero(ltor, -10)) {
          
          // No leaf production possible - put all biomass into roots
          // (Individual will die next time period)
          
          cmass_leaf_inc = 0.0;
          
          // Make sure we don't end up with negative cmass_root
          if (bminc < -cmass_root) {
          exceeds_cmass = -(cmass_root + bminc);
          cmass_root_inc = -cmass_root;
          }
          else {
          cmass_root_inc=bminc;
          }
          
          if (lifeform==1) {
          cmass_sap_inc=-cmass_sap;
          cmass_heart_inc=-cmass_sap_inc;
          }
        }
          else if (lifeform==1) {
          
          // TREE ALLOCATION
          
          cmass_heart_inc=0.0;
          
          // Calculate minimum leaf increment to maintain current sapwood biomass
          // Given Eqn (2)
          
          if (height>0.0)
          cmass_leaf_inc_min=k_latosa*cmass_sap/(wooddens*height*sla)-cmass_leaf;
          else
          cmass_leaf_inc_min=0.0;
          
          // Calculate minimum root increment to support minimum resulting leaf biomass
          // Eqn (3)
          
          if (height>0.0)
          cmass_root_inc_min=k_latosa*cmass_sap/(wooddens*height*sla*ltor)-
          cmass_root;
          else
          cmass_root_inc_min=0.0;
          
          if (cmass_root_inc_min<0.0) { // some roots would have to be killed
          
          cmass_leaf_inc_min=cmass_root*ltor-cmass_leaf;
          cmass_root_inc_min=0.0;
          }
          
          // BLARP! C debt stuff
          if (ifcdebt) {
          cmass_deficit=cmass_leaf_inc_min+cmass_root_inc_min-bminc;
          if (cmass_deficit>0.0) {
          cmass_loan=max(min(cmass_deficit*CDEBT_MAXLOAN_DEFICIT,
          (cmass_sap-cmass_debt)*CDEBT_MAXLOAN_MASS),0.0);
          bminc+=cmass_loan;
          cmass_debt_inc=cmass_loan;
          }
          else cmass_debt_inc=0.0;
          }
          else cmass_debt_inc=0.0;
          
          if ( (cmass_root_inc_min >= 0.0 && cmass_leaf_inc_min >= 0.0 &&
          cmass_root_inc_min + cmass_leaf_inc_min <= bminc) || bminc<=0.0) {
          
          // Normal allocation (positive increment to all living C compartments)
          
          // Calculation of leaf mass increment (lminc_ind) satisfying Eqn (13)
          // using bisection method (Press et al 1986)
          
          // Set values for global variables for reuse by function f
          
          k1 = pow(k_allom2, 2.0 / k_allom3) * 4.0 / PI / wooddens;
          k2 = 1.0 + 2 / k_allom3;
          k3 = k_latosa / wooddens / sla;
          b = cmass_sap + bminc - cmass_leaf / ltor + cmass_root;
          ltor_g = ltor;
          cmass_leaf_g = cmass_leaf;
          cmass_heart_g = cmass_heart;
          
          x1 = 0.0;
          x2 = (bminc - (cmass_leaf / ltor - cmass_root)) / (1.0 + 1.0 / ltor);
          dx = (x2 - x1) / (double)NSEG;
          
          if (cmass_leaf < 1.0e-10) x1 += dx; // to avoid division by zero
          
          // Evaluate f(x1), i.e. Eqn (13) at cmass_leaf_inc = x1
          
          fx1 = f(x1);
          
          // Find approximate location of leftmost root on the interval
          // (x1,x2).  Subdivide (x1,x2) into nseg equal segments seeking
          // change in sign of f(xmid) relative to f(x1).
          
          fmid = f(x1);
          
          xmid = x1;
          
          while (fmid * fx1 > 0.0 && xmid < x2) {
          
          xmid += dx;
          fmid = f(xmid);
          }
          
          x1 = xmid - dx;
          x2 = xmid;
          
          // Apply bisection to find root on new interval (x1,x2)
          
          if (f(x1) >= 0.0) sign = -1.0;
          else sign = 1.0;
          
          rtbis = x1;
          dx = x2 - x1;
          
          // Bisection loop
          // Search iterates on value of xmid until xmid lies within
          // xacc of the root, i.e. until |xmid-x|<xacc where f(x)=0
          
          fmid = 1.0; // dummy value to guarantee entry into loop
          j = 0; // number of iterations so far
          
          while (dx >= XACC && fabs(fmid) > YACC && j <= JMAX) {
          
          dx *= 0.5;
          xmid = rtbis + dx;
          
          fmid = f(xmid);
          
          if (fmid * sign <= 0.0) rtbis = xmid;
          j++;
          }
          
          // Now rtbis contains numerical solution for cmass_leaf_inc given Eqn (13)
          
          cmass_leaf_inc = rtbis;
          
          // Calculate increments in other compartments
          
          cmass_root_inc = (cmass_leaf_inc + cmass_leaf) / ltor - cmass_root; // Eqn (3)
          cmass_sap_inc = bminc - cmass_leaf_inc - cmass_root_inc; // Eqn (1)
          
          // guess2008 - extra check - abnormal allocation can still happen if ltor is very small
          if ((cmass_root_inc > 50 || cmass_root_inc < -50) && ltor < 0.0001) {
          cmass_leaf_inc = 0.0;
          cmass_root_inc = bminc;
          cmass_sap_inc = -cmass_sap;
          cmass_heart_inc = -cmass_sap_inc;
          }
          
          // Negative sapwood increment larger than existing sapwood or
          // if debt becomes larger than existing woody biomass
          if (cmass_sap < -cmass_sap_inc || cmass_sap + cmass_sap_inc + cmass_heart < cmass_debt + cmass_debt_inc) {
          
          // Abnormal allocation: reduction in some biomass compartment(s) to
          // satisfy allometry
          
          // Attempt to distribute this year's production among leaves and roots only
          // Eqn (3)
          
          cmass_leaf_inc = (bminc - cmass_leaf / ltor + cmass_root) / (1.0 + 1.0 / ltor);
          cmass_root_inc = bminc - cmass_leaf_inc;
          
          // Make sure we don't end up with negative cmass_leaf
          cmass_leaf_inc = max(-cmass_leaf, cmass_leaf_inc);
          
          // Make sure we don't end up with negative cmass_root
          cmass_root_inc = max(-cmass_root, cmass_root_inc);
          
          // If biomass of roots and leafs can't meet biomass decrease then
          // sapwood also needs to decrease
          cmass_sap_inc = bminc - cmass_leaf_inc - cmass_root_inc;
          
          // No sapwood turned into heartwood
          cmass_heart_inc = 0.0;
          
          // Make sure we don't end up with negative cmass_sap
          if (cmass_sap_inc < -cmass_sap) {
            exceeds_cmass = -(cmass_sap + cmass_sap_inc);
            cmass_sap_inc = -cmass_sap;
          }
          
          // Comment: Can happen that biomass decrease is larger than biomass in all compartments.
          // Then bminc is more negative than there is biomass to respire
          }
          }
else {
  
  // Abnormal allocation: reduction in some biomass compartment(s) to
  // satisfy allometry
  
  // Attempt to distribute this year's production among leaves and roots only
  // Eqn (3)
  
  cmass_leaf_inc = (bminc - cmass_leaf / ltor + cmass_root) / (1.0 + 1.0 / ltor);
  
  if (cmass_leaf_inc > 0.0) {
  
  // Positive allocation to leaves
  
  cmass_root_inc = bminc - cmass_leaf_inc; // Eqn (1)
  
  // Add killed roots (if any) to litter
  
  // guess2008 - back to LPJF method in this case
  // if (cmass_root_inc<0.0) litter_root_inc=-cmass_root_inc;
  if (cmass_root_inc < 0.0) {
  cmass_leaf_inc = bminc;
  cmass_root_inc = (cmass_leaf_inc + cmass_leaf) / ltor - cmass_root; // Eqn (3)
  }
  
  }
  else {
  
  // Negative or zero allocation to leaves
  // Eqns (1), (3)
  
  cmass_root_inc = bminc;
  cmass_leaf_inc = (cmass_root + cmass_root_inc) * ltor - cmass_leaf;
  }
  
  // Make sure we don't end up with negative cmass_leaf
  if (cmass_leaf_inc < -cmass_leaf) {
    exceeds_cmass += -(cmass_leaf + cmass_leaf_inc);
    cmass_leaf_inc = -cmass_leaf;
  }
  
  // Make sure we don't end up with negative cmass_root
  if (cmass_root_inc < -cmass_root) {
  exceeds_cmass += -(cmass_root + cmass_root_inc);
  cmass_root_inc = -cmass_root;
  }
  
  // Add killed leaves to litter
  litter_leaf_inc = max(-cmass_leaf_inc, 0.0);
  
  // Add killed roots to litter
  litter_root_inc = max(-cmass_root_inc, 0.0);
  
  // Calculate increase in sapwood mass (which must be negative)
  // Eqn (2)
  cmass_sap_inc = (cmass_leaf_inc + cmass_leaf) * wooddens * height * sla / k_latosa -
  cmass_sap;
  
  // Make sure we don't end up with negative cmass_sap
  if (cmass_sap_inc < -cmass_sap) {
    exceeds_cmass += -(cmass_sap + cmass_sap_inc);
    cmass_sap_inc = -cmass_sap;
  }
  
  // Convert killed sapwood to heartwood
  cmass_heart_inc = -cmass_sap_inc;
}
}
else if (lifeform == 2) {
  
  // GRASS ALLOCATION
  // Allocation attempts to distribute biomass increment (bminc) among leaf
  // and root compartments, i.e.
  //   (14) bminc = cmass_leaf_inc + cmass_root_inc
  // while satisfying Eqn(3)
  
  cmass_leaf_inc = (bminc - cmass_leaf / ltor + cmass_root) / (1.0 + 1.0 / ltor);
  cmass_root_inc = bminc - cmass_leaf_inc;
  
  if (bminc >= 0.0) {
    
    // Positive biomass increment
    
    if (cmass_leaf_inc < 0.0) {
      
      // Positive bminc, but ltor causes negative allocation to leaves,
      // put all of bminc into roots
      
      cmass_root_inc = bminc;
      cmass_leaf_inc = (cmass_root + cmass_root_inc) * ltor - cmass_leaf; // Eqn (3)
    }
    else if (cmass_root_inc < 0.0) {
      
      // Positive bminc, but ltor causes negative allocation to roots,
      // put all of bminc into leaves
      
      cmass_leaf_inc = bminc;
      cmass_root_inc = (cmass_leaf + bminc) / ltor - cmass_root;
    }
    
    // Make sure we don't end up with negative cmass_leaf
    if (cmass_leaf_inc < -cmass_leaf) {
    exceeds_cmass += -(cmass_leaf + cmass_leaf_inc);
    cmass_leaf_inc = -cmass_leaf;
    }
    
    // Make sure we don't end up with negative cmass_root
    if (cmass_root_inc < -cmass_root) {
      exceeds_cmass += -(cmass_root + cmass_root_inc);
      cmass_root_inc = -cmass_root;
    }
    
    // Add killed leaves to litter
    litter_leaf_inc = max(-cmass_leaf_inc, 0.0);
    
    // Add killed roots to litter
    litter_root_inc = max(-cmass_root_inc, 0.0);
  }
  else if (bminc < 0) {
    
    // Abnormal allocation: negative biomass increment
    
    // Negative increment is respiration (neg bminc) or/and increment in other
    // compartments leading to no litter production
    
    if (bminc < -(cmass_leaf + cmass_root)) {
      
      // Biomass decrease is larger than existing biomass
      
      exceeds_cmass = -(bminc + cmass_leaf + cmass_root);
      
      cmass_leaf_inc = -cmass_leaf;
      cmass_root_inc = -cmass_root;
    }
    else if (cmass_root_inc < 0.0) {
      
      // Negative allocation to root
      // Make sure we don't end up with negative cmass_root

				if (cmass_root < -cmass_root_inc) {
					cmass_leaf_inc = bminc + cmass_root;
					cmass_root_inc = -cmass_root;
				}
			}
			else if (cmass_leaf_inc < 0.0) {

				// Negative allocation to leaf
				// Make sure we don't end up with negative cmass_leaf
      
      if (cmass_leaf < -cmass_leaf_inc) {
        cmass_root_inc = bminc + cmass_leaf;
        cmass_leaf_inc = -cmass_leaf;
      }
    }
  }
  }

// Check C budget after allocation

// maximum carbon mismatch
double EPS = 1.0e-12;

assert(fabs(bminc + exceeds_cmass - (cmass_leaf_inc + cmass_root_inc + cmass_sap_inc + cmass_heart_inc + litter_leaf_inc + litter_root_inc)) < EPS);


List ret;
ret["cmass_leaf_inc"] = cmass_leaf_inc;
ret["cmass_root_inc"] = cmass_root_inc;
ret["cmass_sap_inc"] = cmass_sap_inc;
ret["cmass_debt_inc"] = cmass_debt_inc;
ret["cmass_heart_inc"] = cmass_heart_inc;
ret["litter_leaf_inc"] = litter_leaf_inc;
ret["litter_root_inc"] = litter_root_inc;
ret["exceeds_cmass"] = exceeds_cmass;
return ret;

}