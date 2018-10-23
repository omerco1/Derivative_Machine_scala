// This file uses the 'pimp my library' pattern to add builder methods and regex
// operations to Regex.

package edu.ucsb.cs.cs162.regex
//package edu.ucsb.cs.cs162.range_set
//import edu.ucsb.cs.cs162.range_set._

object `package` {
  import Regex._

  // Convenient methods to build regular expressions.
  implicit class RegexBuilder(val re: Regex) extends AnyVal {
    //----------------------------------------------------------------------------
    // Public API.
    //----------------------------------------------------------------------------

    // Concatenate 're' with 'other', simplifying if possible (assumes that 're'
    // and 'other' have already been simplified).
    def ~(other: Regex): Regex = (re, other) match {
      case (re, `∅`) => ∅
      case (`∅`, re) => ∅
      case (re, `ε`) => re
      case (`ε`, other) => other
      case (re, other) => Concatenate(re, other)
      //case (_,_) => Concatenate(re,other)

    }
    // def ~(other: Regex): Regex = { 
    //   //`∅` | `ε` => ∅

    //   //r ~ ∅ => ∅
    //   if (other==`∅`)  
    //     ∅
    //   else if (re == `∅`)
    //     ∅
    //   else if ((other == `ε`) & ((re != `∅`) | (re != `ε`)) ) 
    //     Concatenate(re, ε) 
    //   else if ((re == `ε`) & ((other != `∅`) | (other != `ε`)) )  
    //     Concatenate(ε, re) 
    //   else 
    //     Concatenate(re, other) 
    // }

    // Union 're' with 'other', simplifying if possible (assumes that 're' and
    // 'other' have already been simplified).
    def |(other: Regex): Regex = (re, other) match {

      case (re, `∅`) => re // re | ∅ => r  
      case ( `∅`, other) => other  // ∅ | re => r done 

      
      case (Chars(re_temp), Chars(re2_temp)) => Chars(re_temp ++ re2_temp)//{a} | {b} => {a ∪ b} //???? 

      case (KleeneStar(re), `ε`) => KleeneStar(re)  // r* | ε => r*
      case (`ε`, KleeneStar(re)) => KleeneStar(re) // ε | r* => r* //other??

      // case (re, `ε`) => Union(re, `ε`) // re | empty language
      // case (`ε`, re ) => Union(`ε`, re) // empty language | re 
      
      //case (KleeneStar(Chars(a)), re) => KleeneStar(Chars(a))// {a} | {b} => {a ∪ b} //???? 

      case (KleeneStar(`α`), other) => KleeneStar(α) // α* | r => α*
      case (re, KleeneStar(`α`)) => KleeneStar(α) // r | α* => α*

      case (re, other) => {
        if(re==other) re
        else Union(re, other) 
      } 
      case (_, _) => Union(re, other) // r | r => r
    //   if (other==`∅`) 
    //     ∅
    //   else if (re == `∅`)
    //     ∅
    //   else if ((other == `ε`) & ((re != `∅`) | (re != `ε`)) ) 
    //     Union(re, ε) 
    //   else if ((re == `ε`) & ((other != `∅`) | (other != `ε`)) )  
    //     Union(ε, re) 
    //   else 
    //     Union(re, other) 
    // }
      
    }


      

    // Apply the Kleene star to 're', simplifying if possible (assumes that 're'
    // has already been simplified).
    def * : Regex = re match { 
      case `∅` => ε
      case `ε` => ε
      case KleeneStar(KleeneStar(re)) => KleeneStar(re)
      case KleeneStar(re) => KleeneStar(re) //(r*)* => r*
      case _ => KleeneStar(re)
    }


    // Complement 're', simplifying if possible (assumes that 're' has already
    // been simplified).
    def unary_! : Regex = re match {
      case `∅`  =>  α.*     //KleeneStar(re) //'α'* WORKS
      case Complement(re) => re 
      case `ε` => α.+ //case `ε`  => 'α'+  WORKS
      case _  => Complement(re)
    } 
     
    
    // Intersect 're' with 'other', simplifying if possible (assumes that 're'
    // and 'other' have already been simplified).
    def &(other: Regex): Regex = (re, other) match {
      case (`∅`, other) => ∅ // ∅ & r => ∅
      case (re, `∅`) => ∅  // r & ∅ => ∅
      case (Chars(re_temp), Chars(re2_temp)) => Chars(re_temp & re2_temp)

        //Intersect(Chars(a), Chars(b)) // {a} & {b} => {a ∩ b} ????

      case (KleeneStar(`α`), other) => other // α* & r => r
      case (re, KleeneStar(`α`)) => re // r & α* => r
  
      case (re, other) => {
        if(re==other) re      // r & r => r
        else Intersect(re, other) 
      }
      case (_,_) => Intersect(re, other) //anything else
    }
    // Shorthand for 1 or more repetitions of re regex.
    def + : Regex = Concatenate(re, KleeneStar(re))

    // Shorthand for 0 or 1 instances of re regex.
    def ? : Regex = ε | re

    // Shorthand for exactly 'num' repetitions of re regex.
    def ^(num: Int): Regex = (num) match  {
      //assert not negative FOR ALL OF THESE 
      //At most??
      //foldLeft, ignore current element argument using (_
      //CASE: if num == 0 return lamda
      // assert(num >= 0)  
      //   re } //CHECK IF negative

      // if (num == 0) 
      //   ε
      // else {
      //   for (i<-1 to num)
      //     Concatenate(re, re)
        
      // }
      // (1 to num).foldLeft(0) { (0) => Concatenate(re, re) } 
      // re
      // else 
      // if (num <= 0)
      //   re 
      // else 
      //   Concatenate(re, re).^(num-1) 
      case 0 => ε
      case 1 => re
      case 2 => Concatenate(re, re)
      case _ => { 
        //num -1 
        if (num < 0) { 
          assert(num >= 0)
          re //This should never return... because assert would throw exeption? ASK TA
        } 
        else Concatenate(re^(num-1), re) 
      }
    }

    // Shorthand for at least 'min' repetitions of re regex.
    def >=(min: Int): Regex = { Concatenate(re^min, KleeneStar(re))}
    
    // Shorthand for at most 'max' repetitions of re regex.
    def <=(max: Int): Regex = max match {
      case 0 => ε
      case _ => Union(re<=(max-1), re^(max))
    }

    // Shorthand for at least 'min' but at most 'max' repetitions of re regex.
    def <>(min: Int, max: Int): Regex = { 
      // call both before and intersect them 
      Intersect(re >= min, re <= max) 
    }
  }

  // Add convenient methods to String for building simple regular expressions.
  implicit class StringToRegex(val str: String) extends AnyVal {
    // Builds the concatenation of each character in 'str' in sequence. Example:
    // "abc".concatenate == Chars('a') ~ Chars('b') ~ Chars('c').
    def concatenate: Regex =
      str.foldLeft(ε: Regex)((acc, char) => acc ~ Chars(char))

    // Builds a charset containing each character in 'str'. Example:
    // "abc".charset == Chars('a', 'b', 'c').
    def charset: Regex =
      if (str.isEmpty) ε else Chars(str.toSeq: _*)
  }

  // Operations on regular expressions.
  implicit class RegexOps(val re: Regex) extends AnyVal {
    // Returns ε if 're' is nullable, otherwise returns ∅.
    def nullable: Regex = re match {
      case `ε` | _: KleeneStar => ε
      case `∅` | _: Chars => ∅
      case Concatenate(re1, re2) => re1.nullable ~ re2.nullable
      case Union(re1, re2) => re1.nullable | re2.nullable
      case Complement(re1) => if (re1.nullable == ε) ∅ else ε
      case Intersect(re1, re2) => re1.nullable & re2.nullable
    }
  }
}
