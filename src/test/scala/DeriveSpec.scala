package edu.ucsb.cs.cs162.regex.derivative

import org.scalatest._
import edu.ucsb.cs.cs162.regex._

class DeriveSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  //Regexes I will test

  val b: Regex = Chars('b')
  val c: Regex = Chars('c')
  val d: Regex = Chars('d')
  val e: Regex = Chars('e')
  val kleene: Regex = Chars('b', 'c')
  val emp: Regex = ε
  val charSet1 = Chars('a', 'b', 'c', 'd')
  val charSet2 = Chars('b', 'c')

  //DerivativeMachine Instances 
  val derMach1 = new DerivativeMachine(b)
  val derMach2 = new DerivativeMachine(∅)
  val derMach4 = new DerivativeMachine(emp)

  //KLEENESTAR TESTS 
  val derMach3 = new DerivativeMachine(b.*)
  val derMach9 = new DerivativeMachine(charSet1.*)

  //COMPLEMENT TESTS
  val derMach10 = new DerivativeMachine(!(b ~ c))
  val derMach11 = new DerivativeMachine(!emp)

  //UNION TESTS 
  val derMach15 = new DerivativeMachine((ε | b)) //empty in lang
  val derMach5 = new DerivativeMachine(b | c)
  val derMach12 = new DerivativeMachine(Chars('a') | b.*)
  
  //INTESECTION TESTS
  val derMach6 = new DerivativeMachine(charSet1 & charSet2) //Intersection
  val derMach7 = new DerivativeMachine(b ~ c) //Concat
  val derMach8 = new DerivativeMachine(emp ~ emp)
  val derMach14 = new DerivativeMachine(b ~ emp)

  //BUILDER METHOD TESTS
  val derMach17 = new DerivativeMachine((e.+)^3)
  val derMach18 = new DerivativeMachine((e)<=5)
  val derMach19 = new DerivativeMachine(charSet2 <>(2,5))
  val derMach20 = new DerivativeMachine(charSet1 >= 5)

  //COMPLEX TESTS
  val derMach13 = new DerivativeMachine(!(!(e | (charSet2 ~ charSet2)))) //Complement of complement
  val derMach16 = new DerivativeMachine(d ~ (((b.*) | !(c.*)) & (b.*)))
  val derMach21 = new DerivativeMachine(((charSet2)^3) & (b~b~b))


  


  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "matches"

  it should "recognize strings in the language 1" in { 
    // (Chars('a'->'d') & Chars('c'->'f')) should equal (Chars('c'->'d'))
    // (α.* & r) should equal(r)
    derMach1.eval("b") should equal (true)
    
  }

  it should "recognize KleeneStar of a regex 2" in { 
    derMach3.eval("b") should equal (true) 
    derMach3.eval("bb") should equal (true) //FAILING eval??
    derMach3.eval("bbbbbbbbbbbbb") should equal (true)
    derMach9.eval("cb") should equal (true)
    derMach9.eval("bbbbb") should equal (true)
  }

  it should "recognize Union of a regex 3" in { 
    derMach5.eval("b") should equal (true)
    derMach5.eval("c") should equal (true)
    derMach12.eval("b") should equal (true)
    derMach12.eval("a") should equal (true)
    derMach12.eval("bbbbb") should equal (true)
    derMach15.eval("b") should equal (true)
     derMach15.eval("") should equal (true)

  }  

  it should "recognize Intersection of a regex 4" in { 
    derMach6.eval("b") should equal (true)
    derMach6.eval("c") should equal (true)
  }  

  it should "recognize the concatenation of a regex 5" in { 
    derMach8.eval("") should equal (true)
    derMach7.derive('b') should equal (c) 
    derMach1.derive('b') should equal (ε)
    derMach14.eval("b") should equal (true)
    //derMach7.eval("bc") should equal (true) //FAILING EVAL??
    derMach7.eval("bc") should equal (true)
  } 

  it should "recognize the complement of a regex 6" in { 
    derMach10.eval("b") should equal (true)
    derMach10.eval("bbbbb") should equal (true)
    derMach10.eval("bcbcbcbcbbcbcbcbbbbb") should equal (true)
    derMach11.eval("b") should equal (true)
    derMach11.eval("himynameisomer") should equal (true)
  }

  it should "recognize a complex regex 7" in {  
    derMach16.eval("db") should equal (true) 
    derMach16.eval("db") should equal (true) 
    derMach16.eval("dbbbbbb") should equal (true) 
  
  }

  //OTHER BUILDER METHOD TESTS
  it should "recognize other builder method regex 8" in {  
     derMach17.eval("eee") should equal (true)
     derMach17.eval("eeeeeeeeeee") should equal (true)
     derMach18.eval("ee") should equal (true)
     derMach18.eval("") should equal (true)
     derMach19.eval("bb") should equal (true)
     derMach19.eval("bbb") should equal (true)
     derMach20.eval("ccccc") should equal (true) //if I get greater than 5 
  }


  // more tests...

  it should "not recognize strings not in the language 1" in {  
    derMach2.eval("b") should equal (false) //empty language
    derMach2.eval("testing 123") should equal (false)
  }

  it should "not recognize the empty set 2" in {  
    derMach4.eval("b") should equal (false) //empty set 
    derMach4.eval("ε") should equal (false) //empty set 
  
  }

  it should "not recognize a KleeneStar regex set 3" in {  
    derMach3.eval("a") should equal (false) //empty set 
    //derMach4.eval("ε") should equal (false) //empty set 
  }

  it should "not recognize a Union regex set 4" in {  
    derMach5.eval("z") should equal (false) //empty set 
    derMach12.eval("asdfasdfasdfasdf") should equal (false)
    //derMach4.eval("ε") should equal (false) //empty set 
  }

  it should "not recognize a complement regex 5" in {  
    derMach10.eval("bc") should equal (false)

  }

  it should "not recognize a concatenation regex 6" in {  
    derMach7.eval("ee") should equal (false)
    derMach13.eval("x") should equal (false)

  }

  it should "not recognize an Intersection regex 6" in {  
    derMach6.eval("a") should equal (false)
    derMach6.eval("d") should equal (false)   

  }

  it should "not recognize a complex regex 7" in {  
    derMach16.eval("z") should equal (false) 
    derMach16.eval("dbdb") should equal (false)  //THis will check letters that are members of the same language but not of the regex
    derMach16.eval("c") should equal (false)
  }

  it should "not recognize other builder method regex 8" in {  
     derMach17.eval("e") should equal (false)
     derMach17.eval("ee") should equal (false)
     derMach18.eval("eeeeeeeeeeeeeeeeeeeee") should equal (false)
     derMach19.eval("") should equal (false)
     derMach19.eval("b") should equal (false)
     derMach19.eval("cccccccc") should equal (false)
     derMach20.eval("ccc") should equal (false) //if I get less than 5 
     derMach20.eval("zzzzzzz") should equal (false) //if >= 5 of a letter not in language
  
  }







  // more tests...
}
