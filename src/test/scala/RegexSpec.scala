package edu.ucsb.cs.cs162.regex

import org.scalatest._

class RegexSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  val b = Chars('b')
  val c = Chars('c')
  val d = Chars('d')

  val r = Chars('a') | Chars('b').+
  val r1 = Chars('x', 'y').* ~ r
  val r2 = Chars('y', 'x').+ ~ r
  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "a regex"

  it should "be buildable using `~`" in {
    (r1 ~ r2) should equal (Concatenate(r1, r2))
    // simplifications
    (r ~ ∅) should equal(∅)
    (∅ ~ r) should equal(∅)
    (r ~ ε) should equal(r)
    (ε ~ r) should equal(r)
  }


  it should "be buildable using `|`" in {
    (r1 | r2) should equal(Union(r1, r2))
    // simplifications
    (r | ∅) should equal(r)
    (∅ | r) should equal(r)
    (Chars('a' -> 'c') | Chars('c' -> 'f')) should equal(Chars('a'->'f'))
    (r.* |   ε) should equal(r.*)
    (ε   | r.*) should equal(r.*)
    (α.* |   r) should equal(α.*)
    (r |   α.*) should equal(α.*)
    (r | r)     should equal(r)
  }

  it should "be buildable using `*`" in {
    r.* should equal(KleeneStar(r))
    // simplifications
    ∅.* should equal(ε)
    ε.* should equal(ε)
    (r.*).* should equal(r.*)
  }

  it should "be buildable using `!`" in {
    !r should equal(Complement(r))
    // Simplifications
    !(!r) should equal(r)
    !(∅) should equal(α.*)
    !ε should equal(α.+)
  }

  it should "be buildable using `&`" in {
    (r1 & r2) should equal(Intersect(r1, r2))
    // Simplifications
    (∅ & r) should equal(∅)
    (r & ∅) should equal(∅)
    (Chars('a'->'d') & Chars('c'->'f')) should equal (Chars('c'->'d'))
    (α.* & r) should equal(r)
    (r & α.*) should equal(r)
    (r & r) should equal(r)
  }

  it should "be buildable using `^`" in {
    (r^5) should equal(r ~ r ~ r ~ r ~ r)
  }

  it should "be buildable using `>=`" in {
    (r >= 3) should equal(r ~ r ~ r ~ r.*)
  }

  it should "be buildable using `<=`" in {
    (r <= 3) should equal(ε | r | (r ~ r) | (r ~ r ~ r))
  }

  it should "be buildable using `<=>`" in {
    (r <>(2, 3)) should equal((r ~ r ~ r.*) & (ε | r | (r ~ r) | (r ~ r ~ r)))
  }


  it should "be buildable using convenience methods 1" in {
    (b ~ c) should equal (Concatenate(b, c))
  }

  it should "be buildable using convenience methods 2" in {
    (b | (b ~ c)) should equal (Union(b, Concatenate(b, c)))
  }

  it should "be buildable using convenience methods 3" in {
    b.* should equal (KleeneStar(b))
  }

  it should "be buildable using convenience methods 4" in {
    !b should equal (Complement(b))
  }

  it should "be buildable using convenience methods 5" in {
    (b & (b ~ c)) should equal (Intersect(b, Concatenate(b, c)))
  }

  it should "be buildable using convenience methods 6" in {
    b.+ should equal (Concatenate(b, KleeneStar(b)))
  }

  it should "be buildable using convenience methods 7" in {
    b.? should equal (Union(ε, b))
  }

  it should "be buildable using convenience methods 8" in {
    b^3 should equal (Concatenate(Concatenate(b, b), b))
  }

  it should "be buildable using convenience methods 9" in {
    (b >= 2) should equal (Concatenate(Concatenate(b, b), KleeneStar(b)))
  }

  it should "be buildable using convenience methods 10" in {
    (b <= 2) should equal (Union(Union(ε, b), Concatenate(b, b)))
  }

  it should "be buildable using convenience methods 11" in {
    (b <> (1, 3)) should equal (Intersect(Concatenate(b, KleeneStar(b)), Union(Union(Union(ε, b), Concatenate(b, b)), Concatenate(Concatenate(b, b), b))))
  }

  it should "be buildable from strings" in {
    "ab".charset ~ "cd".concatenate should equal (Concatenate(Chars('a', 'b'),
      Concatenate(Chars('c'), Chars('d'))))
  }

  it should "pretty-print correctly" in {
    (b.? | (c >= 1)).prettyPrint should equal (s"""Union\n├─ Union\n│  ├─ ε\n│  └─ b\n└─ Concatenate\n   ├─ c\n   └─ KleeneStar\n      └─ c\n""")
  }


  behavior of "nullable"

  it should "recognize a nullable regex 1" in { 
    ε.nullable should equal (ε)
    //Complement(r1).nullable should equal (ε - r1.nullable)
    //d.nullable should equal (ε)     
  }

  it should "recognize a concatenatenation of regex (Test: 2)" in { 
    Concatenate(b,c).nullable should equal ((b.nullable) ~ (c.nullable))
  }

  it should "recognize the Complement of a regex (Test: 3)" in { 
    Complement(r).nullable should equal (ε)
  }

  it should "recognize an intesetion of KleeneStar'd regexes (Test: 4)" in { 
    Intersect(b.*, d.*).nullable should equal (ε)
    Intersect(Complement(r2), Complement(r1)).nullable should equal (ε)
  }

  it should "recognize KleeneStar as true (Test: 5)" in { 
     (r.*).nullable should equal (ε) //Kleene star should be nullable
  }

  it should "recognize Union of nullable re's (Test: 5)" in { 
     Union(r1, r2).nullable should equal (r1.nullable | r2.nullable)
  }

  it should "recognize a nullable complex regex (Test: 6)" in { 
    Concatenate(Complement(Intersect(r1, r)), Complement(r2)).nullable should equal (ε)
  }

  it should "recognize a nullable complex regex (Test: 7)" in { 
    Concatenate(Union(r1, Complement(r2)), Complement(r)).nullable should equal (ε)
  }

  // more tests... 

  it should "recognize a non-nullable regex 1" in { 
    d.nullable should equal (∅)
    ∅.nullable should equal (∅)
    r1.nullable should equal (∅)
    //rest of the re's given are not nullable
   }

   it should "recognize a non-nullable Complement regex (Test: 2)" in { 
    Complement(KleeneStar(r1)).nullable should equal (∅)
   }

   it should "recognize a non-nullable Union regex (Test: 3)" in { 
    Union(r1, r2).nullable should equal (∅)
   }

   it should "recognize a non-nullable concatenation regex (Test: 4)" in { 
    Concatenate(r1, r).nullable should equal (∅)
   }

   it should "recognize a non-nullable complex regex (Test: 5)" in { 
    Concatenate(Union(r1, r2), KleeneStar(r2)).nullable should equal (∅)
   }

   it should "recognize a non-nullable complex regex (Test: 6)" in { 
    Union(Concatenate(Intersect(r1, r), Complement(r2)), r).nullable should equal (∅)
  }

  it should "recognize a non-nullable complex regex (Test: 7)" in { 
    Intersect(Complement(Concatenate(r1, r2)), Complement(KleeneStar(r2))).nullable should equal (∅)
   }

  // more tests...
}
