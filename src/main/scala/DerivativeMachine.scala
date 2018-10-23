// A virtual machine implementation of derivative-based matching.

package edu.ucsb.cs.cs162.regex.derivative

import edu.ucsb.cs.cs162.regex._

object `package` {
  // Programs for the DerivativeMachine.
  type Program = Seq[Instruction]

  // Pretty-print derivative virtual machine programs.
  def programToString(prog: Program): String = {
    val strs = for (inst <- prog) yield inst match {
      case `PushDerive` => "derive"
      case `PushConcatenate` => "concatenate"
      case `PushUnion` => "union"
      case `PushComplement` => "complement"
      case `PushIntersect` => "intersect"
      case `PushNullable` => "nullable"
      case PushRe(re) => "push " + re.toString
    }

    strs.mkString("\n")
  }
}

// Instructions for the virtual machine.
//
// - Derive: pop the top of the operand stack, compute its derivative w.r.t. the
//   machine's given char, then push the result back on the operand stack.
// - PushConcatentate: pop the top two elements of the operand stack and push
//   their concatenation back on.
// - PushUnion: pop the top two elements of the operand stack and push their
//   union back on.
// - PushComplement: pop the top of the operand stack, take its complement, and
//   push the result back on.
// - PushIntersect: pop the top two elements of the operand stack and push
//   their intersection back on.
// - PushNullable: pop the top of the operand stack, compute its nullability,
//   and push the result back on the operand stack.
// - PushRe(re): push re onto the top of the operand stack.
sealed abstract class Instruction
case object PushDerive extends Instruction
case object PushConcatenate extends Instruction
case object PushUnion extends Instruction
case object PushComplement extends Instruction
case object PushIntersect extends Instruction
case object PushNullable extends Instruction
case class PushRe(re: Regex) extends Instruction

class DerivativeMachine(re: Regex) {
  import Regex._

  //----------------------------------------------------------------------------
  // Public API.
  //----------------------------------------------------------------------------

  // Returns true iff 'str' is recognized by 're'.
  def eval(str: String): Boolean = { 
      //derive each character 
      for (i <- str) { 
        if(derive(i)==`∅`)
          false 
      }
      true 
  } 

  // Returns the derivative of 're' w.r.t. 'char'.
  def derive(char: Char): Regex = { 
    re
    // val operands: Seq[Regex] = re 
    // re.run(operands, PushDerive, char) 
    // case `∅` | `ε` => ∅
    // case Chars(chars) => if (chars.contains(char)) ε else ∅
    // case Concatenate(re1, re2) => {
    //   //(derive(re1, char) ~ re2) | (re1.nullable ~ derive(re2, char))
     
    // }
    // case Union(re1, re2) =>  Union(//derive(re1, char) | derive(re2, char)
    // case rek @ KleeneStar(re1) => derive(re1, char) ~ rek
    // case Complement(re1) => !derive(re1, char)
    // case Intersect(re1, re2) => derive(re1, char) & derive(re2, char)

  }

  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  // Derives a regular expression from the top of 'operands' w.r.t. 'char'.
  //@annotation.tailrec
  private def run(operands: Seq[Regex], program: Program, char: Char): Regex = {
   //  if (program.isEmpty) {
   //    assert(operands.size == 1)
   //    operands.head
   //  }
   //  else { 
   //    program match {
   //      case PushDerive => { } 
   //      case PushConcatenate => { }
   //      case PushUnion => { }
   //      case PushComplement => {} 
   //      case PushIntersect => { } 
   //      case PushNullable => { }
   //      case PushRe(re) => { } 
   //    }
   // }
   re
}
}