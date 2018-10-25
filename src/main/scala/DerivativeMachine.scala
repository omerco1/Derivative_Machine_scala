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
      //if you get empty language return false 
      str.foldLeft(re)((currentRe, char) => run(Seq(currentRe), Seq(PushDerive), char)).nullable == ε //derive(char)).nullable == ε//
           //derive(char)).nullable == ε
  } 

  // Returns the derivative of 're' w.r.t. 'char'.
  def derive(char: Char): Regex = { 
    run(Seq(re), Seq(PushDerive), char)
  }

  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  // Derives a regular expression from the top of 'operands' w.r.t. 'char'.
  @annotation.tailrec
  private def run(operands: Seq[Regex], program: Program, char: Char): Regex = {
    if (program.isEmpty) {
      //println(operands.size)
      assert(operands.size == 1)
      operands.head
    } else { 
        //assert operands size > 1??
      //Seq(PushDerive, PushRe(re)) ++ Program.tail) --> pre-append 
       (program.head) match { // (operands, program.head)
      //   case (a +: b +: rest, PushUnion) => run(Union(a, b) +: rest, program.tail)
        case PushDerive => { 
          operands.head match {
            case `∅`|`ε` => run(∅ +: operands.tail, program.tail, char)
            case Chars(a) => { 
              //run(Chars(a) +: operands.tail, PushNullable +: program.tail, char)  ... run(ε +: operands.tail, program.tail, char)
              if (a.contains(char)) run(ε +: operands.tail, program.tail, char) else run(∅ +: operands.tail, program.tail, char)
            }
            case Union(a, b) => { run(a +: operands.tail, Seq(PushDerive, PushRe(b), PushDerive, PushUnion) ++ program.tail, char) }
            case Intersect(a, b) => { run(a +: operands.tail, Seq(PushDerive, PushRe(b), PushDerive, PushIntersect) ++ program.tail, char) }
            case Concatenate(a, b) => { 
              //Concatenate(a, b)??? ASK TA
              //COCATNT: (derive(re1, char) ~ re2) | (re1.nullable ~ derive(re2, char))
              run(b +: operands.tail, Seq(PushRe(a), PushDerive, PushConcatenate, PushRe(b), PushDerive, PushRe(a), PushNullable, PushConcatenate, PushUnion) ++ program.tail, char)
            }
            case Complement(a) => { run(a +: operands.tail, PushDerive +: PushComplement +: program.tail, char) }
            case rek @ KleeneStar(a) => { 
              //println(a +: rek +: operands.tail)
              //println("omer is testing")
              run(rek +: operands.tail,  PushRe(a) +: PushDerive +: PushConcatenate +: program.tail, char ) 
            }
           // case _ => {  } // throw an exception??? 
          }
        } 
        case PushConcatenate => { 
            //ASSERT > 2 assert(num >= 0)
            assert(operands.size >= 2)
           // println(operands)
          run((operands.head ~ operands.tail.head) +: operands.tail.tail, program.tail, char) 
        }
        case PushUnion => { //Union(operands.head, operands.tail.head)
          assert(operands.size >= 2)
          
          // println(operands.drop(2).head)
          // val tempOneResult = Seq(Union(operands.drop(2).head, operands.drop(2).tail.head))
          //tempOneResult
          run((operands.head | operands.tail.head) +: operands.tail.tail, program.tail, char)
        }
        case PushComplement => { 
          assert(operands.size >= 1)
          run(!(operands.head) +: operands.tail, program.tail, char) } 
        case PushIntersect => { 
           assert(operands.size >= 2)
          run((operands.head & operands.tail.head) +: operands.tail.tail, program.tail, char) } 
        case PushNullable => { run(operands.head.nullable +: operands.tail, program.tail, char) }
        case PushRe(re) => { run(Seq(re) ++ operands, program.tail, char) } 
      }
   }
}
}
