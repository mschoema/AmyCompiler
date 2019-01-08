package amyc
package analyzer

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private (id: Int) extends Type
    object TypeVariable {
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] = {
      
      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))
      
      e match {
        case IntLiteral(i) =>
          topLevelConstraint(IntType)

        case ArrayLiteral(l, _) =>                          //    <===
          topLevelConstraint(ArrayType(l))

        case UnitLiteral() => 
          topLevelConstraint(UnitType)

        case BooleanLiteral(_) => 
          topLevelConstraint(BooleanType)

        case StringLiteral(_) => 
          topLevelConstraint(StringType)

        case Variable(nm) => 
          topLevelConstraint(env.get(nm).get)

        case Plus(lhs, rhs) => 
          expected match {
            case IntType => 
              genConstraints(lhs,IntType) ++ genConstraints(rhs,IntType)
            case _ => 
              List(Constraint(IntType, expected, e.position)) ++ genConstraints(lhs,IntType) ++ genConstraints(rhs,IntType)
          }

        case Minus(lhs, rhs) => 
          expected match {
            case IntType => 
              genConstraints(lhs,IntType) ++ genConstraints(rhs,IntType)
            case _ => 
              List(Constraint(IntType, expected, e.position)) ++ genConstraints(lhs,IntType) ++ genConstraints(rhs,IntType)
          }

        case Times(lhs, rhs) => 
          expected match {
            case IntType => 
              genConstraints(lhs,IntType) ++ genConstraints(rhs,IntType)
            case _ => 
              List(Constraint(IntType, expected, e.position)) ++ genConstraints(lhs,IntType) ++ genConstraints(rhs,IntType)
          }

        case Div(lhs, rhs) => 
          expected match {
            case IntType => 
              genConstraints(lhs,IntType) ++ genConstraints(rhs,IntType)
            case _ => 
              List(Constraint(IntType, expected, e.position)) ++ genConstraints(lhs,IntType) ++ genConstraints(rhs,IntType)
          }

        case Mod(lhs, rhs) => 
          expected match {
            case IntType => 
              genConstraints(lhs,IntType) ++ genConstraints(rhs,IntType)
            case _ => 
              List(Constraint(IntType, expected, e.position)) ++ genConstraints(lhs,IntType) ++ genConstraints(rhs,IntType)
          }

        case LessThan(lhs, rhs) => 
          expected match {
            case BooleanType => 
              genConstraints(lhs,IntType) ++ genConstraints(rhs,IntType)
            case _ => 
              List(Constraint(BooleanType, expected, e.position)) ++ genConstraints(lhs,IntType) ++ genConstraints(rhs,IntType)
          }

        case LessEquals(lhs, rhs) => 
          expected match {
            case BooleanType => 
              genConstraints(lhs,IntType) ++ genConstraints(rhs,IntType)
            case _ => 
              List(Constraint(BooleanType, expected, e.position)) ++ genConstraints(lhs,IntType) ++ genConstraints(rhs,IntType)
          }

        case And(lhs, rhs) => 
          expected match {
            case BooleanType => 
              genConstraints(lhs,BooleanType) ++ genConstraints(rhs,BooleanType)
            case _ => 
              List(Constraint(BooleanType, expected, e.position)) ++ genConstraints(lhs,BooleanType) ++ genConstraints(rhs,BooleanType)
          }

        case Or(lhs, rhs) => 
          expected match {
            case BooleanType => 
              genConstraints(lhs,BooleanType) ++ genConstraints(rhs,BooleanType)
            case _ => 
              List(Constraint(BooleanType, expected, e.position)) ++ genConstraints(lhs,BooleanType) ++ genConstraints(rhs,BooleanType)
          }

        case Equals(lhs, rhs) => 
          expected match {
            case BooleanType => 
              val tv = TypeVariable.fresh()
              genConstraints(lhs,tv) ++ genConstraints(rhs,tv)
            case _ => 
              val tv = TypeVariable.fresh()
              List(Constraint(BooleanType, expected, e.position)) ++ genConstraints(lhs,tv) ++ genConstraints(rhs,tv)
          }

        case Concat(lhs, rhs) => 
          expected match {
            case StringType => 
              genConstraints(lhs,StringType) ++ genConstraints(rhs,StringType)
            case _ => 
              List(Constraint(StringType, expected, e.position)) ++ genConstraints(lhs,StringType) ++ genConstraints(rhs,StringType)
          }

        case Not(e) => 
          expected match {
            case BooleanType => 
              genConstraints(e,BooleanType)
            case _ => 
              List(Constraint(BooleanType, expected, e.position)) ++ genConstraints(e,BooleanType)
          }

        case Neg(e) => 
          expected match {
            case IntType => 
              genConstraints(e,IntType)
            case _ => 
              List(Constraint(IntType, expected, e.position)) ++ genConstraints(e,IntType)
          }

        case Call(qname, args) => 
          val fct = table.getFunction(qname).getOrElse(table.getConstructor(qname).get)
          fct match {
            case FunSig(argTypes, retType, owner) => 
              val argsAndTypes = args zip argTypes
              val argConstr = argsAndTypes.flatMap{ case ((arg, _),tp) => genConstraints(arg, tp)}
              argConstr ++ List(Constraint(retType, expected, e.position))
            case ConstrSig(argTypes, parent, index) => 
              val argsAndTypes = args zip argTypes
              val argConstr = argsAndTypes.flatMap{ case ((arg, _),tp) => genConstraints(arg, tp)}
              argConstr ++ List(Constraint(ClassType(parent), expected, e.position))
          }

        case ArrayAccess(variable, elem, _) =>                    //    <===
          List(Constraint(IntType, expected, e.position)) ++ genConstraints(variable, GenericArrayType) ++ genConstraints(elem, IntType)

        case ArrayLength(variable) =>                             //    <===
          List(Constraint(IntType, expected, e.position)) ++ genConstraints(variable, GenericArrayType)

        case Sequence(e1, e2) => 
          val tv = TypeVariable.fresh()
          genConstraints(e1,tv) ++ genConstraints(e2,expected)
          
        case Let(df, value, body, _) => 
          val nm = df.name
          val tv = df.tt.tpe
          genConstraints(value,tv) ++ genConstraints(body,expected)(env + (nm -> tv))


        case Ite(cond, thenn, elze) => 
            genConstraints(cond, BooleanType) ++ genConstraints(thenn, expected) ++ genConstraints(elze, expected)

        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def handlePattern(pat: Pattern, scrutExpected: Type):
            (List[Constraint], Map[Identifier, Type]) =
          {
            pat match {
              case WildcardPattern() => 
                (List(),Map())
              case IdPattern(id) => 
                (List(),Map(id -> scrutExpected))
              case LiteralPattern(lit) => 
                (genConstraints(lit,scrutExpected), Map())
              case CaseClassPattern(constr, args) => 
                val constrSig = table.getConstructor(constr).get
                val argsAndTypes = args zip constrSig.argTypes
                val (listPats, listMoreEnv) = argsAndTypes.map{ case (pat,tp) => handlePattern(pat, tp)}.unzip
                val (patConstraints, moreEnv) = (listPats.flatten, listMoreEnv.flatten.toMap)
                (patConstraints ++ List(Constraint(constrSig.retType, scrutExpected, pat.position)), moreEnv)
            }
          }

          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] = {
            val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
            patConstraints ++ genConstraints(cse.expr, expected)(env ++ moreEnv)
          }

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st))

        case Error(e) =>
          genConstraints(e,StringType)
      }
    }


    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] = {
      // Do a single substitution.
      def subst(tpe: Type, from: Int, to: Type): Type = {
        tpe match {
          case TypeVariable(`from`) => to
          case other => other
        }
      }

      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }

    // Solve the given set of typing constraints and
    //  call `typeError` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          // HINT: You can use the `subst_*` helper above to replace a type variable
          //       by another type in your current set of constraints.
          found match {
            case TypeVariable(from) => 
              solveConstraints(subst_*(more,from,expected))
            case _ => 
              expected match {
                case TypeVariable(from) => 
                  solveConstraints(subst_*(more,from,found))
                case _ => 
                  if (wrongConstraint(found,expected)) {        //    <===
                    error(s"Type Error: expected $expected found $found", pos)
                  }
                  solveConstraints(more)
              }
          }
      }
    }

    def wrongConstraint(found: Type, expected: Type): Boolean = {          //    <===
      found match {
        case GenericArrayType => 
          expected match {
            case GenericArrayType => 
              false
            case ArrayType(_) => 
              false
            case _ => 
              true
          }
        case ArrayType(l) => 
          expected match {
            case GenericArrayType => 
              false
            case ArrayType(l2) => 
              l != l2
            case _ => 
              true
          }
        case RangeType(_,_) =>
          expected match {
            case RangeType(_,_) => 
              false
            case IntType => 
              false
            case _ => 
              true
          }
        case IntType => 
          expected match {
            case RangeType(_,_) => 
              false
            case IntType => 
              false
            case _ => 
              true
          }
        case _ => 
          found != expected
      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map{ case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(env))
      }

      // Type-check expression if present. We allow the result to be of an arbitrary type by
      // passing a fresh (and therefore unconstrained) type variable as the expected type.
      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(Map())))
    }

    v

  }
}
