package amyc
package analyzer

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier

// Range checker for Amy
// Takes a symbolic program and modifies the range check attribute in array accesses,
// local variable definitions and function/constructor calls.
// The range check attribute will be true if a dynamic check has to be added during the code generation.
// An error will we thrown in the case of a range error.
object RangeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    // Used to denote any type that is not a range type or array type.
    case object GenericType extends Type

    def transformDef(df: ClassOrFunDef): ClassOrFunDef = { 
      df match {
        case FunDef(name, params, retType, body) =>
          val env = params.map{ case ParamDef(name, tt) => name -> tt.tpe }.toMap
          FunDef(name, params, retType, transformExpr(body)(env))
        case _ => 
          df
      }
    }.setPos(df)

    def transformExpr(expr: Expr)(implicit env: Map[Identifier, Type]): Expr = {
      val res = expr match {
        case Error(msg) => 
          Error(transformExpr(msg))
        case Match(scrut, cases) =>

          def handlePattern(pat: Pattern, scrutType: Type): Map[Identifier, Type] = {
            pat match {
              case IdPattern(id) => 
                Map(id -> scrutType)
              case CaseClassPattern(constr, args) => 
                val constrSig = table.getConstructor(constr).get
                val argsAndTypes = args zip constrSig.argTypes
                val moreEnv = argsAndTypes.map{ case (pat,tp) => handlePattern(pat, tp)}.flatten.toMap
                moreEnv
              case _ => 
                Map()
            }
          }

          val scrutType = getType(scrut, env)

          Match(transformExpr(scrut), cases.map{ 
            case MatchCase(pat, expr) => 
              val moreEnv = handlePattern(pat, scrutType)
              MatchCase(pat, transformExpr(expr)(env ++ moreEnv))
          })

        case Ite(cond, thenn, elze) =>
          Ite(transformExpr(cond), transformExpr(thenn), transformExpr(elze))
        case Let(df, value, body, rc) => 
          val nm = df.name
          val tp = df.tt.tpe
          tp match {
            case RangeType(s, e) => 
              val check = rangeCheckExpr(value, tp, env)
              Let(df, transformExpr(value), transformExpr(body)(env + (nm -> tp)), check)
            case _ => 
              Let(df, transformExpr(value), transformExpr(body)(env + (nm -> tp)), rc)
          }
        case Sequence(e1, e2) => 
          Sequence(transformExpr(e1), transformExpr(e2))
        case ArrayAccess(va, elem, rc) => 
          val arrType = env.get(va.name).get
          arrType match {
            case ArrayType(l) => 
              val check = rangeCheckExpr(elem, RangeType(0,l-1), env)
              ArrayAccess(va, transformExpr(elem), check)
            case _ => 
              ArrayAccess(va, transformExpr(elem), rc)
          }
        case Call(qname, args) => 
          val fct = table.getFunction(qname).getOrElse(table.getConstructor(qname).get)
          fct match {
            case FunSig(argTypes, retType, owner) => 
              Call(qname,args.zip(argTypes).map { 
                case ((arg, rc), tp) => 
                  tp match {
                    case RangeType(s, e) => 
                      val check = rangeCheckExpr(arg, tp, env)
                      (transformExpr(arg), check)
                    case _ => 
                      (transformExpr(arg), rc)
                  }
              })
            case ConstrSig(argTypes, parent, index) => 
              Call(qname,args.zip(argTypes).map { 
                case ((arg, rc), tp) => 
                  tp match {
                    case RangeType(s, e) => 
                      val check = rangeCheckExpr(arg, tp, env)
                      (transformExpr(arg), check)
                    case _ => 
                      (transformExpr(arg), rc)
                  }
              })
          }
        case Neg(e) => 
          Neg(transformExpr(e))
        case Not(e) => 
          Not(transformExpr(e))
        case Concat(lhs,rhs) => 
          Concat(transformExpr(lhs),transformExpr(rhs))
        case Equals(lhs,rhs) => 
          Equals(transformExpr(lhs),transformExpr(rhs))
        case Or(lhs,rhs) => 
          Or(transformExpr(lhs),transformExpr(rhs))
        case And(lhs,rhs) => 
          And(transformExpr(lhs),transformExpr(rhs))
        case LessEquals(lhs,rhs) => 
          LessEquals(transformExpr(lhs),transformExpr(rhs))
        case LessThan(lhs,rhs) => 
          LessThan(transformExpr(lhs),transformExpr(rhs))
        case Mod(lhs,rhs) => 
          Mod(transformExpr(lhs),transformExpr(rhs))
        case Div(lhs,rhs) => 
          Div(transformExpr(lhs),transformExpr(rhs))
        case Times(lhs,rhs) => 
          Times(transformExpr(lhs),transformExpr(rhs))
        case Minus(lhs,rhs) => 
          Minus(transformExpr(lhs),transformExpr(rhs))
        case Plus(lhs,rhs) => 
          Plus(transformExpr(lhs),transformExpr(rhs))
        case _ => 
          expr
      }
      res.setPos(expr)
    }

    def getType(expr: Expr, env: Map[Identifier, Type]): Type = {
      expr match {
        case ArrayLiteral(l, _) => 
          ArrayType(l)
        case IntLiteral(i) => 
          RangeType(i,i)
        case Variable(nm) => 
          env.get(nm).get
        case ArrayLength(v) => 
          val ArrayType(l) = env.get(v.name).get
          RangeType(l,l)
        case Plus(lhs, rhs) => 
          val lhsTpe = getType(lhs, env)
          val rhsTpe = getType(rhs, env)
          lhsTpe match {
            case RangeType(s1,e1) => 
              rhsTpe match {
                case RangeType(s2,e2) => 
                  RangeType(s1 + s2, e1 + e2)
                case _ => 
                  GenericType
              }
            case _ => 
              GenericType
          }
        case Minus(lhs, rhs) => 
          val lhsTpe = getType(lhs, env)
          val rhsTpe = getType(rhs, env)
          lhsTpe match {
            case RangeType(s1,e1) => 
              rhsTpe match {
                case RangeType(s2,e2) => 
                  RangeType(s1 - e2, e1 - s2)
                case _ => 
                  GenericType
              }
            case _ => 
              GenericType
          }
        case _ => 
          GenericType
      }
    }

    def rangeCheckExpr(expr: Expr, rtp: Type, env: Map[Identifier, Type]): Boolean = {
      print(expr.position)
      print(" check?  ")
      val tpe = getType(expr, env)
      val RangeType(min, max) = rtp
      tpe match {
        case RangeType(s, e) => 
          if (e < min || s > max) {
            fatal("Range Error!", expr)
          } else if (s < min || e > max) {
            println("check")
            true
          } else {
            println("no check")
            false
          }
        case _ => 
          println("check")
          true
      }
    }

    // Putting it all together to construct the final program for step 6.
    val newProgram = Program(
      program.modules map { case mod@ModuleDef(name, defs, optExpr) =>
        ModuleDef(
          name,
          defs map (transformDef(_)),
          optExpr map (transformExpr(_)(Map()))
        ).setPos(mod)
      }
    ).setPos(program)

    (newProgram, table)

  }
}
