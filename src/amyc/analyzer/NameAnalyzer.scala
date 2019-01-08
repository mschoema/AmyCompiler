package amyc
package analyzer

import utils._
import ast.{Identifier, NominalTreeModule => N, SymbolicTreeModule => S}

// Name analyzer for Amy
// Takes a nominal program (names are plain strings, qualified names are string pairs)
// and returns a symbolic program, where all names have been resolved to unique Identifiers.
// Rejects programs that violate the Amy naming rules.
// Also populates and returns the symbol table.
object NameAnalyzer extends Pipeline[N.Program, (S.Program, SymbolTable)] {
  def run(ctx: Context)(p: N.Program): (S.Program, SymbolTable) = {
    import ctx.reporter._

    // Step 0: Initialize symbol table
    val table = new SymbolTable

    // Step 1: Add modules to table 
    val modNames = p.modules.groupBy(_.name)
    modNames.foreach { case (name, modules) =>
      if (modules.size > 1) {
        fatal(s"Two modules named $name in program", modules.head.position)
      }
    }

    modNames.keys.toList foreach table.addModule
    val mods = p.modules.toList


    // Helper method: will transform a nominal type 'tt' to a symbolic type,
    // given that we are within module 'inModule'.
    def transformType(tt: N.TypeTree, inModule: String): S.Type = {
      tt.tpe match {
        case N.IntType => S.IntType
        case N.RangeType(s, e) => S.RangeType(s, e)    //   <===
        case N.ArrayType(l) => S.ArrayType(l)          //   <===
        case N.BooleanType => S.BooleanType
        case N.StringType => S.StringType
        case N.UnitType => S.UnitType
        case N.ClassType(qn@N.QualifiedName(module, name)) =>
          table.getType(module getOrElse inModule, name) match {
            case Some(symbol) =>
              S.ClassType(symbol)
            case None =>
              fatal(s"Could not find type $qn", tt)
          }
      }
    }

    // Step 2: Check name uniqueness of definitions in each module
    mods.foreach { case N.ModuleDef(moduleName, defs, _) =>
      val defNames = defs.groupBy(_.name)
      defNames.foreach { case (name, defs) =>
        if (defs.size > 1) {
          fatal(s"Two definitions named $name in module $moduleName", defs.head.position)
        }
      }
    }


    // Step 3: Discover types and add them to symbol table
    mods.foreach { case N.ModuleDef(moduleName, defs, _) =>
      defs.foreach {
        case N.AbstractClassDef(name) => 
          table.addType(moduleName,name)
        case _ => 
      }
    }

    // Step 4: Discover type constructors, add them to table
    mods.foreach { case N.ModuleDef(moduleName, defs, _) =>
      defs.foreach {
        case df@N.CaseClassDef(name, fields, parent) => 
          val types = fields.map(e => transformType(e, moduleName))
          val par = table.getType(moduleName,parent)
          par match {
            case Some(parent) => 
              table.addConstructor(moduleName,name,types,parent)
            case None => 
              fatal(s"No abstract class named $parent",df)
          }
        case _ => 
      }
    }

    // Step 5: Discover functions signatures, add them to table
    mods.foreach { case N.ModuleDef(moduleName, defs, _) =>
      defs.foreach {
        case N.FunDef(name, params, retType, _) => 
          val types = params.map{ case N.ParamDef(nm,tt) => transformType(tt, moduleName)}
          table.addFunction(moduleName,name,types,transformType(retType,moduleName))
        case _ => 
      }
    }

    // Step 6: We now know all definitions in the program.
    //         Reconstruct modules and analyse function bodies/ expressions
    
    // This part is split into three transfrom functions,
    // for definitions, FunDefs, and expressions.
    // Keep in mind that we transform constructs of the NominalTreeModule 'N' to respective constructs of the SymbolicTreeModule 'S'.
    // transformFunDef is given as an example, as well as some code for the other ones

    def transformDef(df: N.ClassOrFunDef, module: String): S.ClassOrFunDef = { df match {
      case (acd@N.AbstractClassDef(name)) =>
        val Some(tp) = table.getType(module,name)
        S.AbstractClassDef(tp)
      case (ccd@N.CaseClassDef(name, _, _)) =>
        val Some((ident, constrSig)) = table.getConstructor(module, name)
        val types = constrSig.argTypes.map(S.TypeTree(_))
        S.CaseClassDef(ident, types, constrSig.parent)
      case fd: N.FunDef =>
        transformFunDef(fd, module)
    }}.setPos(df)

    def transformFunDef(fd: N.FunDef, module: String): S.FunDef = {
      val N.FunDef(name, params, retType, body) = fd
      val Some((sym, sig)) = table.getFunction(module, name)

      params.groupBy(_.name).foreach { case (name, ps) =>
        if (ps.size > 1) {
          fatal(s"Two parameters named $name in function ${fd.name}", fd)
        }
      }

      val paramNames = params.map(_.name)

      val newParams = params zip sig.argTypes map { case (pd@N.ParamDef(name, tt), tpe) =>
        val s = Identifier.fresh(name)
        S.ParamDef(s, S.TypeTree(tpe).setPos(tt)).setPos(pd)
      }

      val paramsMap = paramNames.zip(newParams.map(_.name)).toMap

      S.FunDef(
        sym,
        newParams,
        S.TypeTree(sig.retType).setPos(retType),
        transformExpr(body)(module, (paramsMap, Map()))
      ).setPos(fd)
    }

    // This function takes as implicit a pair of two maps:
    // The first is a map from names of parameters to their unique identifiers,
    // the second is similar for local variables.
    // Make sure to update them correctly if needed given the scoping rules of Amy
    def transformExpr(expr: N.Expr)
                     (implicit module: String, names: (Map[String, Identifier], Map[String, Identifier])): S.Expr = {
      val (params, locals) = names
      val res = expr match {
        case N.Error(msg) => 
          S.Error(transformExpr(msg))
        case N.Match(scrut, cases) =>
          // Returns a transformed pattern along with all bindings
          // from strings to unique identifiers for names bound in the pattern.
          // Also, calls 'fatal' if a new name violates the Amy naming rules.
          def transformPattern(pat: N.Pattern): (S.Pattern, List[(String, Identifier)]) = {
            val moreLocals = List[(String, Identifier)]()
            pat match {
              case N.WildcardPattern() =>
                (S.WildcardPattern(), moreLocals)
              case n@N.IdPattern(name) => 
                if (locals contains name) {
                  fatal(s"Variable name $name already used",n)
                }
                val s = Identifier.fresh(name)
                (S.IdPattern(s), (name,s) :: moreLocals)
              case N.LiteralPattern(lit) => 
                (S.LiteralPattern(transfromLiteral(lit)), moreLocals)
              case cp@N.CaseClassPattern(constr, pats) => 
                var ct: Option[(Identifier, ConstrSig)] = None
                constr match {
                  case N.QualifiedName(Some(mod),nm) => 
                    ct = table.getConstructor(mod, nm)
                  case N.QualifiedName(None,nm) => 
                    ct = table.getConstructor(module, nm)
                }
                ct match {
                  case Some((id, constrSig)) => 
                    if (constrSig.argTypes.size != pats.size) {
                      fatal(s"Constructor $id takes ${constrSig.argTypes.size} arguments and ${pats.size} were provided.",cp)
                    }
                    val newPatsAndLocals = pats.map(transformPattern(_))
                    val newPats = newPatsAndLocals.map{case (p,_) => p}
                    val evenMoreLocals = newPatsAndLocals.map{case (_,l) => l}.flatten
                    evenMoreLocals.groupBy(_._1).foreach { case (str, ids) =>
                      if (ids.size > 1) {
                        fatal(s"Multiple definitions of $str in pattern", cp)
                      }
                    }
                    (S.CaseClassPattern(id, newPats).setPos(pat), evenMoreLocals)
                  case None => 
                    fatal(s"No Constructor named $constr in module ${constr.module.getOrElse(module)}", cp)
                }
            }
          }

          def transformCase(cse: N.MatchCase) = {
            val N.MatchCase(pat, rhs) = cse
            val (newPat, moreLocals) = transformPattern(pat)
            S.MatchCase(newPat, transformExpr(rhs)(module, (params,moreLocals.toMap ++ locals)))
          }

          S.Match(transformExpr(scrut), cases.map(transformCase))

        case N.Ite(cond, thenn, elze) =>
          S.Ite(transformExpr(cond), transformExpr(thenn), transformExpr(elze))
        case N.Let(df, value, body, rc) => 
          if (locals contains df.name) {
            fatal(s"Variable name ${df.name} already used",df)
          } else if (params contains df.name) {
            warning(s"Variable ${df.name} has same name as parameter",df)
          }
          val s = Identifier.fresh(df.name)
          val newDf = S.ParamDef(s, S.TypeTree(transformType(df.tt, module)))
          val newValue = transformExpr(value)
          val newBody = transformExpr(body)(module, (params,locals + (df.name -> s)))
          S.Let(newDf, newValue, newBody, rc)
        case N.Sequence(e1, e2) => 
          S.Sequence(transformExpr(e1), transformExpr(e2))
        case N.ArrayLength(va) =>                                   //    <===
          S.ArrayLength(transfromVariable(va, names))
        case N.ArrayAccess(va, elem, rc) =>                             //    <===
          S.ArrayAccess(transfromVariable(va, names), transformExpr(elem), rc)
        case c@N.Call(qname, args) => 
          val fct = table.getFunction(qname.module.getOrElse(module),qname.name)
          fct match {
            case Some((fid,funsig)) => 
              if (funsig.argTypes.size != args.size) {
                fatal(s"Function ${qname.name} takes ${funsig.argTypes.size} arguments and ${args.size} were provided.",c)
              }
              S.Call(fid,args.map{ case (arg, rc) => (transformExpr(arg), rc) })
            case None => 
              val tpe = table.getConstructor(qname.module.getOrElse(module),qname.name)
              tpe match {
                case Some((tid,constrsig)) => 
                  if (constrsig.argTypes.size != args.size) {
                    if (args.size == 1) {
                      fatal(s"Constructor ${qname.name} takes ${constrsig.argTypes.size} arguments and ${args.size} was provided.",c)
                    } else {
                      fatal(s"Constructor ${qname.name} takes ${constrsig.argTypes.size} arguments and ${args.size} were provided.",c)
                    }
                  }
                  S.Call(tid,args.map{ case (arg, rc) => (transformExpr(arg), rc) })
                case None => 
                  fatal(s"No function or constructor named ${qname.name} in module ${qname.module.getOrElse(module)}",c)
              }
          }
        case N.Neg(e) => 
          S.Neg(transformExpr(e))
        case N.Not(e) => 
          S.Not(transformExpr(e))
        case N.Concat(lhs,rhs) => 
          S.Concat(transformExpr(lhs),transformExpr(rhs))
        case N.Equals(lhs,rhs) => 
          S.Equals(transformExpr(lhs),transformExpr(rhs))
        case N.Or(lhs,rhs) => 
          S.Or(transformExpr(lhs),transformExpr(rhs))
        case N.And(lhs,rhs) => 
          S.And(transformExpr(lhs),transformExpr(rhs))
        case N.LessEquals(lhs,rhs) => 
          S.LessEquals(transformExpr(lhs),transformExpr(rhs))
        case N.LessThan(lhs,rhs) => 
          S.LessThan(transformExpr(lhs),transformExpr(rhs))
        case N.Mod(lhs,rhs) => 
          S.Mod(transformExpr(lhs),transformExpr(rhs))
        case N.Div(lhs,rhs) => 
          S.Div(transformExpr(lhs),transformExpr(rhs))
        case N.Times(lhs,rhs) => 
          S.Times(transformExpr(lhs),transformExpr(rhs))
        case N.Minus(lhs,rhs) => 
          S.Minus(transformExpr(lhs),transformExpr(rhs))
        case N.Plus(lhs,rhs) => 
          S.Plus(transformExpr(lhs),transformExpr(rhs))
        case va: N.Variable =>                                      //    <===
          transfromVariable(va, names)
        case lit: N.Literal[_] => 
          transfromLiteral(lit)
      }
      res.setPos(expr)
    }

    def transfromVariable(variable: N.Variable, names: (Map[String, Identifier], Map[String, Identifier])) = {    //    <===
      val (params, locals) = names
      variable match {
        case va@N.Variable(name) => 
          S.Variable(locals.getOrElse(name,params.getOrElse(name,fatal(s"No variable named $name",va))))
      }
    }

    def transfromLiteral(lit: N.Literal[Any]) = {
      lit match {
        case N.IntLiteral(value) => 
          S.IntLiteral(value)
        case N.BooleanLiteral(value) => 
          S.BooleanLiteral(value)
        case N.StringLiteral(value) => 
          S.StringLiteral(value)
        case N.UnitLiteral() => 
          S.UnitLiteral()
        case N.ArrayLiteral(length, value) =>       //    <===
          S.ArrayLiteral(length, value)
      }
    }

    // Putting it all together to construct the final program for step 6.
    val newProgram = S.Program(
      p.modules map { case mod@N.ModuleDef(name, defs, optExpr) =>
        S.ModuleDef(
          table.getModule(name).get,
          defs map (transformDef(_, name)),
          optExpr map (transformExpr(_)(name, (Map(), Map())))
        ).setPos(mod)
      }
    ).setPos(p)

    (newProgram, table)

  }
}
