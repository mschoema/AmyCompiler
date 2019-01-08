package amyc
package parsing

import grammarcomp.parsing._
import utils.Positioned
import ast.NominalTreeModule._
import Tokens._
import utils._

// Implements the translation from parse trees to ASTs for the LL1 grammar,
// that is, this should correspond to Parser.amyGrammarLL1.
// We extend the plain ASTConstructor as some things will be the same -- you should
// override whatever has changed. You can look into ASTConstructor as an example.
class ASTConstructorLL1(ctx: Context) extends ASTConstructor {
  import ctx.reporter._

  // TODO: Override methods from ASTConstructor as needed
  
  override def constructQname(pTree: NodeOrLeaf[Token]): (QualifiedName, Positioned) = {
    pTree match {
      case Node('QName ::= _, List(mod, Node('QNameRest ::= _, List(_,nm)))) =>
        val (module, pos) = constructName(mod)
        val (name, _) = constructName(nm)
        (QualifiedName(Some(module), name), pos)
      case Node('QName ::= _, List(id, Node('QNameRest ::= _, _))) =>
        val (name, pos) = constructName(id)
        (QualifiedName(None, name), pos)
    }
  }

  override def constructExpr(pTree: NodeOrLeaf[Token]): Expr = {
    pTree match {
      case Node('Expr ::= _, List(Leaf(vt),param,_,value,_,body)) =>
        Let(constructParam(param), 
          constructExprMatch(value), 
          constructExpr(body),
          false).setPos(vt)
      case Node('Expr ::= _, List(eMatch, exprRest)) =>
        val exprMatch = constructExprMatch(eMatch)
        constructExprRest(exprMatch, exprRest)
    }
  }

  def constructExprRest(exprMatch: Expr, exprRest: NodeOrLeaf[Token]): Expr = {
    exprRest match {
      case Node('ExprRest ::= _, List(_, e)) =>
        val expr = constructExpr(e)
        Sequence(exprMatch,expr).setPos(exprMatch)
      case Node('ExprRest ::= _, _) => 
        exprMatch
    }
  }

  def constructExprMatch(pTree: NodeOrLeaf[Token]): Expr = {
    pTree match {
      case Node('ExprMatch ::= _, List(exprOr, matchRest)) =>
        matchRest match {
          case Node('MatchRest ::= _, List(Leaf(mt),_,cs,cases,_)) => 
            val scrut = constructExprOr(exprOr)
            Match(scrut, constructCase(cs) :: constructList(cases, constructCase)).setPos(mt)
          case Node('MatchRest ::= _, _) => 
            constructExprOr(exprOr)
        }
    }
  }

  def constructExprOr(pTree: NodeOrLeaf[Token]): Expr = {
    pTree match {
      case Node('ExprOr ::= _, List(exprAnd, Node('OrRest ::= _, List(Leaf(_))))) =>
        constructExprAnd(exprAnd)
      case Node('ExprOr ::= _, List(exprAnd, rightOp)) =>
        constructOpExpr(constructExprAnd(exprAnd), rightOp)
    }
  }

  def constructExprAnd(pTree: NodeOrLeaf[Token]): Expr = {
    pTree match {
      case Node('ExprAnd ::= _, List(exprEq, Node('AndRest ::= _, List(Leaf(_))))) =>
        constructExprEq(exprEq)
      case Node('ExprAnd ::= _, List(exprEq, rightOp)) =>
        constructOpExpr(constructExprEq(exprEq), rightOp)
    }
  }

  def constructExprEq(pTree: NodeOrLeaf[Token]): Expr = {
    pTree match {
      case Node('ExprEq ::= _, List(exprComp, Node('EqRest ::= _, List(Leaf(_))))) =>
        constructExprComp(exprComp)
      case Node('ExprEq ::= _, List(exprComp, rightOp)) =>
        constructOpExpr(constructExprComp(exprComp), rightOp)
    }
  }

  def constructExprComp(pTree: NodeOrLeaf[Token]): Expr = {
    pTree match {
      case Node('ExprComp ::= _, List(exprLowerOp, Node('CompRest ::= _, List(Leaf(_))))) =>
        constructExprLowerOp(exprLowerOp)
      case Node('ExprComp ::= _, List(exprLowerOp, rightOp)) =>
        constructOpExpr(constructExprLowerOp(exprLowerOp), rightOp)
    }
  }

  def constructExprLowerOp(pTree: NodeOrLeaf[Token]): Expr = {
    pTree match {
      case Node('ExprLowerOp ::= _, List(exprHigherOp, Node('LoweropRest ::= _, List(Leaf(_))))) =>
        constructExprHigherOp(exprHigherOp)
      case Node('ExprLowerOp ::= _, List(exprHigherOp, rightOp)) =>
        constructOpExpr(constructExprHigherOp(exprHigherOp), rightOp)
    }
  }

  def constructExprHigherOp(pTree: NodeOrLeaf[Token]): Expr = {
    pTree match {
      case Node('ExprHigherOp ::= _, List(exprUnary, Node('HigherOpRest ::= _, List(Leaf(_))))) =>
        constructExprUnary(exprUnary)
      case Node('ExprHigherOp ::= _, List(exprUnary, rightOp)) =>
        constructOpExpr(constructExprUnary(exprUnary), rightOp)
    }
  }

  def constructExprUnary(pTree: NodeOrLeaf[Token]): Expr = {
    pTree match {
      case Node('ExprUnary ::= List(BANG(), _), List(Leaf(bt), e)) =>
        Not(constructExprTop(e)).setPos(bt)
      case Node('ExprUnary ::= List(MINUS(), _), List(Leaf(mt), e)) =>
        Neg(constructExprTop(e)).setPos(mt)
      case Node('ExprUnary ::= _, List(e)) =>
        constructExprTop(e)
    }
  }

  def constructExprTop(pTree: NodeOrLeaf[Token]): Expr = {
    pTree match {
      case Node('ExprTop ::= _, List(Leaf(it),_,e1,_,_,e2,_,_,_,e3,_)) =>
        Ite(constructExpr(e1), 
          constructExpr(e2), 
          constructExpr(e3)).setPos(it)
      case Node('ExprTop ::= _, List(Leaf(et),_,e,_)) =>
        Error(constructExpr(e)).setPos(et)
      case Node('ExprTop ::= _, List(Leaf(lpt), Node('ParExpr ::= _, List(_)))) =>
        UnitLiteral().setPos(lpt)
      case Node('ExprTop ::= _, List(Leaf(lpt), Node('ParExpr ::= _, List(e,_)))) => 
        constructExpr(e).setPos(lpt)
      case Node('ExprTop ::= _, List(id, Node('QNameCallOrArr ::= _, List(call)))) =>         //    <===
        constructCall(id,call)
      case Node('ExprTop ::= _, List(id, Node('QNameCallOrArr ::= _, List(_,expr,_)))) =>     //    <===
        val (name, pos) = constructName(id)
        ArrayAccess(Variable(name).setPos(pos), constructExpr(expr), false).setPos(pos)
      case Node('ExprTop ::= _, List(lit)) => 
        constructLiteral(lit)
    }
  }

  override def constructOp(ptree: NodeOrLeaf[Token]): (Expr, Expr) => Expr = {
    ptree match {
      case Node(_, List(Leaf(t))) =>
        tokenToExpr(t)
      case Leaf(t) => 
        tokenToExpr(t)
    }
  }

  def constructCall(id: NodeOrLeaf[Token],pTree: NodeOrLeaf[Token]): Expr = {
    val (name, pos) = constructName(id)
    pTree match {
      case Node('QNameCall ::= _, List(_, Node('QNameCallRest ::= _, List(Leaf(length))))) =>   //    <===
        ArrayLength(Variable(name).setPos(pos)).setPos(pos)
      case Node('QNameCall ::= _, List(_, Node('QNameCallRest ::= _, List(nm, _, as, _)))) => 
        val (firstName, _) = constructName(nm)
        val args = constructList(as, constructExpr, hasComma = true).foldRight(List[(Expr, Boolean)]()) {
          (arg, l) => (arg,false) :: l
        }
        Call(QualifiedName(Some(name), firstName),args).setPos(pos)
      case Node('QNameCall ::= _, List(_, as, _)) =>
        val args = constructList(as, constructExpr, hasComma = true).foldRight(List[(Expr, Boolean)]()) {
          (arg, l) => (arg,false) :: l
        }
        Call(QualifiedName(None, name),args).setPos(pos)
      case  Node('QNameCall ::= _, _) => 
        Variable(name).setPos(pos)
    }
  }

  override def constructLiteral(pTree: NodeOrLeaf[Token]): Literal[_] = {
    pTree match {
      case Node('Literal ::= List(INTLITSENT), List(Leaf(it@INTLIT(i)))) =>
        IntLiteral(i).setPos(it)
      case Node('Literal ::= List(STRINGLITSENT), List(Leaf(st@STRINGLIT(s)))) =>
        StringLiteral(s).setPos(st)
      case Node('Literal ::= _, List(Leaf(tt@TRUE()))) =>
        BooleanLiteral(true).setPos(tt)
      case Node('Literal ::= _, List(Leaf(tf@FALSE()))) =>
        BooleanLiteral(false).setPos(tf)
      case Node('Literal ::= _, List(Leaf(lbr),Leaf(INTLIT(i)),elems,_)) =>                     //    <===
        val list: List[Int] = i :: constructList(elems, constructInt, hasComma = true)
        ArrayLiteral(list.length, list).setPos(lbr)
    }
  }

  def constructInt(pTree: NodeOrLeaf[Token]): Int = {                                           //    <===
    pTree match {
      case Leaf(INTLIT(i)) => 
        i
      case _ =>                           // This never happens, but is just there to remove an unwanted warning
        fatal("Unexpected error")
    }
  }

  override def constructType(pTree: NodeOrLeaf[Token]): TypeTree = {
    pTree match {
      case Node('Type ::= _, List(Leaf(arr),_,Leaf(INTLIT(i)),_)) =>                            //    <===
        TypeTree(ArrayType(i)).setPos(arr)
      case Node('Type ::= _, List(Leaf(lbr),Leaf(INTLIT(start)),_,_,Leaf(INTLIT(end)),_)) =>    //    <===
        TypeTree(RangeType(start, end)).setPos(lbr)
      case Node('Type ::= _, List(Leaf(tp))) =>
        TypeTree((tp: @unchecked) match {
          case INT() => IntType
          case STRING() => StringType
          case BOOLEAN() => BooleanType
          case UNIT() => UnitType
        }).setPos(tp)
      case Node('Type ::= _, List(qn)) =>
        val (qname, pos) = constructQname(qn)
        TypeTree(ClassType(qname)).setPos(pos)
    }
  }

  override def constructPattern(pTree: NodeOrLeaf[Token]): Pattern = {
    pTree match {
      case Node('Pattern ::= List(UNDERSCORE()), List(Leaf(ut))) =>
        WildcardPattern().setPos(ut)
      case Node('Pattern ::= List('Literal), List(lit)) =>
        val literal = constructLiteral(lit)
        LiteralPattern(literal).setPos(literal)
      case Node('Pattern ::= List('Id,'IdAfter), List(id, ia)) =>
        constructIdPattern(id,ia)
      case Node('Pattern ::= _, List(Leaf(lp), _)) => 
        LiteralPattern(UnitLiteral().setPos(lp))setPos(lp)
    }
  }

  def constructIdPattern(id: NodeOrLeaf[Token], pTree: NodeOrLeaf[Token]): Pattern = {
    pTree match {
      case Node('IdAfter ::= _, List(_, nm, _, patts, _)) =>
        val (module, pos) = constructName(id)
        val (name, _) = constructName(nm)
        val patterns = constructList(patts, constructPattern, hasComma = true)
        CaseClassPattern(QualifiedName(Some(module), name), patterns).setPos(pos)
      case Node('IdAfter ::= _, List(_, patts, _)) =>
        val (name, pos) = constructName(id)
        val patterns = constructList(patts, constructPattern, hasComma = true)
        CaseClassPattern(QualifiedName(None, name), patterns).setPos(pos)
      case Node('IdAfter ::= _, _) =>
        val (name, pos) = constructName(id)
        IdPattern(name).setPos(pos)
    }
  }
  

  // Important helper method:
  // Because LL1 grammar is not helpful in implementing left associativity,
  // we give you this method to reconstruct it.
  // This method takes the left operand of an operator (leftopd)
  // as well as the tree that corresponds to the operator plus the right operand (ptree)
  // It parses the right hand side and then reconstruct the operator expression
  // with correct associativity.
  // If ptree is empty, it means we have no more operators and the leftopd is returned.
  // Note: You may have to override constructOp also, depending on your implementation
  def constructOpExpr(leftopd: Expr, pTree: NodeOrLeaf[Token]): Expr = {
    pTree match {
      case Node(_, List()) => //epsilon rule of the nonterminals
        leftopd
      case Node(sym ::= _, List(op, rightNode))
        if Set('OrRest) contains sym =>
        rightNode match {
          case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
            val nextAtom = constructExprAnd(nextOpd)
            constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf) // captures left associativity
        }
      case Node(sym ::= _, List(op, rightNode)) 
        if Set('AndRest) contains sym =>
        rightNode match {
          case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
            val nextAtom = constructExprEq(nextOpd)
            constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf) // captures left associativity
        }
      case Node(sym ::= _, List(op, rightNode))
        if Set('EqRest) contains sym =>
        rightNode match {
          case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
            val nextAtom = constructExprComp(nextOpd)
            constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf) // captures left associativity
        }
      case Node(sym ::= _, List(op, rightNode))
        if Set('CompRest) contains sym =>
        rightNode match {
          case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
            val nextAtom = constructExprLowerOp(nextOpd)
            constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf) // captures left associativity
        }
      case Node(sym ::= _, List(op, rightNode))
        if Set('LowerOpRest) contains sym =>
        rightNode match {
          case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
            val nextAtom = constructExprHigherOp(nextOpd)
            constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf) // captures left associativity
        }
      case Node(sym ::= _, List(op, rightNode))
        if Set('HigherOpRest) contains sym =>
        rightNode match {
          case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
            val nextAtom = constructExprUnary(nextOpd)
            constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf) // captures left associativity
        }
    }
  }

}

