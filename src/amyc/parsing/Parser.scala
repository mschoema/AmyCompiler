package amyc
package parsing

import grammarcomp.grammar.CFGrammar._
import grammarcomp.grammar.GrammarDSL._
import grammarcomp.grammar.GrammarUtils.InLL1
import grammarcomp.grammar._
import grammarcomp.parsing._
import amyc.utils._
import ast.NominalTreeModule._
import Tokens._

// The parser for Amy
// Absorbs tokens from the Lexer and then uses grammarcomp to generate parse trees.
// Defines two different grammars, a naive one which does not obey operator precedence (for demonstration purposes)
// and an LL1 grammar that implements the true syntax of Amy
object Parser extends Pipeline[Stream[Token], Program] {

  /* This grammar does not implement the correct syntax of Amy and is not LL1
   * It is given as an example
   */
  val amyGrammar = Grammar('Program, List[Rules[Token]](
    'Program ::= 'ModuleDefs,
    'ModuleDefs ::= 'ModuleDef ~ 'ModuleDefs | epsilon(),
    'ModuleDef ::= OBJECT() ~ 'Id ~ LBRACE() ~ 'Definitions ~ 'OptExpr ~ RBRACE() ~ EOF(),
    'Definitions ::= 'Definition ~ 'Definitions | epsilon(),
    'Definition ::= 'AbstractClassDef | 'CaseClassDef | 'FunDef,
    'AbstractClassDef ::= ABSTRACT() ~ CLASS() ~ 'Id,
    'CaseClassDef ::= CASE() ~ CLASS() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ EXTENDS() ~ 'Id,
    'FunDef ::= DEF() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'Expr ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Id ~ COLON() ~ 'Type,
    'OptExpr ::= 'Expr | epsilon(),
    'Type ::= INT() | STRING() | BOOLEAN() | UNIT() | 'QName,
    'QName ::= 'Id | 'Id ~ DOT() ~ 'Id,
    'Expr ::= 'Id | 'Literal | 'Expr ~ 'BinOp ~ 'Expr | BANG() ~ 'Expr | MINUS() ~ 'Expr |
              'QName ~ LPAREN() ~ 'Args ~ RPAREN() | 'Expr ~ SEMICOLON() ~ 'Expr |
              VAL() ~ 'Param ~ EQSIGN() ~ 'Expr ~ SEMICOLON() ~ 'Expr |
              IF() ~ LPAREN() ~ 'Expr ~ RPAREN() ~ LBRACE() ~ 'Expr ~ RBRACE() ~ ELSE() ~ LBRACE() ~ 'Expr ~ RBRACE() |
              'Expr ~ MATCH() ~ LBRACE() ~ 'Cases ~ RBRACE() |
              ERROR() ~ LPAREN() ~ 'Expr ~ RPAREN() |
              LPAREN() ~ 'Expr ~ RPAREN(),
    'Literal ::= TRUE() | FALSE() | LPAREN() ~ RPAREN() | INTLITSENT | STRINGLITSENT,
    'BinOp ::= PLUS() | MINUS() | TIMES() | DIV() | MOD() | LESSTHAN() | LESSEQUALS() |
               AND() | OR() | EQUALS() | CONCAT(),
    'Cases ::= 'Case | 'Case ~ 'Cases,
    'Case ::= CASE() ~ 'Pattern ~ RARROW() ~ 'Expr,
    'Pattern ::= UNDERSCORE() | 'Literal | 'Id | 'QName ~ LPAREN() ~ 'Patterns ~ RPAREN(),
    'Patterns ::= epsilon() | 'Pattern ~ 'PatternList,
    'PatternList ::= epsilon() | COMMA() ~ 'Pattern ~ 'PatternList,
    'Args ::= epsilon() | 'Expr ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expr ~ 'ExprList,
    'Id ::= IDSENT
  ))

  // TODO: Write a grammar that implements the correct syntax of Amy and is LL1.
  // You can start from the example above and work your way from there.
  // Make sure you use the warning (see `run` below) that tells you which part is not in LL1.
  lazy val amyGrammarLL1 = Grammar('Program, List[Rules[Token]](
    'Program ::= 'ModuleDefs,
    'ModuleDefs ::= 'ModuleDef ~ 'ModuleDefs |
              epsilon(),
    'ModuleDef ::= OBJECT() ~ 'Id ~ LBRACE() ~ 'Definitions ~ 'OptExpr ~ RBRACE() ~ EOF(),
    'Definitions ::= 'Definition ~ 'Definitions |
              epsilon(),
    'Definition ::= 'AbstractClassDef |
              'CaseClassDef |
              'FunDef,
    'AbstractClassDef ::= ABSTRACT() ~ CLASS() ~ 'Id,
    'CaseClassDef ::= CASE() ~ CLASS() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ EXTENDS() ~ 'Id,
    'FunDef ::= DEF() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'Expr ~ RBRACE(),
    'Params ::= 'Param ~ 'ParamList |
              epsilon(),
    'ParamList ::= COMMA() ~ 'Param ~ 'ParamList |
              epsilon(),
    'Param ::= 'Id ~ COLON() ~ 'Type,
    'OptExpr ::= 'Expr |
              epsilon(),
    'Type ::= INT() | STRING() | BOOLEAN() | UNIT() | 'QName | ARRAY() ~ LBRACKET() ~ INTLITSENT ~ RBRACKET() | LBRACKET() ~ INTLITSENT ~ DOT() ~ DOT() ~ INTLITSENT ~ RBRACKET(),  //    <===
    'QName ::= 'Id ~ 'QNameRest,
    'QNameRest ::= DOT() ~ 'Id |
              epsilon(),
    'Expr ::= VAL() ~ 'Param ~ EQSIGN() ~ 'ExprMatch ~ SEMICOLON() ~ 'Expr |
              'ExprMatch ~ 'ExprRest,
    'ExprRest ::= SEMICOLON() ~ 'Expr |
              epsilon(),
    'ExprMatch ::= 'ExprOr ~ 'MatchRest,
    'MatchRest ::= MATCH() ~ LBRACE() ~ 'Case ~ 'Cases ~ RBRACE() |
              epsilon(),
    'ExprOr ::= 'ExprAnd ~ 'OrRest,
    'OrRest ::= OR() ~ 'ExprOr |
              epsilon(),
    'ExprAnd ::= 'ExprEq ~ 'AndRest,
    'AndRest ::= AND() ~ 'ExprAnd |
              epsilon(),
    'ExprEq ::= 'ExprComp ~ 'EqRest,
    'EqRest ::= EQUALS() ~ 'ExprEq |
              epsilon(),
    'ExprComp ::= 'ExprLowerOp ~ 'CompRest,
    'CompRest ::= LESSTHAN() ~ 'ExprComp |
              LESSEQUALS() ~ 'ExprComp |
              epsilon(),
    'ExprLowerOp ::= 'ExprHigherOp ~ 'LowerOpRest,
    'LowerOpRest ::= PLUS() ~ 'ExprLowerOp |
              MINUS() ~ 'ExprLowerOp |
              CONCAT() ~ 'ExprLowerOp |
              epsilon(),
    'ExprHigherOp ::= 'ExprUnary ~ 'HigherOpRest,
    'HigherOpRest ::= TIMES() ~ 'ExprHigherOp |
              DIV() ~ 'ExprHigherOp |
              MOD() ~ 'ExprHigherOp |
              epsilon(),
    'ExprUnary ::= BANG() ~ 'ExprTop |
              MINUS() ~ 'ExprTop |
              'ExprTop,
    'ExprTop ::= 'Id ~ 'QNameCallOrArr |        //    <===
              'Literal |
              IF() ~ LPAREN() ~ 'Expr ~ RPAREN() ~ LBRACE() ~ 'Expr ~ RBRACE() ~ ELSE() ~ LBRACE() ~ 'Expr ~ RBRACE() |
              ERROR() ~ LPAREN() ~ 'Expr ~ RPAREN() |
              LPAREN() ~ 'ParExpr,
    'ParExpr ::= 'Expr ~ RPAREN() |
              RPAREN(),
    'QNameCallOrArr ::= 'QNameCall |
              LBRACKET() ~ 'Expr ~ RBRACKET(),  //    <===
    'QNameCall ::= DOT() ~ 'QNameCallRest |
              LPAREN() ~ 'Args ~ RPAREN() |
              epsilon,
    'QNameCallRest ::= 'Id ~ LPAREN() ~ 'Args ~ RPAREN() |
              LENGTH(),
    'Literal ::= TRUE() | FALSE() | INTLITSENT | STRINGLITSENT | LBRACKET() ~ INTLITSENT ~ 'ArrayElems ~ RBRACKET(),  //     <===
    'ArrayElems ::= COMMA() ~ INTLITSENT ~ 'ArrayElems |  //    <===
              epsilon(),
    'Cases ::= 'Case ~ 'Cases |
              epsilon(),
    'Case ::= CASE() ~ 'Pattern ~ RARROW() ~ 'Expr,
    'Pattern ::= UNDERSCORE() | 'Literal | 'Id ~ 'IdAfter | LPAREN() ~ RPAREN(),
    'IdAfter ::= DOT() ~ 'Id ~ LPAREN() ~ 'Patterns ~ RPAREN() |
              LPAREN() ~ 'Patterns ~ RPAREN() |
              epsilon(),
    'Patterns ::= epsilon() | 'Pattern ~ 'PatternList,
    'PatternList ::= epsilon() | COMMA() ~ 'Pattern ~ 'PatternList,
    'Args ::= epsilon() | 'Expr ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expr ~ 'ExprList,
    'Id ::= IDSENT

  ))

  def run(ctx: Context)(tokens: Stream[Token]): Program = {
    // TODO: Switch to LL1 when you are ready
    val (grammar, constructor) = (amyGrammarLL1, new ASTConstructorLL1(ctx))
    //val (grammar, constructor) = (amyGrammar, new ASTConstructor)

    import ctx.reporter._
    implicit val gc = new GlobalContext()
    implicit val pc = new ParseContext()

    GrammarUtils.isLL1WithFeedback(grammar) match {
      case InLL1() =>
        // info("Grammar is in LL1")
      case other =>
        warning(other)
    }

    val feedback = ParseTreeUtils.parseWithTrees(grammar, tokens.toList)
    feedback match {
      case s: Success[Token] =>
        constructor.constructProgram(s.parseTrees.head)
      case err@LL1Error(_, Some(tok)) =>
        fatal(s"Parsing failed: $err", tok.obj.position)
      case err =>
        fatal(s"Parsing failed: $err")
    }
  }

}
