package amyc
package parsing

import utils._
import scala.io.Source
import java.io.File

// The lexer for Amy.
// Transforms an iterator coming from scala.io.Source to a stream of (Char, Position),
// then uses a functional approach to consume the stream.
object Lexer extends Pipeline[List[File], Stream[Token]] {
  import Tokens._

  /** Maps a string s to the corresponding keyword,
    * or None if it corresponds to no keyword
    */
  private def keywords(s: String): Option[Token] = s match {
    case "abstract" => Some(ABSTRACT())
    case "array"    => Some(ARRAY())            //      <===
    case "Boolean"  => Some(BOOLEAN())
    case "case"     => Some(CASE())
    case "class"    => Some(CLASS())
    case "def"      => Some(DEF())
    case "else"     => Some(ELSE())
    case "error"    => Some(ERROR())
    case "extends"  => Some(EXTENDS())
    case "false"    => Some(FALSE())
    case "if"       => Some(IF())
    case "Int"      => Some(INT())
    case "length"   => Some(LENGTH())           //      <===
    case "match"    => Some(MATCH())
    case "object"   => Some(OBJECT())
    case "String"   => Some(STRING())
    case "true"     => Some(TRUE())
    case "Unit"     => Some(UNIT())
    case "val"      => Some(VAL())
    case _          => None
  }

  private def lexFile(ctx: Context)(f: File): Stream[Token] = {
    import ctx.reporter._

    // Special character which represents the end of an input file
    val EndOfFile: Char = scala.Char.MaxValue

    val source = Source.fromFile(f)

    var inComment: Boolean = false
    var commentPos: Option[Position] = None

    // Useful type alias:
    // The input to the lexer will be a stream of characters,
    // along with their positions in the files
    type Input = (Char, Position)

    def mkPos(i: Int) = Position.fromFile(f, i)

    // The input to the lexer
    val inputStream: Stream[Input] =
      source.toStream.map(c => (c, mkPos(source.pos))) #::: Stream((EndOfFile, mkPos(source.pos)))

    /** Gets rid of whitespaces and comments and calls readToken to get the next token.
      * Returns the first token and the remaining input that did not get consumed
      */
    @scala.annotation.tailrec
    def nextToken(stream: Stream[Input]): (Token, Stream[Input]) = {
      require(stream.nonEmpty)

      val (currentChar, currentPos) #:: rest = stream

      // Use with care!
      def nextChar = rest.head._1

      if (inComment) {
      	// In a multi-line comment
      	if (currentChar == '*') {
      		if (rest.head._1 == '/') {
      			inComment = false
      			nextToken(rest.tail)
      		} else {
      			nextToken(rest)
      		}
      	} else if (currentChar == EndOfFile) {
      		inComment = false
      		ctx.reporter.error("Bad comment " ,commentPos.getOrElse(currentPos))
      		(BAD().setPos(commentPos.getOrElse(currentPos)),stream)
      	} else {
      		nextToken(rest)
      	}
      } else if (Character.isWhitespace(currentChar)) {
      	// Whitespaces
        nextToken(stream.dropWhile{ case (c, _) => Character.isWhitespace(c) } )
      } else if (currentChar == '/' && nextChar == '/') {
        // Single-line comment
        nextToken(stream.dropWhile{ case (c, _) => c != EndOfFile && c !=  '\n' && c != '\r'})
      } else if (currentChar == '/' && nextChar == '*') {
        // Multi-line comment
        val (commentCharacters, afterComment) = rest.tail.span { case (c, _) =>
        	c != EndOfFile && c !=  '*'
        }
        if (afterComment.head._1 == EndOfFile) {
        	ctx.reporter.error("Bad comment " ,commentPos.getOrElse(currentPos))
        	(BAD().setPos(currentPos),afterComment)
        } else if (afterComment.tail.head._1 == '/') {
        	nextToken(afterComment.tail.tail)
        } else {
        	commentPos = Some(currentPos)
        	inComment = true
        	nextToken(afterComment.tail)
        }
      } else if (currentChar == '\n' || currentChar == '\r') {
      	nextToken(rest)
      } else {
        readToken(stream)
      }
    }

    /** Reads the next token from the stream. Assumes no whitespace or comments at the beginning.
      * Returns the first token and the remaining input that did not get consumed.
      */
    def readToken(stream: Stream[Input]): (Token, Stream[Input]) = {
      require(stream.nonEmpty)

      val (currentChar, currentPos) #:: rest = stream

      // Use with care!
      def nextChar = rest.head._1

      // Returns input token with correct position and uses up one character of the stream
      def useOne(t: Token) = (t.setPos(currentPos), rest)
      // Returns input token with correct position and uses up two characters of the stream
      def useTwo(t: Token) = (t.setPos(currentPos), rest.tail)

      currentChar match {
        case `EndOfFile` => useOne(EOF())

        // Reserved word or Identifier
        case _ if Character.isLetter(currentChar) =>
        	val (wordCharacters, afterWord) = stream.span { case (ch, _) =>
            Character.isLetterOrDigit(ch) || ch == '_'
          	}
         	val word = wordCharacters.map(_._1).mkString
          	// Hint: Decide if it's a letter or reserved word (use our infrastructure!),
          	// and return the correct token, along with the remaining input stream.
          	// Make sure you set the correct position for the token.
          	val keyword: Option[Token] = keywords(word)
          	val token = keyword.getOrElse(ID(word))
          	(token.setPos(currentPos), afterWord)  // afterWord == stream.drop(word.lenght)

        // Int literal
        case _ if Character.isDigit(currentChar) =>
          	// Hint: Use a strategy similar to the previous example.
          	// Make sure you fail for integers that do not fit 32 bits.
          	val (intCharacter, afterInt) = stream.span { case (ch, _) =>
				Character.isDigit(ch)
          	}
          	val intString = intCharacter.map(_._1).mkString
          	if (BigInt(intString).isValidInt) {
          		val integer = intString.toInt
          		(INTLIT(integer).setPos(currentPos), afterInt)
          	} else {
          		ctx.reporter.error("Bad token: " ++ intString ,currentPos)
          		(BAD().setPos(currentPos), afterInt)
          	}

        // String literal
        case '"' =>
        	val (stringCharacters, afterString) = rest.span { case (ch, _) =>
				ch != '\n' && ch != '\r' && ch != '"' && ch != EndOfFile
          	}
          	if (afterString.head._1 == '"') {
          		val string = stringCharacters.map(_._1).mkString
          		(STRINGLIT(string).setPos(currentPos), afterString.tail)
          	} else if (afterString.head._1 == '\n' || afterString.head._1 == '\r') {
          		ctx.reporter.error("Bad token: " ++ stringCharacters ,currentPos)
          		(BAD().setPos(currentPos), afterString.tail)
          	} else {
          		ctx.reporter.error("Bad token: " ++ stringCharacters ,currentPos)
          		(BAD().setPos(currentPos), afterString)
          	}

        case ';' =>
        	useOne(SEMICOLON())

        case '+' =>
        	if (nextChar == '+') {
        		useTwo(CONCAT())
        	} else {
        		useOne(PLUS())
        	}

        case '-' =>
        	useOne(MINUS())

        case '*' =>
        	useOne(TIMES())

        case '/' =>
        	useOne(DIV())

        case '%' =>
        	useOne(MOD())

        case '<' =>
        	if (nextChar == '=') {
        		useTwo(LESSEQUALS())
        	} else {
        		useOne(LESSTHAN())
        	}

        case '&' =>
        	if (nextChar == '&') {
        		useTwo(AND())
        	} else {
        		ctx.reporter.error("Bad token: &" ,currentPos)
        		useOne(BAD())
        	}

        case '|' =>
        	if (nextChar == '|') {
        		useTwo(OR())
        	} else {
        		ctx.reporter.error("Bad token: |" ,currentPos)
        		useOne(BAD())
        	}

        case '=' =>
        	if (nextChar == '=') {
        		useTwo(EQUALS())
        	} else if (nextChar == '>') {
        		useTwo(RARROW())
        	} else {
        		useOne(EQSIGN())
        	}

        case '!' =>
        	useOne(BANG())

        case '{' =>
        	useOne(LBRACE())

        case '}' =>
        	useOne(RBRACE())

        case '(' =>
        	useOne(LPAREN())

        case ')' =>
        	useOne(RPAREN())

        case '[' =>             //      <===
            useOne(LBRACKET())

        case ']' =>             //      <===
            useOne(RBRACKET())

        case ',' =>
        	useOne(COMMA())

        case ':' =>
        	useOne(COLON())

        case '.' =>
        	useOne(DOT())

        case '_' =>
        	useOne(UNDERSCORE())

        case _ =>
        	ctx.reporter.error("Bad token: " + currentChar,currentPos)
        	useOne(BAD())
      }
    }

    // To lex a file, call nextToken() until it returns the empty Stream as "rest"
    def tokenStream(s: Stream[Input]): Stream[Token] = {
      if (s.isEmpty) Stream()
      else {
        val (token, rest) = nextToken(s)
        token #:: tokenStream(rest)
      }
    }

    tokenStream(inputStream)
  }

  // Lexing all input files means putting the tokens from each file one after the other
  def run(ctx: Context)(files: List[File]): Stream[Token] = {
    files.toStream flatMap lexFile(ctx)
  }
}

/** Extracts all tokens from input and displays them */
object DisplayTokens extends Pipeline[Stream[Token], Unit] {
  def run(ctx: Context)(tokens: Stream[Token]): Unit = {
    tokens.toList foreach { t => println(s"$t(${t.position.withoutFile})") }
  }
}
