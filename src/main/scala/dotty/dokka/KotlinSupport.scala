package dotty.dokka


import scala.quoted._

object KotlinSupport:
  inline def create[T](inline objArg: Unit): T = 
    ${ impl('objArg, '[T]) }

  def impl[T](expr:Expr[Unit], tpe: Type[T] )(using ctx: QuoteContext) : Expr[T] =
    import ctx.tasty._
    expr.unseal match {
      case Inlined(None, Nil, Block((params, Literal(Constant(()))))) =>
        val paramsMap = params.flatMap {
          case ValDef(name, _, value) =>
            List(name -> value)
          case unsupported =>
            error(s"Unsupported nested expression: $unsupported", unsupported.pos)
            Nil      
        }.toMap
        println(paramsMap)

        val classSymbol = tpe.unseal.symbol
        if(!classSymbol.isClassDef) error(s"Expected class but got: ${classSymbol.show}", tpe.unseal.pos)
        val constructor = classSymbol.primaryConstructor.tree.asInstanceOf[DefDef]
        println(constructor.paramss)

        import ctx._
        '{ ??? }
      case other =>
         error("Bad tree expected ...", other.pos)
         import ctx._
         '{ ??? }  
    }
    