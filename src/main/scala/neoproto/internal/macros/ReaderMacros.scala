package neoproto.internal.macros

import scala.quoted.*
import neoproto.{ Reader, Result }

val foo = "bar"

object ReaderMacros:
  def readerForValueClassGen[A: Type](using Quotes) =
    import quotes.reflect.*
    val tpe = TypeRepr.of[A].dealias
    if (isCaseClass[A])
      getFields[A] match
        case field :: Nil =>
          val fieldName = field.name
          val fieldType = field.tree.asInstanceOf[ValDef].tpt.tpe.dealias

          (fieldType.asType, tpe.asType) match
            case ('[in], '[out]) =>
              val applyMethod = (i: Expr[in]) =>
                Apply(
                  fun = Select.unique(
                    Ident(TypeRepr.of[A].typeSymbol.companionModule.termRef),
                    "apply"
                  ),
                  args = List(i.asTerm)
                ).asExprOf[out]
              '{
                new Reader[in, out]:
                  override def read(in: in): Result[String, out] =
                    Result.succeed(${ applyMethod('in) })
              }
        case _            => report.errorAndAbort(s"${tpe.typeSymbol.name} is not a value class!")
    else
      report.errorAndAbort(s"Value class reader for ${tpe.typeSymbol.name} is not derivable!")

  def isCaseClass[A: Type](using Quotes) =
    import quotes.reflect.*
    TypeRepr.of[A].classSymbol.exists(_.flags.is(Flags.Case))

  def isSealedTraitOrEnum[A: Type](using Quotes) =
    import quotes.reflect.*
    TypeRepr.of[A].classSymbol.exists(_.flags.is(Flags.Sealed))

  def getFields[A: Type](using quotes: Quotes): List[quotes.reflect.Symbol] =
    quotes.reflect.TypeRepr.of[A].typeSymbol.caseFields

  def fieldDoesNotExistError(using
    q: Quotes
  )(fieldName: String, outType: q.reflect.TypeRepr, inType: q.reflect.TypeRepr) =
    import quotes.reflect.*
    report.errorAndAbort(
      "Field " +
        fieldName +
        " on type " +
        outType.typeSymbol.name +
        " does not exist on type " +
        inType.typeSymbol.name +
        "."
    )

  def caseDoesNotExist(using
    q: Quotes
  )(caseName: q.reflect.TypeRepr, outType: quotes.reflect.TypeRepr) =
    import quotes.reflect.*
    report.errorAndAbort(
      s"Case ${caseName.typeSymbol.name} does not exist on type ${outType.typeSymbol.name}."
    )

  def fieldHasUnassignableTypeError(using
    q: Quotes
  )(
    fieldName: String,
    outType: q.reflect.TypeRepr,
    outFieldType: q.reflect.TypeRepr,
    inType: q.reflect.TypeRepr,
    inFieldType: q.reflect.TypeRepr
  ) =
    import quotes.reflect.*
    report.errorAndAbort(
      "Field " +
        fieldName +
        " is unassignable because on type " +
        outType.typeSymbol.name +
        " it has type " +
        outFieldType.show +
        " but its corresponding field on type " +
        inType.typeSymbol.name +
        " has type " +
        inFieldType.show +
        "!"
    )

  def readerGenCaseClass[In: Type, Out: Type](using Quotes): Expr[Reader[In, Out]] =
    import quotes.reflect.*
    val inTpe  = TypeRepr.of[In].dealias
    val outTpe = TypeRepr.of[Out].dealias

    val inFields = getFields[In]

    val inFieldsByName = inFields.map { symbol =>
      symbol.name -> symbol
    }.toMap

    val outFields = getFields[Out]

    val resultCompanionTermRef = TypeRepr.of[Result].typeSymbol.companionModule.termRef

    def readImpl(in: Expr[In]): Expr[Result[String, Out]] =
      val results = outFields.map { outFieldSymbol =>
        val outFieldName  = outFieldSymbol.name
        val inFieldSymbol = inFieldsByName.getOrElse(
          outFieldName,
          fieldDoesNotExistError(outFieldName, outTpe, inTpe)
        )
        val inFieldType   = inFieldSymbol.tree.asInstanceOf[ValDef].tpt.tpe.dealias
        val outFieldType  = outFieldSymbol.tree.asInstanceOf[ValDef].tpt.tpe.dealias

        // Types are exactly equal. Translation is trivial. Just select the field.
        if inFieldType == outFieldType then
          outFieldType.asType match
            case '[t] =>
              val expr = '{ Result.succeed(${ Select(in.asTerm, inFieldSymbol).asExprOf[t] }) }
              (outFieldName, expr.asTerm, outFieldType)
        else if inFieldType <:< TypeRepr.of[Option[?]] then
          if (outFieldType <:< TypeRepr.of[Option[?]]) {
            (
              inFieldType.typeArgs.head.dealias.asType,
              outFieldType.typeArgs.head.dealias.asType
            ) match
              case ('[in], '[out]) =>
                readerGenOpt[in, out] match
                  case None         =>
                    fieldHasUnassignableTypeError(
                      outFieldName,
                      outTpe,
                      outFieldType,
                      inTpe,
                      inFieldType
                    )
                  case Some(reader) =>
                    val expr = '{
                      given Reader[in, out] = $reader
                      Reader
                        .liftTraversable[Option, in, out]
                        .read(
                          ${ Select(in.asTerm, inFieldSymbol).asExprOf[Option[in]] }
                        )
                    }
                    (outFieldName, expr.asTerm, outFieldType)

          } else {
            val inInnerFieldType = inFieldType.typeArgs.head.dealias

            val selectedOptional = Select(in.asTerm, inFieldSymbol)

            inInnerFieldType.dealias.asType match
              case '[in] =>
                val errorMessage = Expr(
                  s"Property $outFieldName on type ${outTpe.typeSymbol.name} is required but is missing from the input!"
                )

                val fromOption = '{
                  Result.fromOption(
                    ${ selectedOptional.asExprOf[Option[in]] },
                    ${ errorMessage }
                  )
                }

                if inInnerFieldType =:= outFieldType then
                  (outFieldName, fromOption.asTerm, outFieldType)
                else
                  outFieldType.dealias.asType match
                    case '[out] =>
                      val reader = readerGen[in, out]
                      val expr   = '{ ${ fromOption }.flatMap(opt => $reader.read(opt)) }
                      (outFieldName, expr.asTerm, outFieldType)
          }
        else
          (inFieldType.asType, outFieldType.asType) match
            case ('[in], '[out]) =>
              Expr.summon[Reader[in, out]] match
                case Some(reader) =>
                  val term = Apply(
                    fun = Select.unique(reader.asTerm, "read"),
                    args = List(Select(in.asTerm, inFieldSymbol))
                  )
                  (outFieldName, term, outFieldType)
                case None         =>
                  readerGenOpt[in, out] match
                    case None         =>
                      fieldHasUnassignableTypeError(
                        outFieldName,
                        outTpe,
                        outFieldType,
                        inTpe,
                        inFieldType
                      )
                    case Some(reader) =>
                      val term = Apply(
                        fun = Select.unique(reader.asTerm, "read"),
                        args = List(Select(in.asTerm, inFieldSymbol))
                      )
                      (outFieldName, term, outFieldType)
      }

      def recurse(
        terms: List[(String, Term, TypeRepr)],
        args: List[Term] = Nil
      ): Term =
        terms match
          case (name, term, tpe) :: next =>
            val newMethodSymbol = Symbol.newMethod(
              parent = Symbol.spliceOwner,
              name = s"${name}_anon_fun",
              tpe = MethodType(paramNames = List(name))(
                paramInfosExp = _ => List(tpe),
                resultTypeExp = _ => TypeRepr.of[Result[String, Out]]
              )
            )

            Select.overloaded(
              qualifier = term,
              name = "flatMap",
              targs = List(TypeRepr.of[String], outTpe),
              args = List(
                Block(
                  stats = List(
                    DefDef(
                      symbol = newMethodSymbol,
                      rhsFn = trees =>
                        val ident = trees.flatten.head.asInstanceOf[Ident]
                        Some(
                          recurse(next, args :+ ident)
                            .changeOwner(newMethodSymbol)
                        )
                    )
                  ),
                  expr = Closure(meth = Ident(newMethodSymbol.termRef), None)
                )
              )
            )
          case Nil                       =>
            Select.overloaded(
              Ident(resultCompanionTermRef),
              "succeed",
              List(outTpe),
              List(
                Apply(
                  fun = Select.unique(Ident(outTpe.typeSymbol.companionModule.termRef), "apply"),
                  args = args
                )
              )
            )

      val flatMaps = recurse(results)
      flatMaps.asExprOf[Result[String, Out]]

    val readerExpr = '{
      new Reader[In, Out]:
        override def read(in: In): Result[String, Out] =
          ${ readImpl('in) }
    }

    readerExpr

  def readerGenSealedTraitOrEnum[In: Type, Out: Type](using Quotes): Expr[Reader[In, Out]] =
    import quotes.reflect.*
    val inTpe  = TypeRepr.of[In].dealias
    val outTpe = TypeRepr.of[Out].dealias

    val inChildren = inTpe.typeSymbol.children.map { symbol =>
      symbol.name -> symbol.typeRef.dealias
    }.toMap

    val outChildren = outTpe.typeSymbol.children.map { symbol =>
      symbol.name -> symbol.typeRef.dealias
    }.toMap

    def readImpl(in: Expr[In]): Expr[Result[String, Out]] =
      // Pattern match on the input
      val matched = Match(
        selector = in.asTerm,
        cases = inChildren.map { case (name, inCaseType) =>
          val outCaseType = outChildren.getOrElse(name, caseDoesNotExist(inCaseType, outTpe))

          (inCaseType.dealias.asType, outCaseType.dealias.asType) match
            case ('[in], '[out]) =>
              val reader = readerGen[in, out]

              val bindSymbol =
                Symbol.newBind(
                  parent = Symbol.spliceOwner,
                  name = name,
                  flags = Flags.EmptyFlags,
                  tpe = inCaseType
                )

              val typeTree =
                if (inCaseType.typeSymbol.isClassDef) then TypeTree.ref(inCaseType.typeSymbol)
                else Singleton(ref = Ident(inCaseType.typeSymbol.termRef))

              val rhs =
                Apply(
                  fun = Select.unique(reader.asTerm, "read"),
                  args = List(Ident(bindSymbol.termRef))
                )

              CaseDef(
                pattern = Bind(
                  sym = bindSymbol,
                  pattern = Typed(Wildcard(), typeTree.changeOwner(bindSymbol))
                ),
                guard = None,
                rhs = rhs
              )

        }.toList
      )
      matched.asExprOf[Result[String, Out]]

    val readerExpr = '{
      new Reader[In, Out]:
        override def read(in: In): Result[String, Out] =
          ${ readImpl('in) }
    }

    readerExpr

  def readerGen[In: Type, Out: Type](using Quotes): Expr[Reader[In, Out]] =
    import quotes.reflect.*

    readerGenOpt[In, Out] match
      case Some(reader) => reader
      case None         =>
        report.errorAndAbort(
          s"Reader[${TypeRepr.of[In].show}, ${TypeRepr.of[Out].show}] is not derivable!"
        )

  def readerGenOpt[In: Type, Out: Type](using Quotes): Option[Expr[Reader[In, Out]]] =
    if isCaseClass[In] && isCaseClass[Out] then Some(readerGenCaseClass[In, Out])
    else if isSealedTraitOrEnum[In] && isSealedTraitOrEnum[Out] then
      Some(readerGenSealedTraitOrEnum[In, Out])
    else None
