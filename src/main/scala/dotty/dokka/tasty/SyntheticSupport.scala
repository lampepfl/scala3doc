package dotty.dokka.tasty

trait SyntheticsSupport:
  self: TastyParser =>

  import reflect._

  def isValidPos(pos: Position) = 
    try
      val a = (pos.exists, pos.start, pos.end)
      pos.exists && pos.start != pos.end
    catch 
      case scala.util.control.NonFatal(_) =>
        false  

  def constructorWithoutParamLists(c: ClassDef): Boolean =
    !isValidPos(c.constructor.pos)  || {
      val end = c.constructor.pos.end
      val typesEnd =  c.constructor.typeParams.lastOption.fold(end - 1)(_.pos.end)
      val classDefTree = c.constructor.show
      c.constructor.typeParams.nonEmpty && end <= typesEnd + 1
    }

  def isSyntheticFunc(c: Symbol): Boolean =
    c.flags.is(Flags.Synthetic) || c.flags.is(Flags.FieldAccessor)  

  def isSyntheticField(c: Symbol) = 
    c.flags.is(Flags.CaseAcessor) || c.flags.is(Flags.Private)