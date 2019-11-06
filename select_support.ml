
class select_support rank_sup =
object(self)
  val rank_sup = rank_sup
  method select1 i = ()
  method select0 i = ()
  method overhead = rank_sup#overhead
