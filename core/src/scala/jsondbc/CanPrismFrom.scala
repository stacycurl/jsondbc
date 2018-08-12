package jsondbc

import monocle.Prism


case class CanPrismFrom[From, Elem, To](prism: Prism[From, To]) {
  def toList: CanPrismFrom[List[From], Elem, List[To]] =
    CanPrismFrom(Prism[List[From], List[To]](la ⇒ Some(la.flatMap(prism.getOption)))(_.map(prism.reverseGet)))

  // def updateValues[W](f: V ⇒ Option[W]): Map[K, W] = self.flatMap(kv ⇒ f(kv._2).map(kv._1 → _))
  def toMap[K]: CanPrismFrom[Map[K, From], Elem, Map[K, To]] = CanPrismFrom(Prism[Map[K, From], Map[K, To]](mapKA ⇒ {
    Some(for {
      (k, v) <- mapKA
      to    <- prism.getOption(v)
    } yield k -> to)
  })((mapKB: Map[K, To]) ⇒ {
    mapKB.map {
      case (k, v) => k -> prism.reverseGet(v)
    }
  }))
}

object CanPrismFrom {
  implicit def cpfJsonToBoolean[Json](implicit spi: SPI[Json]):    CanPrismFrom[Json, Boolean,    Boolean]    = apply(spi.jBoolean)
  implicit def cpfJsonToString[Json](implicit spi: SPI[Json]):     CanPrismFrom[Json, String,     String]     = CanPrismFrom(spi.jString)
  implicit def cpfJsonToJsonArray[Json](implicit spi: SPI[Json]):  CanPrismFrom[Json, List[Json], List[Json]] = CanPrismFrom(spi.jArray)
  implicit def cpfJsonToBigDecimal[Json](implicit spi: SPI[Json]): CanPrismFrom[Json, BigDecimal, BigDecimal] = CanPrismFrom(spi.jBigDecimal)
  implicit def cpfJsonToBigInt[Json](implicit spi: SPI[Json]):     CanPrismFrom[Json, BigInt,     BigInt]     = CanPrismFrom(spi.jBigInt)
  implicit def cpfJsonToLong[Json](implicit spi: SPI[Json]):       CanPrismFrom[Json, Long,       Long]       = CanPrismFrom(spi.jLong)
  implicit def cpfJsonToInt[Json](implicit spi: SPI[Json]):        CanPrismFrom[Json, Int,        Int]        = CanPrismFrom(spi.jInt)
  implicit def cpfJsonToShort[Json](implicit spi: SPI[Json]):      CanPrismFrom[Json, Short,      Short]      = CanPrismFrom(spi.jShort)
  implicit def cpfJsonToByte[Json](implicit spi: SPI[Json]):       CanPrismFrom[Json, Byte,       Byte]       = CanPrismFrom(spi.jByte)

  implicit def cpfl[From, Elem, To](
    implicit cpf: CanPrismFrom[From, Elem, To]
  ): CanPrismFrom[List[From], Elem, List[To]] = cpf.toList

  implicit def cpfm[From, Elem, To](
    implicit cpf: CanPrismFrom[From, Elem, To]
  ): CanPrismFrom[Map[String, From], Elem, Map[String, To]] = cpf.toMap
}
