ProtobufType
============

`ProtobufType[T, MsgT]` provides conversion between Scala type `T` and Protobuf `MsgT <: Message`. Custom support for type `T` can be added with an implicit instance of `ProtobufField[T]`.

```scala  mdoc
import java.net.URI
case class Inner(long: Long, str: String, uri: URI)
case class Outer(inner: Inner)
val record = Outer(Inner(1L, "hello", URI.create("https://www.spotify.com")))

import magnolify.protobuf._

// Encode custom type URI as String
implicit val uriField = ProtobufField.from[String](URI.create)(_.toString)
 
// MyProto is a compiled Protobuf Message
val protobufType = ProtobufType[Outer, MyProto]
val proto: MyProto = protobufType.to(record)
val copy: Outer = protobufType.from(proto)
```

Enum like types map to Protobuf enums. See [EnumType](enums.md) for more details. An implicit instance from Java or Scala type to Protobuf enum must be provided.

```scala  mdoc
// Scala enum
object Color extends Enumeration {
  type Type = Value
  val Red, Green, Blue = Value
}

// Protobuf enum
// enum ColorProto {
//   RED = 0;
//   GREEN = 1;
//   BLUE = 2;
// }

import magnolify.shared._
implicit val efEnum = ProtobufField.enum[Color.Type, ColorProto]
```

Additional `ProtobufField[T]` instances for `Byte`, `Char`, `Short`, and `UnsafeEnum[T]` are available from `import magnolify.protobuf.unsafe._`. These conversions are unsafe due to potential overflow.

By default, nullable type `Option[T]` is not supported when `MsgT` is compiled with Protobuf 3 syntax. This is because Protobuf 3 does not offer a way to check if a field was set, and instead returns `0`, `""`, `false`, etc. when it was not. You can enable Protobuf 3 support for `Option[T]` by adding `import magnolify.protobuf.unsafe.Proto3Option._`. However with this, Scala `None`s will become `0/""/false` in Protobuf and come back as `Some(0/""/false)`.

To use a different field case format in target records, add an optional `CaseMapper` argument to `ProtobufType`. The following example maps `firstName` & `lastName` to `first_name` & `last_name`.

```scala  mdoc
import magnolify.shared.CaseMapper
import com.google.common.base.CaseFormat

case class LowerCamel(firstName: String, lastName: String)

val toSnakeCase = CaseFormat.LOWER_CAMEL.converterTo(CaseFormat.LOWER_UNDERSCORE).convert _
val protobufType = ProtobufType[LowerCamel, LowerHyphenProto](CaseMapper(toSnakeCase))
protobufType.to(LowerCamel("John", "Doe"))
```
