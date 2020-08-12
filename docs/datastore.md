EntityType
==========

`EntityType[T]` provides conversion between Scala type `T` and Datastore `Entity`. Custom support for type `T` can be added with an implicit instance of `EntityField[T]`.

```scala
import java.net.URI
case class Inner(long: Long, str: String, uri: URI)
case class Outer(inner: Inner)
val record = Outer(Inner(1L, "hello", URI.create("https://www.spotify.com")))

import magnolify.datastore._
import com.google.datastore.v1.{Entiyt, Entity.Builder}

// Encode custom type URI as String
implicit val uriField = EntityField.from[String](URI.create)(_.toString)

val entityType = EntityType[Outer]
val entityBuilder: Entity.Builder = entityType.to(record)
val copy: Outer = entityType.from(entityBuilder.build)
```

Additional `EntityField[T]` instances for `Byte`, `Char`, `Short`, `Int`, `Float`, Java `Enum` and Scala `Enumeration` are available from `import magnolify.datastore.unsafe._`. These conversions are unsafe due to potential overflow or encoding errors.

To set a field as key, annotate the field with `key` annotation.

```scala
// Leave projectId as empty, use current package as namespaceId and class name "Record" as kind
case class Record(@key k: String, v: Long)

// Custom projectId, namespaceId and kind.
case class Record(@key("my-project", "com.spotify", "MyKind") k: String, v: Long)

// Encode custom key type ByteString as String
case class ByteStringKey(@key k: ByteString)
implicit val kfByteString = KeyField.at[ByteString](_.toStringUtf8)

// Encode custom key type RecordKey as String
case class RecordKey(s: String, l: Long)
case class NestedKey(@key k: RecordKey)
implicit val kfRecord = KeyField.at[RecordKey](r => r.s + r.l)
```

To exclude a property from indexes, annotate the field with `excludeFromIndexes` annotation.

```scala
case class Record(@excludeFromIndexes i: Int, @excludeFromIndexes(true) s: String)
```

To use a different field case format in target records, add an optional `CaseMapper` argument to `EntityType`. The following example maps `firstName` & `lastName` to `first_name` & `last_name`.

```scala
import magnolify.shared.CaseMapper
import com.google.common.base.CaseFormat

case class LowerCamel(firstName: String, lastName: String)

val toSnakeCase = CaseFormat.LOWER_CAMEL.converterTo(CaseFormat.LOWER_HYPHEN).convert _
val entityType = EntityType[LowerCamel](CaseMapper(toSnakeCase))
entityType.to(LowerCamel("John", "Doe"))
```

`CaseMapper` supports enums too.

```scala
object Color extends Enumeration {
  type Type = Value
  val Red, Green, Blue = Value
}

import magnolify.shared._
// Encode as ["red", "green", "blue"]
implicit val enumType = EnumType[Color.Type].map(CaseMapper(_.toLowerCase))
```
