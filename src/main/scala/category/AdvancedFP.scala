package category

object AdvancedFP {
  /* Advanced Functional Programming with Scala - Notes

  Copyright &copy; 2017 Fantasyland Institute of Learning. All rights reserved.*

   1. Mastering Functions

  A function is a mapping from one set, called a *domain*, to another set, called the *codomain*. A function associates every element in the domain with exactly one element in the codomain. In Scala, both domain and codomain are *types*.

  */
  val square : Int => Int = x => x * x

  square(2) // 4
  /*

  ## Higher-Order Functions

  A *higher-order* function is a function that *accepts* or *returns* a function.

  */
  trait List[A] {
    def filter(f: A => Boolean): List[A]
  }
  /*

  *Example*: `List[A].filter` is a higher-order function that accepts the function `A => Boolean` and returns the value `List[A]`.

  ## Combinators

  *Function combinators* are higher-order functions that accept and return functions.

    */
  trait ConfigReader {
    def readString(s: String): String
  }

  type Conf[A] = ConfigReader => A

  def string(name: String): Conf[String] = _.readString(name)

  def both[A,B](left: Conf[A], right: Conf[B]): Conf[(A, B)] = c => (left(c), right(c))
  /*

  *Example*: `both` is a combinator that takes two functions and returns a function.

    ## Polymorphic Functions

  A polymorphic function is one that is universally quantified over one or more type parameters. Scala has no support for polymorphic functions, but they can be emulated via polymorphic methods on traits. A trait modeling a polymorphic function usually has a single method called `apply`, so it can be applied with ordinary function application syntax.

  */
  case object identity {
    def apply[A](value: A): A = value
  }
  identity(3)   // 3
  identity("3") // "3"
  /*

  *Example*: This emulates a polymorphic function called `id`, which accepts one type parameter `A`, and a value of type `A`, and returns that same value.

  # 2. Mastering Types

  ## Type Theory 101

  A *type* is a compile-time description of a *set of values*. `Int` is the set of all integers between -2147483648 and 2147483647. Values *have* types, which is to say, they may be a member of the set of values they represent.

  */
  2 : Int
  /*

  *Example*: `2` is a member of the set of all `Int`. Equivalently, `2` has type `Int`.

    ## Algebraic Data Types

  An algebraic data type is a type formed by composing *product* and *sum* types.

    ### Product Type

  Product types are defined by a *Cartesian cross product* on 2 or more types.

  */
  type Point2D = (Int, Int)
  /*

  *Example*: A two-dimensional point is a product of a number and a number; each value has *both* an x-coordinate *and* a y-coordinate.

    #### Case Classes

  In Scala, case classes are the idiomatic representation of product types.

  */
  case class Person(name: String, age: Int)
  /*

  *Example*: A person has *both* a string (`name`) and an integer (`age`).

    ### Sum Types

  Sum types are defined by a *disjoint union* on 2 or more types.

    */
  type HttpResponse
  type RequestResult = Either[Error, HttpResponse]
  /*

  *Example*: An request result is a sum of `Error` and `HttpResponse`; each value is *either* an error *or* an HTTP response.

  #### Sealed Traits

  In Scala, sealed traits are the idiomatic representation of sum types (pre-Dotty).

    */
  sealed trait AddressType
  case object Home     extends AddressType
  case object Business extends AddressType
  /*

  *Example*: An `AddressType` is *either* a `Home` or a `Business`, but not both.

    ## Subtyping

  Type `A` is a subtype of `B` if $A \subseteq B$. In Scala, $A$ must *extend* $B$. The compiler allows subtypes to be used wherever a supertype is required.

  */
  sealed trait Shape {
    def width: Int
    def height: Int
  }

  case class Rectangle(corner: Point2D, width: Int, height: Int) extends Shape

  /*

  *Example*: A `Rectangle` is a subtype of `Shape`, because every `Rectangle` is a shape (but not every `Shape` is a `Rectangle`).

    ## Supertyping

  Type `A` is a supertype of `B` if $B \subseteq A$. In Scala, $B$ must *extend* $A$. The compiler allows supertypes to be used wherever a subtype is provided.

  In the previous example, `Shape` is a supertype of `Rectangle`, and a variable of type `Shape` may be assigned to a value of type `Rectangle`.


    ## Universals

  A universally quantified type defines a *category* (or *kind*) of types that are all parameterized by some arbitrary type. In Scala, type constructors (such as traits) and methods may be universally quantified, although methods do not have a type (they are a *component* of a type).

  ## Type Constructors

  A type constructor is a universally quantified type, which can be used to construct types.

    */

  sealed trait List[A] {
    def map[B](f: A => B): List[B]
  }
  case class Nil[A]() extends List[A]
  case class Cons[A](head: A, tail: List[A]) extends List[A]
  /*

  *Example*: `List` is type constructor, which defines a category of `List`-like types, including `List[Boolean]` (the type of lists of booleans). `List` is said to be *universally quantified* over its type variable `A`.

  ## Higher-Kinded Types

  ### Type-Level Functions

  Type constructors can be thought of as type-level functions, which accept types and return types. This interpretation is useful when reasoning about higher-kinded types.

  *Example*: `List` is a type-level function that accepts one type `A` (the type of its elements), and returns another type `List[A]`. If you pass `Boolean` to `List`, you get back `List[Boolean]`, the type of lists of boolean values.

    ### Kinds

  Kinds can be thought of as "the type of types".

    * `*` — The kind of types (the set of all types).
    * `* => *` — The kind of type-level functions that accept 1 type (the set of all type-level functions that accept 1 type). The type constructor `List` has kind `* => *`, represented as `_[_]` in Scala.
    * `[*, *] => *` — The kind of type-level functions that accept 2 types (the set of all type-level functions that accept 2 types). The type constructor `Either` has kind `[*, *] => *`, represented as `_[_, _]` in Scala.

    **Note**: Compare with the types of functions: `A => B`, `A => B => C`, `A => B => C => D`.

    ### Higher-Order Kinds

  Just like functions can be "higher-order", type constructors can be higher-order, too. Scala uses underscores to encode higher-order type constructors. The declaration `trait CollectionModule[Collection[_]]` specifies that `CollectionModule`'s type constructor requires a type constructor of kind `* -> *` (such as `List`).

    * `(* => *) => *` — The kind of type constructors that accept a type constructor of kind `* => *`. For example, `trait Functor[F[_]] { ... }` and `trait Monad[F[_]] { ... }`.

    **Note**: All higher-order kinds that return a type (`*`) are valid kinds.

  ## Existentials

  An existentially quantified type defines a type that depends on some definite but unknowable type. Existential types are useful for hiding type information that is not globally relevant.

  */
  trait ListMap[A] {
    type B
    val list : List[B]
    val mapf : B => A

    def run : List[A] = list.map(mapf)
  }
  /*

  *Example*: The type `ListMap[A]#B` is some definite type, but there is no way to know what that type is — it could be anything.

  ### Skolemization

  Every existential type can be encoded as a universal type. This process is called *skolemization*.

    */
  case class ListMap[B, A](list: List[B], mapf: B => A)

  trait ListMapInspector[A, Z] {
    def apply[B](value: ListMap[B, A]): Z
  }

  case class AnyListMap[A] {
    def apply[Z](value: ListMapInspector[A, Z]): Z
  }
  /*

  *Example*: Instead of using `ListMap` directly, we use `AnyListMap`, which allows us to inspect a `ListMap` but only if we can handle any type parameter for `B`.

    ## Type Lambdas

  Functions may be *partially applied* with the underscore operator; e.g. `zip(a, _)`. A type lambda is a way to partially apply a higher-kinded type, which yields another type constructor with fewer type parameters.

    Type lambdas are to type constructors as lambdas are to functions; the former are type/value expressions, while the latter are declarations.

  */
  ({type λ[α] = Either[String, α]})
  /*

  *Example*: This is the `Either` type, partially applied with a `String` as the first type parameter.

    **Note**: In many (but not all) cases, you can use type aliases instead of type lambdas (e.g. `type EitherString[A] = Either[String, A]`).

    ### Kind Projector

  *Kind Projector* is a common compiler plugin for Scala that provides easier syntax to create type lambdas. For example, the type lambda `({type λ[α] = Either[String, α]})#λ` can be represented with the syntax `Either[String, ?]`. Other syntax can create more complex type lambdas.

  <https://github.com/non/kind-projector>

  # 3. Mastering Type Classes

  A *type class* is a bundle of types and operations defined on them. Most type classes have *laws* that implementations are required to satisfy.

    */
  trait ShowRead[A] {
    def show(v: A): String
    def read(v: String): Either[String, A]
  }
  object ShowRead {
    def apply[A](implicit v: ShowRead[A]): ShowRead[A] = v
  }
  /*

  *Example*: The `ShowRead[A]` type class defines a way of "showing" a type `A` by rendering it to a string, and reading it by parsing it from a string (or producing an error message).

    * **Right Identity**

  */
  read(show(v)) == Right(v)
  /*
  * **Left Partial Identity**

  */
  read(v).map(show(_)).fold(_ => true, _ == v)
  /*

  ## Instances

  A *type class instance*, or simply *instance*, is an implementation of a type class for a given set of types. Such instances are usually made *implicit* so the compiler can thread them through functions that require them.

  */
  implicit val ShowReadString: ShowRead[String] = new ShowRead[String] {
    def show(v: String): String = v
    def read(v: String): Either[String, String] = Right(v)
  }

  ShowRead[String].show("foo") // foo
  /*

  ## Syntax

  Convenient syntax, sometimes called *extension methods*, can be added to types to make it easier to use type classes.

    */
  implicit class ShowOps[A: ShowRead](self: A) {
    def show: String = ShowRead[A].show(self)
  }
  implicit class ReadOps(self: String) {
    def read[A: ShowRead]: Either[String, A] = ShowRead[A].read(self)
  }

  true.show.read[Boolean] // Right(true)
  /*

  # 4. Mastering Functional Patterns

  ## Option, Either, Validation

  These types are commonly used to describe optionality and partiality.

  */
  sealed trait Maybe[A]
  final case class Just [A](value: A) extends Maybe[A]
  final case class Empty[A]()         extends Maybe[A]

  sealed trait \/[A, B]
  final case class -\/ [A, B](value: A) extends \/[A, B]
  final case class  \/-[B, B](value: B) extends \/[A, B]

  type Either[A, B] = A \/ B

  sealed trait Validation[A, B]
  final case class Failure[A, B](value: A) extends Validation[A, B]
  final case class Success[A, B](value: B) extends Validation[A, B]
  /*

  ## Semigroup, Monoid

  Semigroups allows combining two things of the same type into another thing of the same type. For example, addition forms a semigroup over integers. Monoids add the additional property of having an "zero" element, which you can append to a value without changing the value.

    */
  trait Semigroup[A] {
    def append(a1: A, a2: A): A
  }
  trait Monoid[A] extends Semigroup[A] {
    def zero: A
  }
  /*

  * **Associativity**

  */
  //append(a1, append(a2, a3)) == append(append(a1, a2), a3)
  /*
  * **Identity**

  */
  //append(a, zero) == a
  /*

  ## Functors

  A functor `F[_]` is a type constructor of kind `* -> *`. In the most general case, an `F[A]` represents a *description* of a computation that may halt, run forever, or produce 0 or more `A`'s.

    */
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(ab: A => B): F[B]
  }
  /*

  **Note**: Technically, this is a *covariant endofunctor*, and there are many other types of functors, including invariant and contravariant.

  * **Identity**

  */
  //map(fa)(identity) == fa
  /*
  * **Composition**

  */
  //map(map(fa)(ab))(...
}
