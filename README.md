# true-delta

A FP solution to find delta in Scala.

## Usage

The documentation isn't complete. 
It is in progress.

## Find the fields that are updated

```scala

    import com.thaj.delta._
    
    case class Inner3(field: String, id: String)
    case class Inner2(something: String, id: String, inner3: Inner3)
    
    case class Outer(inner2: Inner2)
  
  
      // This can be derived automatically!
    implicit val idInner2: HasId[Inner2] = _.id
    implicit val idInner3: HasId[Inner3] = _.id
      
    val s1 = Outer(Inner2("abc", "inner2id", Inner3("kkk", "inner3id")))
    val s2 = Outer(Inner2("def", "inner2id", Inner3("bbb", "inner3id")))
    
    val meta = FindDeltaMeta[Outer](s1, s2) 
    
    // result is:
     
    List(
      UpdateInfo(inner2.inner2id.something, update, abc, def),
      UpdateInfo(inner2.inner2id.inner3.inner3id.field, update, kkk, bbb ),
    )
  
```

## Finding the fields that are deleted/created.

```scala

    
    case class Inner3(field: String, id: String)
    case class Inner2(something: String, id: String, inner3: Inner3)
  
    case class Outer(inner2: Inner2)
  
  
    // This can be derived automatically!
    implicit val idInner2: HasId[Inner2] = _.id
    implicit val idInner3: HasId[Inner3] = _.id
    
   val s11 = Outer(Inner2("abc", "inner2id", Inner3("kkk", "inner3id1")))
   val s22 = Outer(Inner2("abc", "inner2id", Inner3("bbb", "inner3id2")))
   
    val meta = FindDeltaMeta[Outer](s1, s2) 
       
   // result is:
        
   List(
      UpdateInfo(inner2.inner3id1, delete, Inner3(kkk, inner3id1),),
      UpdateInfo(inner2.inner3id2, create,,Inner3(bbb, inner3id2)),
   )

```

## What if there is list?

```scala

  case class Inner2(something: String, id: String)
  case class Complex(inner2: List[Inner2])
  
  val s1 = 
    Outer(List(
       Inner2("abc", "inner2id1"), 
       Inner2("xyz", "inner2id2"),
    ))
    
  val s2 =
    Outer(List(
       Inner2("abc-changed", "inner2id1")
       Inner2("xyz", "inner2id3")
    ))  
    
  val meta = FindDeltaMeta[Outer](s1, s2) 
  
 // result is:
        
   List(
      UpdateInfo(inner2.inner2id1.something, update, abc, abc-changed),
      UpdateInfo(inner2.inner2id2, delete,  Inner2(xyz, inner2id2),),
      UpdateInfo(inner2.innder2id3, create,, Inner2(xyz, inner2id3)),
   )
```       

## Options ? AnyVal ?

Take a look at FindDeltaMetaSpec test cases that covers most of the scenarios. 



## Type safe ?

Please note that you can call `Delta[A]` to get a type safe delta, that in turn shows the meta of individual fields recursively.
Please note, `UpdateInfo` is just a string representation which I used in README to make it look nice ! 
Under the hood, `UpateInfo` == `Meta` which is just a `list` of `cats.IoR` if you think about it.
More on this later.

```scala

scala> import cats.implicits._
import cats.implicits._

scala> import com.thaj.delta._
import com.thaj.delta._

scala> case class X(name: String, value: String)
defined class X

scala> implicit val hasId: HasId[X] = _.name
hasId: com.thaj.delta.HasId[X] = $$Lambda$3954/16197430@5c5ec7e2

scala> List(X("id", "value"), X("id2", "value2"))
res6: List[X] = List(X(id,value), X(id2,value2))

scala> List(X("id", "valuechanged"), X("id2", "value2changed"))
res7: List[X] = List(X(id,valuechanged), X(id2,value2changed))

scala> res6.diffWith(res7)
res10: List[com.thaj.delta.Delta.DeltaOp[X]] =
    List(
      Update(
        X(id, value),
        X(id, valuechanged),
        List(UpdateInfo(value, update, value, valuechanged, List()))
      ),
      Update(
        X(id2, value2),
        X(id2, value2changed),
        List(UpdateInfo(value, update, value2, value2changed, List()))
      )
    )


```

## Structure of Keys

The structure of the Key in `UpdateInfo` is `field1.<field1-key>.<field2>.<field2-key>.field3` where `field2` is a member in  `field1` and `field3` is a member of `field2`.

## Override behavior ?
Yes, we can override the comparison behavior.

Under the hood, `FindDeltaMeta` (or `Delta`) considers primitives/value-classes to have instances of `Primitive` type class. All that we need to do is create an instance of the same 
with custom equality operation. It is easy and looks like this:

```scala
scala>  case class Yaml(x: String) extends AnyVal
defined class Yaml

scala>  case class Outer(yaml: Yaml)
defined class Outer

scala>  val outer1 = Outer(Yaml("input"))
outer1: Outer = Outer(Yaml(input))

scala>  val outer2 = Outer(Yaml("inputjunk"))
outer2: Outer = Outer(Yaml(inputjunk))

scala> FindDeltaMeta[Outer].apply(outer1, outer2) // returns non-empty updates.
res11: com.thaj.delta.DeltaMeta.Meta = List(UpdateInfo(yaml,update,input,inputjunk,List()))

// overriding it now:

scala> implicit val primitive: Primitive[Yaml] = Primitive.fromEquality(
  (a, b) => a.x.replace("junk", "") === b.x.replace("junk", "")
)
primitive: com.thaj.delta.Primitive[Yaml] = com.thaj.delta.Primitive$$anon$2@7c2c4cd

scala> FindDeltaMeta[Outer].apply(outer1, outer2)
res18: com.thaj.delta.DeltaMeta.Meta = List()

```
 

 
