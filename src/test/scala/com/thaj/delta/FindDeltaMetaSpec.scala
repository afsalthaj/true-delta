package com.thaj.delta

import cats.data.Ior
import org.specs2.{ScalaCheck, Specification}
import cats.implicits._
import com.thaj.delta.DeltaMeta.Meta
import enumeratum.{Enum, EnumEntry}

final case class Location(value: String) extends AnyVal

class FindDeltaMetaSpec extends Specification with ScalaCheck {
  def is =
    s2"""
      Works for simple case class with no fields $simpleTestOfCaseClassWithNoValue
      Works for simple case class with a String $simpleTestOfCaseClass
      Works for simple case class with an Int $simpleTestForInt
      Works for simple case class with a Double $simpleTestForDouble
      Works for simple case class with a Long $simpleTestForLong
      Works for simple nested case classes with no value $simpleTestOfNestedCaseClassWithOnlyNames
      If there is a nested case class in the School, the inner case class should have a HasName instance and
        will be compared only with an inner class having the same name $simpleNestedCaseClassWithSameNames
      If there is a nested case class in the School, the inner case class having different names
        on left right will be tracked as new or deleted. $simpleNestedCaseClassWithDifferentNames
      Multiple inner case classes with same names, such that changed values
        will be tracked correctly $multipleInnerCaseClassWithSameNames
      Multiple inner case classes with different names, such that new and deleted
        will be tracked correctly $multipleInnerCaseClassWithDifferentNames
      Multiple inner case classes with same and different names, such that new, deleted and changed
        will be tracked correctly $multipleInnerCaseClassWithDifferentNamesAndSameNames
      2 level nesting with same names  will be compared correctly $nestedToNestedInnerClassWithSameNames
      2 level nesting with different names will be compared correctly $nestedToNestedInnerClassWithDifferentNames
      2 level nesting with different names and same names
        will be compared correctly $nestedToNestedInnerClassWithSameAndDifferentNames
      3 level nesting with different names and same names
        will be compared correctly $nestedToNestedInnerClassWithSameAndDifferentNamesWith3Levels
      4 level nesting with different names and same names with 4th level as an AnyVal
        will be compared correctly  $nestedToNestedInnerClassWithSameAndDifferentNamesWith4LevelsWithAnyVal
      4 level nesting comparison will be short circuited if first level itself is different
         $shortCircuitIfFirstLevelIsDifferentWithExtraFields
      Works for case classes with inner lists empty $simpleListThatAreEmpty
      Works for case classes with inner list of primitives $simpleTestForListOfPrimitives
      Works for case classes with inner list of case classes $simpleTestForListOfProducts
      Works for 2 level list of products $twoLevelListOfProducts
      Works for 2 level list of products where everything is a list $twoLevelListOfListofProducts
      Works for case classes with optional fields in it, where type is some primitive $simpleTestForOptionWithPrimnitives
      Works for case classes with optional case classes inside it $simpleTestForOption
      Works for case classes with optional case classes in 2 level nesting $simpleTestForOptionWith2Levels
      Works for case class with Enum Entrys $simpleTestWithEnumeratumEnumEntries
      Works for nested case class that ends up in an Enum Entry $simpleTestWithEnumeratumEnumEntriesNested
      Works if equality instance of anyval classes are overriden $overrideAnyValsEquality
      """

  private def simpleTestOfCaseClassWithNoValue = {
    case class School()

    val s1 = School()
    val s2 = School()

    FindDeltaMeta[School].apply(s1, s2) must_=== Meta.empty
  }

  private def simpleTestOfCaseClass = {
    case class School(field: String)

    val s1 = School("value1")
    val s2 = School("value1changed")

    FindDeltaMeta[School].apply(s1, s2) must_=== Meta("field", Ior.Both("value1", "value1changed"), Nil)
  }

  private def simpleTestForInt = {
    case class School(field: Int)

    val s1 = School(1)
    val s2 = School(2)

    FindDeltaMeta[School].apply(s1, s2) must_=== Meta("field", Ior.Both("1", "2"), Nil)
  }

  private def simpleTestForDouble = {
    case class School(field: Double)

    val s1 = School(1)
    val s2 = School(2)

    FindDeltaMeta[School].apply(s1, s2) must_=== Meta("field", Ior.Both("1.0", "2.0"), Nil)
  }

  private def simpleTestForLong = {
    case class School(field: Long)

    val s1 = School(1)
    val s2 = School(2)

    FindDeltaMeta[School].apply(s1, s2) must_=== Meta("field", Ior.Both("1", "2"), Nil)
  }

  private def simpleTestOfNestedCaseClassWithOnlyNames = {
    case class Address(name: String)
    case class School(address: Address)

    implicit val hasNameDL: HasId[Address] = _.name

    val s1 = School(Address("name"))
    val s2 = School(Address("name"))

    FindDeltaMeta[School].apply(s1, s2) must_=== Meta.empty
  }

  // If there is a nested class, the code doesn't compile without a HasName instance.
  // The key strucutre is key.<keyname>.innerKey.<innerKeyName>.field. Ex: pinAddress.<name-of-storageaccount>.tier
  private def simpleNestedCaseClassWithSameNames = {
    case class PinAddress(tier: String, name: String)
    case class School(pinAddress: PinAddress)

    implicit val hasNamePinAddress: HasId[PinAddress] = _.name

    val s1 = School(PinAddress("oldValue", "storage-name"))
    val s2 = School(PinAddress("newValue", "storage-name"))

    FindDeltaMeta[School].apply(s1, s2) must_=== Meta(
      "pinAddress.storage-name.tier",
      Ior.Both("oldValue", "newValue")
      , Nil)
  }

  private def simpleNestedCaseClassWithDifferentNames = {
    case class PinAddress(tier: String, name: String)
    case class School(pinAddress: PinAddress)

    implicit val hasNamePinAddress: HasId[PinAddress] = _.name

    val s1 = School(PinAddress("oldValue", "storage-name1"))
    val s2 = School(PinAddress("newValue", "storage-name2"))

    FindDeltaMeta[School].apply(s1, s2) must_===
      Meta("pinAddress.storage-name2", Ior.Right("PinAddress(newValue,storage-name2)"), Nil) ++
        Meta("pinAddress.storage-name1", Ior.Left("PinAddress(oldValue,storage-name1)"), Nil)
  }

  private def multipleInnerCaseClassWithSameNames = {
    case class PinAddress(tier: String, name: String)
    case class Address(sku: String, name: String)
    case class School(pinAddress: PinAddress, address: Address)

    implicit val hasNamePinAddress: HasId[PinAddress] = _.name
    implicit val hasNameAddress: HasId[Address] = _.name

    val s1 = School(PinAddress("oldValue", "storage-name"), Address("oldValue", "address-name"))
    val s2 = School(PinAddress("newValue", "storage-name"), Address("newValue", "address-name"))

    FindDeltaMeta[School].apply(s1, s2) must_===
      Meta("pinAddress.storage-name.tier", Ior.Both("oldValue", "newValue"), Nil) ++
        Meta("address.address-name.sku", Ior.Both("oldValue", "newValue"), Nil)
  }

  private def multipleInnerCaseClassWithDifferentNames = {
    case class PinAddress(tier: String, name: String)
    case class Address(sku: String, name: String)
    case class School(pinAddress: PinAddress, address: Address)

    implicit val hasNamePinAddress: HasId[PinAddress] = _.name
    implicit val hasNameAddress: HasId[Address] = _.name

    val s1 = School(PinAddress("oldValue", "storage-name1"), Address("oldValue", "address-name1"))
    val s2 = School(PinAddress("newValue", "storage-name2"), Address("newValue", "address-name2"))

    FindDeltaMeta[School].apply(s1, s2) must_===
      Meta("pinAddress.storage-name2", Ior.Right("PinAddress(newValue,storage-name2)"), Nil) ++
        Meta("pinAddress.storage-name1", Ior.Left("PinAddress(oldValue,storage-name1)"), Nil) ++
        Meta("address.address-name2", Ior.Right("Address(newValue,address-name2)"), Nil) ++
        Meta("address.address-name1", Ior.Left("Address(oldValue,address-name1)"), Nil)
  }

  private def multipleInnerCaseClassWithDifferentNamesAndSameNames = {
    case class PinAddress(tier: String, name: String)
    case class Address(sku: String, name: String)
    case class School(pinAddress: PinAddress, address: Address)

    implicit val hasNamePinAddress: HasId[PinAddress] = _.name
    implicit val hasNameAddress: HasId[Address] = _.name

    val s1 = School(PinAddress("oldValue", "storage-name"), Address("oldValue", "address-name1"))
    val s2 = School(PinAddress("newValue", "storage-name"), Address("newValue", "address-name2"))

    FindDeltaMeta[School].apply(s1, s2) must_===
      Meta("pinAddress.storage-name.tier", Ior.Both("oldValue", "newValue"), Nil) ++
        Meta("address.address-name2", Ior.Right("Address(newValue,address-name2)"), Nil) ++
        Meta("address.address-name1", Ior.Left("Address(oldValue,address-name1)"), Nil)
  }

  private def nestedToNestedInnerClassWithSameNames = {
    case class Loc(value: String, name: String)
    case class Address(loc: Loc, name: String)
    case class School(address: Address)

    implicit val hasNamePinAddress: HasId[Address] = _.name
    implicit val hasNameLoc: HasId[Loc] = _.name

    val s1 = School(Address(Loc("oldValue", "loc-name"), "address-name"))
    val s2 = School(Address(Loc("newValue", "loc-name"), "address-name"))
    val s3 = School(Address(Loc("oldValue", "loc-name"), "address-name"))
    val s4 = School(Address(Loc("oldValue", "loc-name"), "address-name"))

    (FindDeltaMeta[School].apply(s1, s2) must_=== Meta(
      "address.address-name.loc.loc-name.value",
      Ior.Both("oldValue", "newValue")
      , Nil)) and (
      FindDeltaMeta[School].apply(s3, s4) must_=== Meta.empty
      )
  }

  private def nestedToNestedInnerClassWithDifferentNames = {
    case class Loc(value: String, name: String)
    case class Address(loc: Loc, name: String)
    case class School(address: Address)

    implicit val hasNamePinAddress: HasId[Address] = _.name
    implicit val hasNameLoc: HasId[Loc] = _.name

    val s1 = School(Address(Loc("oldValue", "loc-name1"), "address-name"))
    val s2 = School(Address(Loc("newValue", "loc-name2"), "address-name"))
    val s3 = School(Address(Loc("oldValue", "loc-name3"), "address-name"))
    val s4 = School(Address(Loc("oldValue", "loc-name4"), "address-name"))

    (FindDeltaMeta[School].apply(s1, s2) must_===
      Meta("address.address-name.loc.loc-name2", Ior.Right("Loc(newValue,loc-name2)"), Nil) ++
        Meta("address.address-name.loc.loc-name1", Ior.Left("Loc(oldValue,loc-name1)"), Nil)) and (
      FindDeltaMeta[School].apply(s3, s4) must_===
        Meta("address.address-name.loc.loc-name4", Ior.Right("Loc(oldValue,loc-name4)"), Nil) ++
          Meta("address.address-name.loc.loc-name3", Ior.Left("Loc(oldValue,loc-name3)"), Nil)
      )
  }

  private def nestedToNestedInnerClassWithSameAndDifferentNames = {
    case class Loc(value: String, name: String)
    case class Address(loc: Loc, name: String)
    case class School(address: Address)

    implicit val hasNamePinAddress: HasId[Address] = _.name
    implicit val hasNameLoc: HasId[Loc] = _.name

    val s1 = School(Address(Loc("oldValue", "loc-name"), "address-name"))
    val s2 = School(Address(Loc("newValue", "loc-name"), "address-name"))
    val s3 = School(Address(Loc("oldValue", "loc-name3"), "address-name"))
    val s4 = School(Address(Loc("oldValue", "loc-name4"), "address-name"))

    (FindDeltaMeta[School].apply(s1, s2) must_===
      Meta("address.address-name.loc.loc-name.value", Ior.Both("oldValue", "newValue"), Nil)) and (
      FindDeltaMeta[School].apply(s3, s4) must_===
        Meta("address.address-name.loc.loc-name4", Ior.Right("Loc(oldValue,loc-name4)"), Nil) ++
          Meta("address.address-name.loc.loc-name3", Ior.Left("Loc(oldValue,loc-name3)"), Nil)
      )
  }

  private def nestedToNestedInnerClassWithSameAndDifferentNamesWith3Levels = {
    case class Per(permisson: String, name: String)
    case class Loc(per: Per, name: String)
    case class Address(loc: Loc, name: String)
    case class School(address: Address)

    // TODO; Fix missing Per implicit results in a different result due to lower priority implicit.
    // Catch this at compile time.0
    implicit val hasNamePinAddress: HasId[Address] = _.name
    implicit val hasNameLoc: HasId[Loc] = _.name
    implicit val hasNamePerm: HasId[Per] = _.name

    val s1 = School(Address(Loc(Per("oldValue", "per-name"), "loc-name"), "address-name"))
    val s2 = School(Address(Loc(Per("newValue", "per-name"), "loc-name"), "address-name"))
    val s3 = School(Address(Loc(Per("oldValue", "per-name1"), "loc-name"), "address-name"))
    val s4 = School(Address(Loc(Per("oldValue", "per-name2"), "loc-name"), "address-name"))

    (FindDeltaMeta[School].apply(s1, s2) must_===
      Meta(
        "address.address-name.loc.loc-name.per.per-name.permisson",
        Ior.Both("oldValue", "newValue")
        , Nil)) and (
      FindDeltaMeta[School].apply(s3, s4) must_===
        Meta(
          "address.address-name.loc.loc-name.per.per-name2",
          Ior.Right("Per(oldValue,per-name2)")
          , Nil) ++
          Meta(
            "address.address-name.loc.loc-name.per.per-name1",
            Ior.Left("Per(oldValue,per-name1)")
            , Nil))
  }

  // For location, which is an AnyVal, the HasName is just its inner value. This is handled in HasName companion object
  // Hence the previous value -> new value is "australia -> india" and not "Location(australia) -> Location(india")"
  private def nestedToNestedInnerClassWithSameAndDifferentNamesWith4LevelsWithAnyVal = {
    case class Per(permisson: String, name: String, location: Location)
    case class Loc(per: Per, name: String)
    case class Address(loc: Loc, name: String)
    case class School(address: Address)

    // TODO; Fix missing Per implicit results in a different result due to lower priority implicit.
    // Catch this at compile time.0
    implicit val hasNamePinAddress: HasId[Address] = _.name
    implicit val hasNameLoc: HasId[Loc] = _.name
    implicit val hasNamePerm: HasId[Per] = _.name

    val s1 = School(
      Address(
        Loc(Per("oldValue", "per-name", Location("australia")), "loc-name"),
        "address-name"
      )
    )
    val s2 = School(
      Address(
        Loc(Per("newValue", "per-name", Location("india")), "loc-name"),
        "address-name"
      )
    )
    val s3 = School(
      Address(
        Loc(
          Per("oldValue", "per-name1", Location("australia")),
          "loc-name"
        ), "address-name"
      )
    )
    val s4 = School(
      Address(
        Loc(Per("oldValue", "per-name2", Location("india")), "loc-name"),
        "address-name"
      )
    )

    (FindDeltaMeta[School].apply(s1, s2) must_===
      Meta(
        "address.address-name.loc.loc-name.per.per-name.permisson",
        Ior.Both("oldValue", "newValue")
        , Nil) ++
        Meta(
          "address.address-name.loc.loc-name.per.per-name.location",
          Ior.Both("australia", "india")
          , Nil)) and
      (FindDeltaMeta[School].apply(s3, s4) must_=== Meta(
        "address.address-name.loc.loc-name.per.per-name2",
        Ior.Right("Per(oldValue,per-name2,Location(india))")
        , Nil) ++ Meta(
        "address.address-name.loc.loc-name.per.per-name1",
        Ior.Left("Per(oldValue,per-name1,Location(australia))")
        , Nil))
  }

  private def shortCircuitIfFirstLevelIsDifferent = {
    case class Per(permisson: String, name: String, location: Location)
    case class Loc(per: Per, name: String)
    case class Address(loc: Loc, name: String)
    case class School(address: Address, tier: String, subscriptionId: String, somethingElse: Int)

    // TODO; Fix missing Per implicit results in a different result due to lower priority implicit.
    // Catch this at compile time.0
    implicit val hasNamePinAddress: HasId[Address] = _.name
    implicit val hasNameLoc: HasId[Loc] = _.name
    implicit val hasNamePerm: HasId[Per] = _.name

    val s1 = School(
      Address(
        Loc(Per("oldValue", "per-name", Location("australia")), "loc-name"),
        "address-name1"
      ), "tier1", "subscription-id", 1
    )
    val s2 = School(
      Address(
        Loc(Per("newValue", "per-name", Location("india")), "loc-name"),
        "address-name2"
      ), "tier2", "subscription-id", 2
    )
    val s3 = School(
      Address(
        Loc(Per("oldValue", "per-name", Location("india")), "loc-name"),
        "address-name3"
      ), "tier3", "subscription-id", 3
    )
    val s4 = School(
      Address(
        Loc(Per("newValue", "per-name", Location("australia")), "loc-name"),
        "address-name3"
      ), "tier4", "subscription-id", 4
    )

    (FindDeltaMeta[School].apply(s1, s2) must_===
      Meta(
        "address.address-name2",
        Ior.Right("Address(Loc(Per(newValue,per-name,Location(india)),loc-name),address-name2)")
        , Nil) ++
        Meta(
          "address.address-name1",
          Ior
            .Left(
              "Address(Loc(Per(oldValue,per-name,Location(australia)),loc-name),address-name1)"
            )
          , Nil) ++
        Meta("tier", Ior.Both("tier1", "tier2"), Nil) ++ Meta("somethingElse", Ior.Both("1", "2"), Nil)) and (
      FindDeltaMeta[School].apply(s3, s4) must_===
        (Meta(
          "address.address-name3.loc.loc-name.per.per-name.permisson",
          Ior.Both("oldValue", "newValue")
          , Nil) ++
          Meta(
            "address.address-name3.loc.loc-name.per.per-name.location",
            Ior.Both("india", "australia")
            , Nil) ++
          Meta("tier", Ior.Both("tier3", "tier4"), Nil) ++
          Meta("somethingElse", Ior.Both("3", "4"), Nil)
          ))
  }

  private def shortCircuitIfFirstLevelIsDifferentWithExtraFields = {
    case class Per(permisson: String, name: String, location: Location)
    case class Loc(per: Per, name: String)
    case class Address(loc: Loc, name: String)
    case class School(address: Address, tier: String, subscriptionId: String, somethingElse: Int)

    // TODO; Fix missing Per implicit results in a different result due to lower priority implicit.
    // Catch this at compile time.0
    implicit val hasNamePinAddress: HasId[Address] = _.name
    implicit val hasNameLoc: HasId[Loc] = _.name
    implicit val hasNamePerm: HasId[Per] = _.name

    val s1 = School(
      Address(
        Loc(Per("oldValue", "per-name", Location("australia")), "loc-name"),
        "address-name1"
      ), "tier1", "subscription-id", 1
    )
    val s2 = School(
      Address(
        Loc(Per("newValue", "per-name", Location("india")), "loc-name"),
        "address-name2"
      ), "tier2", "subscription-id", 2
    )
    val s3 = School(
      Address(
        Loc(Per("oldValue", "per-name", Location("india")), "loc-name"),
        "address-name3"
      ), "tier3", "subscription-id", 3
    )
    val s4 = School(
      Address(
        Loc(Per("newValue", "per-name", Location("australia")), "loc-name"),
        "address-name3"
      ), "tier4", "subscription-id", 4
    )

    (FindDeltaMeta[School].apply(s1, s2) must_===
      Meta(
        "address.address-name2",
        Ior.Right("Address(Loc(Per(newValue,per-name,Location(india)),loc-name),address-name2)")
        , Nil) ++
        Meta(
          "address.address-name1",
          Ior
            .Left(
              "Address(Loc(Per(oldValue,per-name,Location(australia)),loc-name),address-name1)"
            )
          , Nil) ++
        Meta("tier", Ior.Both("tier1", "tier2"), Nil) ++ Meta("somethingElse", Ior.Both("1", "2"), Nil)) and (
      FindDeltaMeta[School].apply(s3, s4) must_===
        (Meta(
          "address.address-name3.loc.loc-name.per.per-name.permisson",
          Ior.Both("oldValue", "newValue")
          , Nil) ++
          Meta(
            "address.address-name3.loc.loc-name.per.per-name.location",
            Ior.Both("india", "australia")
            , Nil) ++
          Meta("tier", Ior.Both("tier3", "tier4"), Nil) ++
          Meta("somethingElse", Ior.Both("3", "4"), Nil)
          ))
  }


  private def simpleTestForListOfPrimitives = {
    case class School(field: List[String], tier: String)

    val s1 = School(List("afsal", "thaj"), "tier1")
    val s2 = School(List("afsalchanged", "thajchanged"), "tier2")

    FindDeltaMeta[School].apply(s1, s2) must_===
      Meta("field", Ior.Both("List(afsal, thaj)", "List(afsalchanged, thajchanged)"), Nil) ++
        Meta("tier", Ior.Both("tier1", "tier2"), Nil)
  }

  private def simpleListThatAreEmpty = {
    case class Address(loc: String, name: String)
    case class School(field: List[Address])

    val address1Changed1 = Address("loc1-changed", "datalake-account1")
    val address1Changed2 = Address("loc2-changed", "datalake-account2")

    implicit val hasNamePinAddress: HasId[Address] = _.name

    val s1 = School(Nil)
    val s2 = School(Nil)

    FindDeltaMeta[School].apply(s1, s2) must_=== Meta.empty
  }

  private def simpleTestForListOfProducts = {
    case class Address(loc: String, name: String)
    case class School(field: List[Address])

    val address1 = Address("loc1", "datalake-account1")
    val address2 = Address("loc2", "datalake-account2")
    val address3 = Address("loc3", "datalake-account3")
    val address4 = Address("loc4", "datalake-account4")

    val address1Changed1 = Address("loc1-changed", "datalake-account1")
    val address1Changed2 = Address("loc2-changed", "datalake-account2")

    implicit val hasNamePinAddress: HasId[Address] = _.name

    val s1 = School(List(address1, address2, address3, address4))
    val s2 = School(List(address1Changed1, address1Changed2, address3, address4))

    FindDeltaMeta[School].apply(s1, s2) must_===
      Meta("field.datalake-account1.loc", Ior.Both("loc1", "loc1-changed"), Nil) ++
        Meta("field.datalake-account2.loc", Ior.Both("loc2", "loc2-changed"), Nil)
  }

  private def twoLevelListOfProducts = {
    case class Loc(per: String, name: String)
    case class Address(loc: Loc, name: String)
    case class School(field: List[Address])

    val address1 = Address(Loc("loc1", "loc-name"), "datalake-account1")
    val address2 = Address(Loc("loc2", "loc-name"), "datalake-account2")
    val address3 = Address(Loc("loc3", "loc-name3"), "datalake-account3")
    val address4 = Address(Loc("loc4", "loc-name4"), "datalake-account4")

    val address1Changed1 = Address(Loc("loc1-changed", "loc-name"), "datalake-account1")
    val address1Changed2 = Address(Loc("loc2-changed", "loc-name"), "datalake-account2")
    val address1Changed4 = Address(Loc("loc4-changed", "loc-name4-new"), "datalake-account4")

    implicit val hasNamePinAddress: HasId[Address] = _.name
    implicit val hasNameLoc: HasId[Loc] = _.name

    val s1 = School(List(address1, address2, address3, address4))
    val s2 = School(
      List(
        address1Changed1,
        address1Changed2,
        address3,
        address1Changed4
      )
    )

    FindDeltaMeta[School].apply(s1, s2) must_===
      Meta("field.datalake-account1.loc.loc-name.per", Ior.Both("loc1", "loc1-changed"), Nil) ++
        Meta("field.datalake-account2.loc.loc-name.per", Ior.Both("loc2", "loc2-changed"), Nil) ++
        Meta("field.datalake-account4.loc.loc-name4-new", Ior.Right("Loc(loc4-changed,loc-name4-new)"), Nil) ++
        Meta("field.datalake-account4.loc.loc-name4", Ior.Left("Loc(loc4,loc-name4)"), Nil)
  }

  private def twoLevelListOfListofProducts = {
    case class Loc(per: String, name: String)
    case class Address(locs: List[Loc], name: String)
    case class School(field: List[Address])

    val loc1 = Loc("loc1", "loc-name1")
    val loc2 = Loc("loc2", "loc-name2")
    val loc3 = Loc("loc3", "loc-name3")
    val loc4 = Loc("loc4", "loc-name4")
    val loc5 = Loc("loc5", "new-loc5")
    val loc6 = Loc("loc6", "new-loc6")

    val loc1Changed = Loc("loc1-changed", "loc-name1")
    val loc2Changed = Loc("loc2-changed", "loc-name2")

    val address1 = Address(List(loc1, loc2), "datalake-account1")
    val address2 = Address(List(loc1, loc2), "datalake-account2")
    val address3 = Address(List(loc2, loc3), "datalake-account3")
    val address4 = Address(List(loc3, loc4), "datalake-account4")

    val address1Changed1 = Address(List(loc1Changed, loc2), "datalake-account1")
    val address1Changed2 = Address(List(loc1, loc2Changed), "datalake-account2")
    val address1Changed4 = Address(List(loc5, loc6), "datalake-account4")

    implicit val hasNamePinAddress: HasId[Address] = _.name
    implicit val hasNameLoc: HasId[Loc] = _.name

    val s1 = School(List(address1, address2, address3, address4))
    val s2 = School(
      List(
        address1Changed1,
        address1Changed2,
        address3,
        address1Changed4
      )
    )

    FindDeltaMeta[School].apply(s1, s2) must_===
      Meta("field.datalake-account1.locs.loc-name1.per", Ior.Both("loc1", "loc1-changed"), Nil) ++
        Meta("field.datalake-account2.locs.loc-name2.per", Ior.Both("loc2", "loc2-changed"), Nil) ++
        Meta("field.datalake-account4.locs.loc-name3", Ior.Left("Loc(loc3,loc-name3)"), Nil) ++
        Meta("field.datalake-account4.locs.loc-name4", Ior.Left("Loc(loc4,loc-name4)"), Nil) ++
        Meta("field.datalake-account4.locs.new-loc5", Ior.Right("Loc(loc5,new-loc5)"), Nil) ++
        Meta("field.datalake-account4.locs.new-loc6", Ior.Right("Loc(loc6,new-loc6)"), Nil)
  }


  private def simpleTestForOptionWithPrimnitives = {
    case class School(x: Option[String], tier: String)

    val s1 = School(Some("afsal"), "tier1")
    val s2 = School(None, "tier1")

    FindDeltaMeta[School].apply(s1, s2) must_===
      // TODO, what happens if its an option of string, not sure. But doesn't matter for now.
      Meta("x", Ior.Both("Some(afsal)", "None"), Nil)
  }

  private def simpleTestForOption = {
    case class X(n: Int, name: String)
    case class School(x: Option[X], tier: String)

    implicit val hasName: HasId[X] = _.name

    val x = X(1, "afsal")

    val s1 = School(Some(x), "tier1")
    val s2 = School(None, "tier1")
    val s3 = School(Some(x), "tier1")
    val s4 = School(Some(x.copy(n = x.n + 1)), "tier1")
    val s5 = School(None, "tier1")
    val s6 = School(Some(x), "tier1")

    (FindDeltaMeta[School].apply(s1, s2) must_=== Meta("x.afsal", Ior.Left("X(1,afsal)"), Nil)) and (
      FindDeltaMeta[School].apply(s3, s4) must_=== Meta("x.afsal.n", Ior.Both("1", "2"), Nil)) and (
      FindDeltaMeta[School].apply(s5, s6) must_=== Meta("x.afsal", Ior.Right(x.toString), Nil)
      )
  }

  private def simpleTestForOptionWith2Levels = {
    case class Y(n: Int, name: String)
    case class X(y: Y, name: String)

    case class School(x: Option[X], tier: String)

    implicit val hasNameX: HasId[X] = _.name
    implicit val hasNameY: HasId[Y] = _.name

    val x = X(Y(1, "y-name"), "x-name")
    val xChanged = X(Y(2, "y-name"), "x-name")

    val s1 = School(Some(x), "tier1")
    val s2 = School(Some(xChanged), "tier2")

    FindDeltaMeta[School].apply(s1, s2) must_=== Meta("x.x-name.y.y-name.n", Ior.Both("1", "2"), Nil) ++ Meta(
      "tier",
      Ior.Both("tier1", "tier2")
      , Nil)
  }

  private def simpleTestWithEnumeratumEnumEntries = {
    abstract sealed class XYZ(override val entryName: String) extends EnumEntry

    object XYZ extends Enum[XYZ] {
      val values = findValues

      final case object Cntt extends XYZ("cntttt")
      final case object abc extends XYZ("abc")
    }

    case class School(x: Option[String], storageAccess: XYZ, secondXYZ: XYZ)

    val s1 = School(Some("afsal"), XYZ.Cntt, XYZ.Cntt)
    val s2 = School(None, XYZ.abc, XYZ.Cntt)

    FindDeltaMeta[School].apply(s1, s2) must_===
      Meta("x", Ior.Both("Some(afsal)", "None"), Nil) ++ Meta("storageAccess", Ior.Both("cntttt", "abc"), Nil)
  }

  private def simpleTestWithEnumeratumEnumEntriesNested = {
    abstract sealed class XYZ(override val entryName: String) extends EnumEntry

    object XYZ extends Enum[XYZ] {
      val values = findValues

      final case object Cntt extends XYZ("cntttt")
      final case object abc extends XYZ("blob")
    }

    case class Y(en: XYZ, name: String)

    implicit val hasNameY: HasId[Y] =_.name

    case class School(x: Option[String], y: Y)

    val y1 = Y(XYZ.Cntt, "y1")
    val y2 = Y(XYZ.abc, "y1")

    val s1 = School(Some("afsal"), y1)
    val s2 = School(None, y2)

    FindDeltaMeta[School].apply(s1, s2) must_===
      Meta("x", Ior.Both("Some(afsal)", "None"), Nil) ++ Meta("y.y1.en", Ior.Both("cntttt", "blob"), Nil)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  private def overrideAnyValsEquality =  {
    case class Per(permisson: String, name: String, location: Location)
    case class Loc(per: Per, name: String)
    case class Address(loc: Loc, name: String)
    case class School(address: Address, tier: String, subscriptionId: String, somethingElse: Int)

    // TODO; Fix missing Per implicit results in a different result due to lower priority implicit.
    // Catch this at compile time.0
    implicit val hasNamePinAddress: HasId[Address] = _.name
    implicit val hasNameLoc: HasId[Loc] = _.name
    implicit val hasNamePerm: HasId[Per] = _.name

    // This implies if the left side is india, the location update is discarded.
    implicit val primitiveLocation: Primitive[Location] =
      Primitive.fromEquality((a, b) => if (a.value == "india") true else a.value == b.value)

    // CustomLocation considers australia and India to be the same

    val s1 = School(
      Address(
        Loc(Per("oldValue", "per-name", Location("australia")), "loc-name"),
        "address-name1"
      ), "tier1", "subscription-id", 1
    )
    val s2 = School(
      Address(
        Loc(Per("newValue", "per-name", Location("india")), "loc-name"),
        "address-name2"
      ), "tier2", "subscription-id", 2
    )
    val s3 = School(
      Address(
        Loc(Per("oldValue", "per-name", Location("india")), "loc-name"),
        "address-name3"
      ), "tier3", "subscription-id", 3
    )
    val s4 = School(
      Address(
        Loc(Per("newValue", "per-name", Location("australia")), "loc-name"),
        "address-name3"
      ), "tier4", "subscription-id", 4
    )

    (FindDeltaMeta[School].apply(s1, s2) must_===
      Meta(
        "address.address-name2",
        Ior.Right("Address(Loc(Per(newValue,per-name,Location(india)),loc-name),address-name2)")
        , Nil) ++
        Meta(
          "address.address-name1",
          Ior
            .Left(
              "Address(Loc(Per(oldValue,per-name,Location(australia)),loc-name),address-name1)"
            )
          , Nil) ++
        Meta("tier", Ior.Both("tier1", "tier2"), Nil) ++ Meta("somethingElse", Ior.Both("1", "2"), Nil)) and (
      FindDeltaMeta[School].apply(s3, s4) must_===
        (Meta(
          "address.address-name3.loc.loc-name.per.per-name.permisson",
          Ior.Both("oldValue", "newValue")
          , Nil) ++
          Meta("tier", Ior.Both("tier3", "tier4"), Nil) ++
          Meta("somethingElse", Ior.Both("3", "4"), Nil)
          ))
  }
}
