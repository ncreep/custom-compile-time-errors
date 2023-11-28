package ncreep

import io.github.iltotore.iron.constraint.collection.*
import io.github.iltotore.iron.constraint.numeric.*
import io.github.iltotore.iron.constraint.numeric
import io.github.iltotore.iron.{! => _, *}

case class HotelTable(
    trHotelId: Key,
    trLastUpdate: Timestamp,
    trHotelName: SizedString[200],
    trHotelAddress: SizedString[200],
    trCity: SizedString[100],
    trState: SizedString[50],
    trZipCode: SizedString[10],
    trCountry: SizedString[20],
    trPhoneNumber: SizedString[15],
    trHotelWebsite: SizedString[200],
    trHotelRating: Rating[5],
    trAmenities: SizedString[300],
    trNumberOfRooms: Natural,
    trNumberOfAvailableRooms: Natural,
    trRoomTypes: SizedString[30],
    trRoomPrices: SizedString[70],
    trHotelImageUrl: SizedString[300],
    trDescription: SizedString[300],
    trPolicies: SizedString[200],
    trCancellationPolicy: SizedString[500],
    trContactPerson: SizedString[50],
    trContactEmail: SizedString[40])

object HotelTable:
// sometimes you need to "recompile workspace" to get this properly working
  // performChecks[SchemaOkay, HotelTable]
end HotelTable

case class BadHotelTable(
    trLastUpdate: Timestamp,
    trHotelName: SizedString[200],
    trHotelAddress: SizedString[200],
    trCity: SizedString[100],
    trRegion: SizedString[50],
    trState: SizedString[50],
    trZipCode: SizedString[10],
    trCountry: SizedString[20],
    trCreationDate: Timestamp,
    trPhoneNumber: SizedString[15],
    trSecondaryPhoneNumber: SizedString[15],
    trHotelWebsite: SizedString[200],
    trHotelRating: Rating[5],
    trAmenities: String,
    trNumberOfRooms: Natural,
    trNumberOfAvailableRooms: Natural,
    trNumberOfBusinessCenters: Natural,
    roomTypes: SizedString[30],
    trRoomPrices: SizedString[70],
    trHotelImageUrl: SizedString[300],
    description: SizedString[300],
    trPolicies: SizedString[200],
    trPrivacyPolicy: SizedString[300],
    cancellationPolicy: SizedString[700],
    trContactPerson: SizedString[50],
    trContactEmail: SizedString[40])

object BadHotelTable:
// sometimes you need to "recompile workspace" to get this properly working
// seems to have issues when trying to invoke this directly in the REPL
  // performChecks[SchemaOkay, BadHotelTable]
end BadHotelTable

// Cannot create match types on both String and an opaque type that is a subtype of a
// string
// So creating a wrapper instead
// Possibly related to these issues:
// https://github.com/lampepfl/dotty/issues/17339
// https://github.com/lampepfl/dotty/issues/13804
// https://github.com/lampepfl/dotty/issues/17211
case class SizedString[N <: Int](value: String :| MaxLength[N])

case class Rating[UpTo <: Int](value: Double :| Interval.Closed[0, UpTo])

case class Natural(value: Int :| numeric.GreaterEqual[0])

case class Key(value: String)

case class Timestamp(value: Long)
