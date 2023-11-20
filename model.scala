package ncreep

import io.github.iltotore.iron.constraint.collection.*
import io.github.iltotore.iron.constraint.numeric.*
import io.github.iltotore.iron.{! => _, *}

// Cannot match on both String and an opaque type that is a subtype of a string
// So creating a wrapper instead
// Possibly related to this:
// https://github.com/lampepfl/dotty/issues/17339
// https://github.com/lampepfl/dotty/issues/13804
// https://github.com/lampepfl/dotty/issues/17211
case class SizedString[N <: Int](value: String :| MaxLength[N])

case class Key(value: String)

case class Timestamp(value: Long)

case class Foo(
    id: Key,
    time: Timestamp,
    time2: Timestamp,
    time3: Timestamp,
    someInt: Int,
    actualStr: String,
    someStr: SizedString[30],
    otherStr: SizedString[12])

// CREATE TABLE hotels (
//   hotel_id INT PRIMARY KEY AUTO_INCREMENT,
//   hotel_name VARCHAR(255) NOT NULL,
//   hotel_address VARCHAR(255) NOT NULL,
//   city VARCHAR(255) NOT NULL,
//   state VARCHAR(255) NOT NULL,
//   zip_code VARCHAR(255) NOT NULL,
//   country VARCHAR(255) NOT NULL,
//   phone_number VARCHAR(255) NOT NULL,
//   hotel_website VARCHAR(255) NOT NULL,
//   hotel_rating DECIMAL(2,1) NOT NULL,
//   amenities VARCHAR(255) NOT NULL,
//   number_of_rooms INT NOT NULL,
//   number_of_available_rooms INT NOT NULL,
//   room_types VARCHAR(255) NOT NULL,
//   room_prices VARCHAR(255) NOT NULL,
//   hotel_image_url VARCHAR(255) NOT NULL,
//   description VARCHAR(255) NOT NULL,
//   policies VARCHAR(255) NOT NULL,
//   cancellation_policy VARCHAR(255) NOT NULL,
//   contact_person VARCHAR(255) NOT NULL,
//   contact_email VARCHAR(255) NOT NULL
// );