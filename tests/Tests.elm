module Tests exposing (suite)

import Dict
import Expect
import JSContact exposing (AddressComponentKind(..), JSContact, Kind(..), NameComponentKind(..), RelationType(..))
import JSContact.Encoder exposing (encode)
import Json.Decode exposing (decodeString)
import Json.Encode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "JSContact"
        [ parseJSContact
        , missingData
        , encoding
        ]


minimalContactStr : String
minimalContactStr =
    """{
  "@type": "Card",
  "version": "1.0",
  "uid": "urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6"
}"""


minimalContact : JSContact
minimalContact =
    { kind = Individual
    , uid = "urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6"
    , version = "1.0"
    , created = Nothing
    , updated = Nothing
    , language = Nothing
    , name = Nothing
    , members = []
    , prodId = Nothing
    , relatedTo = Dict.empty
    , emails = Dict.empty
    , addresses = Dict.empty
    , phones = Dict.empty
    , onlineServices = Dict.empty
    , preferredLanguages = Dict.empty
    }


parseJSContact : Test
parseJSContact =
    describe "parseJSContact"
        [ test "can parse a minimal contact" <|
            \_ -> Ok minimalContact |> Expect.equal (decodeString JSContact.decoder minimalContactStr)
        , test "can parse a full contact" <|
            \_ -> Ok fullContact |> Expect.equal (decodeString JSContact.decoder fullContactStr)
        ]


fullContactStr : String
fullContactStr =
    """{
  "@type": "Card",
  "version": "1.0",
  "uid": "urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6",
  "prodId": "Elm JSContact",
  "language": "en-CA",
  "members": {
    "urn:uuid:03a0e51f-d1aa-4385-8a53-e29025acd8af": true,
    "urn:uuid:b8767877-b4a1-4c70-9acc-505d3819e519": true
  },
  "relatedTo": {
    "urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6": {
      "relation": {
        "friend": true
      }
    }
  },
  "name": {
    "components": [
      { "kind": "given", "value": "Robert" },
      { "kind": "given2", "value": "Pau" },
      { "kind": "surname", "value": "Shou Chang" }
    ],
    "sortAs": {
      "surname": "Pau Shou Chang",
      "given": "Robert"
    },
    "isOrdered": true
  },
  "addresses": {
    "k23": {
      "contexts": {
        "work": true
      },
      "components": [
        { "kind": "number", "value": "54321" },
        { "kind": "separator", "value": " " },
        { "kind": "name", "value": "Oak St" },
        { "kind": "locality", "value": "Reston" },
        { "kind": "region", "value": "VA" },
        { "kind": "separator", "value": " " },
        { "kind": "postcode", "value": "20190" },
        { "kind": "country", "value": "USA" }
      ],
      "countryCode": "US",
      "defaultSeparator": ", ",
      "isOrdered": true
    }
  },
  "emails": {
      "e1": {
        "contexts": {
          "work": true
        },
        "address": "jqpublic@xyz.example.com"
      },
      "e2": {
        "address": "jane_doe@example.com",
        "pref": 1
      }
  },
  "onlineServices": {
    "x1": {
      "uri": "xmpp:alice@example.com"
    },
    "x2": {
      "service": "Mastodon",
      "user": "@alice@example2.com",
      "uri": "https://example2.com/@alice"
    }
  },

  "preferredLanguages": {
    "l1": {
      "language": "en",
      "contexts": {
        "work": true
      },
      "pref": 1
    },
    "l2": {
      "language": "fr",
      "contexts": {
        "work": true
      },
      "pref": 2
    },
    "l3": {
      "language": "fr",
      "contexts": {
        "private": true
      }
    }
  }
}"""


fullContact : JSContact
fullContact =
    { kind = Individual
    , uid = "urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6"
    , version = "1.0"
    , created = Nothing
    , updated = Nothing
    , language = Just "en-CA"
    , members =
        [ "urn:uuid:03a0e51f-d1aa-4385-8a53-e29025acd8af"
        , "urn:uuid:b8767877-b4a1-4c70-9acc-505d3819e519"
        ]
    , name =
        Just
            { components =
                [ { kind = Given, value = "Robert", phonetic = Nothing }
                , { kind = Given2, value = "Pau", phonetic = Nothing }
                , { kind = Surname, value = "Shou Chang", phonetic = Nothing }
                ]
            , sortAs =
                [ ( Surname, "Pau Shou Chang" )
                , ( Given, "Robert" )
                ]
            , isOrdered = True
            , phoneticScript = Nothing
            , phoneticSystem = Nothing
            , defaultSeparator = Nothing
            , full = Nothing
            }
    , prodId = Just "Elm JSContact"
    , relatedTo =
        [ ( "urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6", [ Friend ] )
        ]
            |> Dict.fromList
    , emails =
        [ ( "e1", { address = "jqpublic@xyz.example.com", contexts = [ "work" ], pref = Nothing } )
        , ( "e2", { address = "jane_doe@example.com", pref = Just 1, contexts = [] } )
        ]
            |> Dict.fromList
    , addresses =
        [ ( "k23"
          , { components =
                [ { kind = StreetNumber, value = "54321", phonetic = Nothing }
                , { kind = Separator, value = " ", phonetic = Nothing }
                , { kind = AddressName, value = "Oak St", phonetic = Nothing }
                , { kind = Locality, value = "Reston", phonetic = Nothing }
                , { kind = Region, value = "VA", phonetic = Nothing }
                , { kind = Separator, value = " ", phonetic = Nothing }
                , { kind = Postcode, value = "20190", phonetic = Nothing }
                , { kind = Country, value = "USA", phonetic = Nothing }
                ]
            , countryCode = Just "US"
            , coordinates = Nothing
            , full = Nothing
            , timeZone = Nothing
            , defaultSeparator = Just ", "
            , isOrdered = True
            , contexts = [ "work" ]
            , pref = Nothing
            , phoneticScript = Nothing
            , phoneticSystem = Nothing
            }
          )
        ]
            |> Dict.fromList
    , phones = Dict.empty
    , onlineServices =
        [ ( "x1"
          , { service = Nothing
            , user = Nothing
            , uri = "xmpp:alice@example.com"
            , contexts = []
            , pref = Nothing
            }
          )
        , ( "x2"
          , { service = Just "Mastodon"
            , user = Just "@alice@example2.com"
            , uri = "https://example2.com/@alice"
            , contexts = []
            , pref = Nothing
            }
          )
        ]
            |> Dict.fromList
    , preferredLanguages =
        [ ( "l1", { language = "en", pref = Just 1, contexts = [ "work" ] } )
        , ( "l2", { language = "fr", pref = Just 2, contexts = [ "work" ] } )
        , ( "l3", { language = "fr", pref = Nothing, contexts = [ "private" ] } )
        ]
            |> Dict.fromList
    }


{-| A naive way of removing the given field from the minimal example
-}
missing : String -> String
missing key =
    String.lines minimalContactStr
        |> List.filter (\line -> not (line |> String.contains key))
        |> String.join "\n"


missingData : Test
missingData =
    describe "attempt to parse with missing mandatory fields" <|
        [ test "fails to parse if @type field is missing" <|
            \_ -> Expect.err (decodeString JSContact.decoder <| missing "@type")
        , test "fails to parse if version field is missing" <|
            \_ -> Expect.err (decodeString JSContact.decoder <| missing "version")
        , test "fails to parse if uid field is missing" <|
            \_ -> Expect.err (decodeString JSContact.decoder <| missing "uid")
        ]


encoding : Test
encoding =
    describe "encoding & decoding results in the same result" <|
        [ test "mininmal contact is encoded" <|
            \_ ->
                Expect.equal
                    (Ok minimalContact)
                    (encode minimalContact
                        |> Json.Encode.encode 0
                        |> Json.Decode.decodeString JSContact.decoder
                    )
        , test "full contact is encoded" <|
            \_ ->
                Expect.equal
                    (Ok fullContact)
                    (encode fullContact
                        |> Json.Encode.encode 0
                        |> Json.Decode.decodeString JSContact.decoder
                    )
        ]
