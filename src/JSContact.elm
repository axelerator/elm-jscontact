module JSContact exposing
    ( decoder, JSContact
    , Address, AddressComponent, AddressComponentKind(..), Addresses, Email, Emails, Kind(..), OnlineService, OnlineServices, Phone, Phones, PreferredLanguage, PreferredLanguages, RelatedTo, RelationType(..), UTCDateTime, Uid, Name, NameComponent, NameComponentKind(..)
    )

{-|

@docs decoder, JSContact

@docs Address, AddressComponent, AddressComponentKind, Addresses, Email, Emails, Kind, OnlineService, OnlineServices, Phone, Phones, PreferredLanguage, PreferredLanguages, RelatedTo, RelationType, UTCDateTime, Uid, Name, NameComponent, NameComponentKind

-}

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, andThen, bool, dict, fail, field, int, list, map, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import List exposing (isEmpty)
import List.Extra exposing (unique)


{-| The contact card.
-}
type alias JSContact =
    { kind : Kind
    , version : String
    , uid : Uid
    , created : Maybe UTCDateTime
    , updated : Maybe UTCDateTime
    , language : Maybe String
    , name : Maybe Name
    , members : List Uid
    , prodId : Maybe String
    , relatedTo : RelatedTo
    , emails : Emails
    , addresses : Addresses
    , phones : Phones
    , onlineServices : OnlineServices
    , preferredLanguages : PreferredLanguages
    }


{-| A JSON decoder for `JSContact`.
-}
decoder : Decoder JSContact
decoder =
    field "@type" string
        |> andThen
            (\typeStr ->
                if typeStr /= "Card" then
                    fail "@type must be Card"

                else
                    succeed JSContact
                        |> optional "kind" kindDecoder Individual
                        |> required "version" string
                        |> required "uid" string
                        |> optional "created" (map Just string) Nothing
                        |> optional "updated" (map Just string) Nothing
                        |> optional "language" (map Just string) Nothing
                        |> optional "name" (map Just nameDecoder) Nothing
                        |> optional "members" members []
                        |> optional "prodId" (map Just string) Nothing
                        |> optional "relatedTo" relatedToDecoder Dict.empty
                        |> optional "emails" emails Dict.empty
                        |> optional "addresses" addresses Dict.empty
                        |> optional "phones" phones Dict.empty
                        |> optional "onlineServices" onlineServices Dict.empty
                        |> optional "preferredLanguages" preferredLanguages Dict.empty
            )


{-| The UTCDateTime type is a String in "date-time" format [RFC3339]
-}
type alias UTCDateTime =
    String


{-| An identifier that associates the object as the same across different systems,
address books, and views. The value SHOULD be a URN [RFC8141],
but for compatibility with [RFC6350], it MAY also be a URI [RFC3986] or free-text value.
The value of the URN SHOULD be in the "uuid" namespace [RFC9562].
[RFC9562] describes multiple versions of Universally Unique IDentifiers (UUIDs);
UUID version 4 is RECOMMENDED.
-}
type alias Uid =
    String


{-| The kind of the entity the Card represents
-}
type Kind
    = Individual
    | Group
    | Organization
    | Location
    | Device
    | Application


kindDecoder : Decoder Kind
kindDecoder =
    string
        |> andThen
            (\k ->
                case k of
                    "individual" ->
                        succeed Individual

                    "group" ->
                        succeed Group

                    "organization" ->
                        succeed Organization

                    "location" ->
                        succeed Location

                    "device" ->
                        succeed Device

                    "application" ->
                        succeed Application

                    _ ->
                        fail "invalid kind"
            )


{-| The set of Card objects that relate to the Card. The value is a map,
where each key is the uid property value of the related Card,
and the value defines the relation
-}
type alias RelatedTo =
    Dict Uid (List RelationType)


relatedToDecoder : Decoder RelatedTo
relatedToDecoder =
    dict relationsDecoder


relationsDecoder : Decoder (List RelationType)
relationsDecoder =
    field "relation" (dict bool)
        |> andThen
            (\d ->
                let
                    unifiedValues : List Bool
                    unifiedValues =
                        Dict.values d
                            |> unique
                in
                if unifiedValues == [ True ] then
                    let
                        folder : String -> ( List String, List RelationType ) -> ( List String, List RelationType )
                        folder s ( iv, v ) =
                            case relationTypeFromStr s of
                                Just r ->
                                    ( iv, r :: v )

                                Nothing ->
                                    ( s :: iv, v )

                        ( invalid, valid ) =
                            Dict.keys d
                                |> List.foldl folder ( [], [] )
                    in
                    if List.isEmpty invalid then
                        succeed valid

                    else
                        fail <| "unknown relation types: " ++ String.join ", " invalid

                else
                    fail "relation values must be true"
            )


relationTypeFromStr : String -> Maybe RelationType
relationTypeFromStr s =
    case s of
        "aquaintance" ->
            Just Aquaintance

        "agent" ->
            Just Agent

        "child" ->
            Just Child

        "coresident" ->
            Just Coresident

        "coworker" ->
            Just Coworker

        "colleague" ->
            Just Colleague

        "contact" ->
            Just Contact

        "crush" ->
            Just Crush

        "date" ->
            Just Date

        "emergency" ->
            Just Emergency

        "friend" ->
            Just Friend

        "kin" ->
            Just Kin

        "me" ->
            Just Me

        "met" ->
            Just Met

        "muse" ->
            Just Muse

        "neighbor" ->
            Just Neighbor

        "parent" ->
            Just Parent

        "sibling" ->
            Just Sibling

        "spouse" ->
            Just Spouse

        "sweetheart" ->
            Just Sweetheart

        _ ->
            Nothing


{-| The variants define the relation type between the two objects.
-}
type RelationType
    = Aquaintance
    | Agent
    | Child
    | Coresident
    | Coworker
    | Colleague
    | Contact
    | Crush
    | Date
    | Emergency
    | Friend
    | Kin
    | Me
    | Met
    | Muse
    | Neighbor
    | Parent
    | Sibling
    | Spouse
    | Sweetheart


members : Decoder (List Uid)
members =
    trueMap "members"


{-| This section defines how properties contact the entity represented by the Card.
-}
type alias Emails =
    Dict String Email


emails : Decoder Emails
emails =
    dict email


{-| The email addresses in which to contact the entity represented by the Card.
-}
type alias Email =
    { contexts : List String
    , address : String
    , pref : Maybe Int
    }


email : Decoder Email
email =
    succeed Email
        |> optional "contexts" contexts_ []
        |> required "address" string
        |> optional "pref" (map Just int) Nothing


{-| The phone numbers by which to contact the entity represented by the Card
-}
type alias Phones =
    Dict String Phone


phones : Decoder Phones
phones =
    dict phone


{-| A phone number by which to contact the entity represented by the Card
-}
type alias Phone =
    { contexts : List String
    , phone : String
    , pref : Maybe Int
    }


phone : Decoder Phone
phone =
    succeed Phone
        |> optional "contexts" contexts_ []
        |> required "phone" string
        |> optional "pref" (map Just int) Nothing


{-| The online services in which to contact the entity represented by the Card
-}
type alias OnlineServices =
    Dict String OnlineService


onlineServices : Decoder OnlineServices
onlineServices =
    dict onlineService


{-| The online services that are associated with the entity represented by the
Card. This can be messaging services, social media profiles, and other.
-}
type alias OnlineService =
    { service : Maybe String
    , user : Maybe String
    , uri : String
    , contexts : List String
    , pref : Maybe Int
    }


onlineService : Decoder OnlineService
onlineService =
    succeed OnlineService
        |> optional "service" (map Just string) Nothing
        |> optional "user" (map Just string) Nothing
        |> required "uri" string
        |> optional "contexts" contexts_ []
        |> optional "pref" (map Just int) Nothing


{-| The preferred languages for contacting the entity associated with the Card.
-}
type alias PreferredLanguages =
    Dict String PreferredLanguage


preferredLanguages : Decoder PreferredLanguages
preferredLanguages =
    dict preferredLanguage


{-| A preferred language for contacting the entity associated with the Card
-}
type alias PreferredLanguage =
    { language : String
    , contexts : List String
    , pref : Maybe Int
    }


preferredLanguage : Decoder PreferredLanguage
preferredLanguage =
    succeed PreferredLanguage
        |> required "language" string
        |> optional "contexts" contexts_ []
        |> optional "pref" (map Just int) Nothing


contexts_ : Decoder (List String)
contexts_ =
    trueMap "contexts"


trueMap : String -> Decoder (List String)
trueMap label =
    dict bool
        |> andThen
            (\d ->
                let
                    unifiedValues : List Bool
                    unifiedValues =
                        Dict.values d
                            |> unique
                in
                if isEmpty unifiedValues || unifiedValues == [ True ] then
                    Dict.keys d
                        |> succeed

                else
                    fail <| label ++ " values must be true"
            )


{-| This section defines properties for postal addresses and geographical
locations associated with the entity represented by the Card
-}
type alias Addresses =
    Dict String Address


{-| The address of the entity represented by the Card
-}
type alias Address =
    { contexts : List String
    , components : List AddressComponent
    , isOrdered : Bool
    , countryCode : Maybe String
    , coordinates : Maybe String
    , timeZone : Maybe String
    , full : Maybe String
    , defaultSeparator : Maybe String
    , pref : Maybe Int
    , phoneticScript : Maybe String
    , phoneticSystem : Maybe String
    }


{-| The components that make up the address.
The component list MUST have at least one entry that has a kind property value other than "separator".
-}
type alias AddressComponent =
    { value : String
    , kind : AddressComponentKind
    , phonetic : Maybe String
    }


addresses : Decoder (Dict String Address)
addresses =
    dict address


address : Decoder Address
address =
    succeed Address
        |> optional "contexts" contexts_ []
        |> required "components" (list addressComponent)
        |> optional "isOrdered" bool False
        |> optional "countryCode" (map Just string) Nothing
        |> optional "coordinates" (map Just string) Nothing
        |> optional "timeZone" (map Just string) Nothing
        |> optional "full" (map Just string) Nothing
        |> optional "defaultSeparator" (map Just string) Nothing
        |> optional "pref" (map Just int) Nothing
        |> optional "phoneticScript" (map Just string) Nothing
        |> optional "phoneticSystem" (map Just string) Nothing


addressComponent : Decoder AddressComponent
addressComponent =
    succeed AddressComponent
        |> required "value" string
        |> required "kind" addressComponentKindDecoder
        |> optional "phonetic" (map Just string) Nothing


{-| The kind of the address component.
-}
type AddressComponentKind
    = Room
    | Apartment
    | Floor
    | Building
    | StreetNumber
    | AddressName
    | Block
    | Subdistrict
    | District
    | Locality
    | Region
    | Postcode
    | Country
    | Direction
    | Landmark
    | PostOfficeBox
    | Separator
    | Other String


addressComponentKindDecoder : Decoder AddressComponentKind
addressComponentKindDecoder =
    string
        |> map
            (\k ->
                case k of
                    "room" ->
                        Room

                    "apartment" ->
                        Apartment

                    "floor" ->
                        Floor

                    "building" ->
                        Building

                    "number" ->
                        StreetNumber

                    "name" ->
                        AddressName

                    "block" ->
                        Block

                    "subdistrict" ->
                        Subdistrict

                    "district" ->
                        District

                    "locality" ->
                        Locality

                    "region" ->
                        Region

                    "postcode" ->
                        Postcode

                    "country" ->
                        Country

                    "direction" ->
                        Direction

                    "landmark" ->
                        Landmark

                    "postOfficeBox" ->
                        PostOfficeBox

                    "separator" ->
                        Separator

                    _ ->
                        Other k
            )


nameDecoder : Decoder Name
nameDecoder =
    succeed Name
        |> optional "components" (list nameComponentDecoder) []
        |> optional "isOrdered" bool False
        |> optional "defaultSeparator" (map Just string) Nothing
        |> optional "full" (map Just string) Nothing
        |> optional "sortAs" sortAsDecoder []
        |> optional "phoneticScript" (map Just string) Nothing
        |> optional "phoneticSystem" (map Just string) Nothing


{-| A Name object as defined in RFC 9553.
-}
type alias Name =
    { components : List NameComponent
    , isOrdered : Bool
    , defaultSeparator : Maybe String
    , full : Maybe String
    , sortAs : List ( NameComponentKind, String )
    , phoneticScript : Maybe String
    , phoneticSystem : Maybe String
    }


sortAsDecoder : Decoder (List ( NameComponentKind, String ))
sortAsDecoder =
    dict string
        |> andThen
            (\kindToName ->
                let
                    folder : ( String, String ) -> ( List String, List ( NameComponentKind, String ) ) -> ( List String, List ( NameComponentKind, String ) )
                    folder ( ks, n ) ( iv, v ) =
                        case nameKindFromStr ks of
                            Just r ->
                                ( iv, ( r, n ) :: v )

                            Nothing ->
                                ( ks :: iv, v )

                    ( invalid, valid ) =
                        Dict.toList kindToName
                            |> List.foldl folder ( [], [] )
                in
                if invalid == [] then
                    succeed valid

                else
                    fail ("Unknown name component kinds: " ++ String.join ", " invalid)
            )


{-| A NameComponent object as defined in RFC 9553.
-}
type alias NameComponent =
    { kind : NameComponentKind
    , value : String
    , phonetic : Maybe String
    }


nameComponentDecoder : Decoder NameComponent
nameComponentDecoder =
    succeed NameComponent
        |> required "kind" nameKindDecoder
        |> required "value" string
        |> optional "phonetic" (map Just string) Nothing


{-| The kind of the name component.
-}
type NameComponentKind
    = Title
    | Given
    | Given2
    | Surname
    | Surname2
    | Credential
    | Generation
    | NameComponentSeparator


nameKindDecoder : Decoder NameComponentKind
nameKindDecoder =
    string
        |> andThen
            (\k ->
                case nameKindFromStr k of
                    Just n ->
                        succeed n

                    Nothing ->
                        fail ("Unknown name component kind: " ++ k)
            )


nameKindFromStr : String -> Maybe NameComponentKind
nameKindFromStr =
    \k ->
        case k of
            "title" ->
                Just Title

            "given" ->
                Just Given

            "given2" ->
                Just Given2

            "surname" ->
                Just Surname

            "surname2" ->
                Just Surname2

            "credential" ->
                Just Credential

            "generation" ->
                Just Generation

            "separator" ->
                Just NameComponentSeparator

            _ ->
                Nothing
