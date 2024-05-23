module JSContact.Encoder exposing (encode)

{-|

@docs encode

-}

import Dict exposing (Dict)
import JSContact exposing (Address, AddressComponent, AddressComponentKind(..), Addresses, Email, Emails, JSContact, Kind(..), Name, NameComponent, NameComponentKind(..), OnlineService, OnlineServices, Phone, Phones, PreferredLanguage, PreferredLanguages, RelatedTo, RelationType(..))
import Json.Encode exposing (Value, bool, dict, int, list, object, string)
import List exposing (isEmpty)
import Maybe exposing (map)
import Maybe.Extra


{-| Encodes contact card to JSON.
-}
encode : JSContact -> Value
encode contact =
    withOptionals
        [ ( "@type", Just <| string "Card" )
        , ( "kind", Just <| encodeKind contact.kind )
        , ( "version", Just <| string contact.version )
        , ( "uid", Just <| string contact.uid )
        , ( "created", map string contact.created )
        , ( "updated", map string contact.updated )
        , ( "language", map string contact.language )
        , ( "members", Just <| trueMap contact.members )
        , ( "name", map encodeName contact.name )
        , ( "prodId", map string contact.prodId )
        , ( "relatedTo", skipIfEmptyDict encodeRelatedTo contact.relatedTo )
        , ( "emails", skipIfEmptyDict encodeEmails contact.emails )
        , ( "addresses", skipIfEmptyDict encodeAddresses contact.addresses )
        , ( "phones", skipIfEmptyDict encodePhones contact.phones )
        , ( "onlineServices", skipIfEmptyDict encodeOnlineServices contact.onlineServices )
        , ( "preferredLanguages", skipIfEmptyDict encodePreferredLanguages contact.preferredLanguages )
        ]


skipIfEmptyDict : (Dict k v -> Value) -> Dict k v -> Maybe Value
skipIfEmptyDict encoder dict =
    if Dict.isEmpty dict then
        Nothing

    else
        Just <| encoder dict


encodeKind : Kind -> Value
encodeKind kind =
    case kind of
        Individual ->
            string "individual"

        Group ->
            string "group"

        Organization ->
            string "organization"

        Location ->
            string "location"

        Device ->
            string "device"

        Application ->
            string "application"


encodeRelatedTo : RelatedTo -> Value
encodeRelatedTo relatedTo =
    dict identity (\v -> object [ ( "relation", List.map encodeRelationType v |> object ) ]) relatedTo


encodeRelationType : RelationType -> ( String, Value )
encodeRelationType relationType =
    ( relationTypeToString relationType, bool True )


relationTypeToString : RelationType -> String
relationTypeToString relationType =
    case relationType of
        Aquaintance ->
            "aquaintance"

        Agent ->
            "agent"

        Child ->
            "child"

        Coresident ->
            "coresident"

        Coworker ->
            "coworker"

        Colleague ->
            "colleague"

        Contact ->
            "contact"

        Crush ->
            "crush"

        Date ->
            "date"

        Emergency ->
            "emergency"

        Friend ->
            "friend"

        Kin ->
            "kin"

        Me ->
            "me"

        Met ->
            "met"

        Muse ->
            "muse"

        Neighbor ->
            "neighbor"

        Parent ->
            "parent"

        Sibling ->
            "sibling"

        Spouse ->
            "spouse"

        Sweetheart ->
            "sweetheart"


encodeEmails : Emails -> Value
encodeEmails emails =
    dict identity encodeEmail emails


encodeEmail : Email -> Value
encodeEmail email =
    withOptionals
        [ ( "contexts", Just <| trueMap email.contexts )
        , ( "address", Just <| string email.address )
        , ( "pref", map int email.pref )
        ]


encodePhones : Phones -> Value
encodePhones phones =
    dict identity encodePhone phones


encodePhone : Phone -> Value
encodePhone phone =
    withOptionals
        [ ( "contexts", Just <| trueMap phone.contexts )
        , ( "phone", Just <| string phone.phone )
        , ( "pref", map int phone.pref )
        ]


encodeOnlineServices : OnlineServices -> Value
encodeOnlineServices onlineServices =
    dict identity encodeOnlineService onlineServices


encodeOnlineService : OnlineService -> Value
encodeOnlineService service =
    withOptionals
        [ ( "service", map string service.service )
        , ( "user", map string service.user )
        , ( "uri", Just <| string service.uri )
        , ( "contexts", Just <| trueMap service.contexts )
        , ( "pref", map int service.pref )
        ]


encodePreferredLanguages : PreferredLanguages -> Value
encodePreferredLanguages langs =
    dict identity encodePreferredLanguage langs


encodePreferredLanguage : PreferredLanguage -> Value
encodePreferredLanguage lang =
    withOptionals
        [ ( "language", Just <| string lang.language )
        , ( "contexts", Just <| trueMap lang.contexts )
        , ( "pref", map int lang.pref )
        ]


encodeAddresses : Addresses -> Value
encodeAddresses addresses =
    dict identity encodeAddress addresses


encodeAddress : Address -> Value
encodeAddress address =
    withOptionals
        [ ( "contexts", Just <| trueMap address.contexts )
        , ( "components", Just <| list encodeAddressComponent address.components )
        , ( "isOrdered", Just <| bool address.isOrdered )
        , ( "countryCode", map string address.countryCode )
        , ( "coordinates", map string address.coordinates )
        , ( "timeZone", map string address.timeZone )
        , ( "full", map string address.full )
        , ( "defaultSeparator", map string address.defaultSeparator )
        , ( "pref", map int address.pref )
        , ( "phoneticScript", map string address.phoneticScript )
        , ( "phoneticSystem", map string address.phoneticSystem )
        ]


encodeAddressComponent : AddressComponent -> Value
encodeAddressComponent component =
    withOptionals
        [ ( "value", Just <| string component.value )
        , ( "kind", Just <| encodeAddressComponentKind component.kind )
        , ( "phonetic", Maybe.map string component.phonetic )
        ]


encodeAddressComponentKind : AddressComponentKind -> Value
encodeAddressComponentKind kind =
    string <|
        case kind of
            Room ->
                "room"

            Apartment ->
                "apartment"

            Floor ->
                "floor"

            Building ->
                "building"

            StreetNumber ->
                "number"

            AddressName ->
                "name"

            Block ->
                "block"

            Subdistrict ->
                "subdistrict"

            District ->
                "district"

            Locality ->
                "locality"

            Region ->
                "region"

            Postcode ->
                "postcode"

            Country ->
                "country"

            Direction ->
                "direction"

            Landmark ->
                "landmark"

            PostOfficeBox ->
                "postOfficeBox"

            Separator ->
                "separator"

            Other k ->
                k


encodeName : Name -> Value
encodeName name =
    withOptionals
        [ ( "components", Just <| list encodeNameComponent name.components )
        , ( "isOrdered", Just <| bool name.isOrdered )
        , ( "defaultSeparator", map string name.defaultSeparator )
        , ( "full", map string name.full )
        , ( "sortAs"
          , if isEmpty name.sortAs then
                Nothing

            else
                Just <| encodeSortAs name.sortAs
          )
        , ( "phoneticScript", map string name.phoneticScript )
        , ( "phoneticSystem", map string name.phoneticSystem )
        ]


encodeNameComponent : NameComponent -> Value
encodeNameComponent c =
    withOptionals
        [ ( "value", Just <| string c.value )
        , ( "kind", Just <| encodeNameComponentKind c.kind )
        , ( "phonetic", map string c.phonetic )
        ]


encodeSortAs : List ( NameComponentKind, String ) -> Value
encodeSortAs sortAs =
    List.map (\( kind, value ) -> ( nameKindToString kind, string value )) sortAs
        |> Dict.fromList
        |> dict identity identity


encodeNameComponentKind : NameComponentKind -> Value
encodeNameComponentKind kind =
    nameKindToString kind
        |> string


nameKindToString : NameComponentKind -> String
nameKindToString kind =
    case kind of
        Title ->
            "title"

        Given ->
            "given"

        Given2 ->
            "given2"

        Surname ->
            "surname"

        Surname2 ->
            "surname2"

        Credential ->
            "credential"

        Generation ->
            "generation"

        NameComponentSeparator ->
            "nameComponentSeparator"


withOptionals : List ( String, Maybe Value ) -> Value
withOptionals pairs =
    List.map (\( k, v ) -> map (\vv -> ( k, vv )) v) pairs
        |> Maybe.Extra.values
        |> object


trueMap : List String -> Value
trueMap keys =
    List.map (\key -> ( key, bool True )) keys
        |> object
