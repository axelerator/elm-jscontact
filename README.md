![Test Status](https://github.com/axelerator/elm-jscontact/actions/workflows/test.yml/badge.svg)

# JSON conversion for JSContact cards (RFC 9553)

This packages provides a `record` type (and subtypes) for JSContact cards as well as a JSON
decoder and encoder.

The spec is [here](https://datatracker.ietf.org/doc/html/rfc9553#name-card).
It's quite extensive and not fully implemented yet.
That means concretely that some of the fields (calendars, resource properties, localization) are not
implemented yet and some of the constraints are not fully enforced.

Feedback and contributions are very welcome!
