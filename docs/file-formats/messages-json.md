# Messages JSON Schema

The Messages JSON Schema is used to provide a way to generate the messages data header and also the
message catalogues and ensure they remain in sync.

The schema definition for validation purposes is in
[/schema/messages.schema.json](../../schema/messages.schema.json).  This document provides an
overview of how interpret the schema.

## Sets

The top-level object is an array of set objects.  Each set contains the following attributes:

| Attribute  |  Type  |  Description  |
| :--------- | :----- | :------------ |
| set | String | C/C++ identifier for set.  |
| id  | Positive integer | Numeric identifier for set. |
| messages | Array | Array of message objects - see below.  |

Set 'set' and 'id' identifiers must be unique within a file.

## Messages

The 'messages' array within a set contains objects with the following attributes.

| Attribute  |  Type  |  Description  |
| :--------- | :----- | :------------ |
| msg | String | C/C++ identifier for message.  |
| id  | Positive integer | Numeric identifier for message. |
| description | String | Long form description of the message.  Describe how it is used, what any parameters are, so that translators have a good chance of being able to translate. |
| C | String | The fall-back value of the message for the C/POSIX locale. |
| \<lang\>_\<terr\> | String | The value of the message in the given language and territory. |
| \<lang\> | String | The value of the message in the given language. |

When doing message lookups for a particular locale we look for the \<lang\>_\<terr\> message first.
If that is not present we fallback to the \<lang\> message, or finally the 'C' message.

It is not an error to not have a translation for every single message in a particular locale.

The 'msg' and 'id' identifiers must be unique within a file.
