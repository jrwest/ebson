## ebson

ebson currently has several incomplete features:

    * provide access to ebson_get through ebson module
      * and write corresponding property tests
    * Parse and Encode the following types of values:
      * undefined - 0x06
      * ObjectId - 0x07
      * Regular Exp. - 0x0B
      * DBPointer - 0x0C
      * Javascript Code - 0x0D
      * Symbol - 0x0E
      * Javascript Code w/ Scope - 0x0F
      * Timestamp - 0x11
      * Min/Max Keys - 0xFF/0x7F
    * proplist support for ebson_get
    * rename ebson_get to ebson_fetch
    * support to use dictionaries instead of proplists?
    * ebson_write
      * support for creating documents as proplists
      * support for creating documents as binaries?
      * support for accessing/updating/deleting docuent contents
        * proplist support
	* binary support?


