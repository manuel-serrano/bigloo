(module __openpgp-packets
   (export
    (class PGP-Packet)
    (abstract-class PGP-Session-Key-Packet::PGP-Packet)
    (final-class
	  PGP-Public-Key-Encrypted-Session-Key-Packet::PGP-Session-Key-Packet
       version::long
       id::bstring ;; might be "00000000" for wild-card
       algo::symbol
       encrypted-session-key) ;; m**e for RSA, (g**k . m*y**k) for ElGamal
    ;; A PGP-Signature-Packet has redundant information in it.
    ;; For instance the signed-packet-prefix contains the 'version',
    ;; etc. However the signed-packet-prefix and the version are not
    ;; necessarily in sync. Just be careful.
    (class PGP-Sig-Packet::PGP-Packet
       version::long
       signature-type::symbol
       issuer ;::bstring or #f
       public-key-algo::symbol
       hash-algo::symbol)
    (class PGP-Signature-Packet::PGP-Sig-Packet
       creation-date::date
       signature  ;; m**d for RSA, (r . s) for DSA
       signed-packet-prefix::bstring
       hash-trailer::bstring ;; as suffix to the message
       left-hash::bstring)
    (final-class PGP-Signature-v3-Packet::PGP-Signature-Packet)
    (final-class PGP-Signature-v4-Packet::PGP-Signature-Packet
       secure-sub-packets::pair-nil
       insecure-sub-packets::pair-nil)
    (final-class
	  PGP-Symmetric-Key-Encrypted-Session-Key-Packet::PGP-Session-Key-Packet
       version::long
       algo::symbol
       s2k
       ;; encrypted-session-key might be #f in which case the result of s2k is
       ;; the session key.
       encrypted-session-key)
    (final-class PGP-One-Pass-Signature-Packet::PGP-Sig-Packet
       contains-nested-sig?::bool)
    (class PGP-Key-Packet::PGP-Packet
       (id (default #f)) ;; just to avoid recomputations.
       version::long
       (subkey?::bool (default #f))
       algo::symbol
       creation-date::date
       valid-days ;; int for v3 (days or 0 for infinite). #f for v4
       key) ;; key depends on used algorithm.
    (final-class PGP-Public-Key-Packet::PGP-Key-Packet)
    (final-class PGP-Secret-Key-Packet::PGP-Key-Packet
       password-protected-secret-key-data::bstring)
    (wide-class PGP-Secret-Key-Decoded-Packet::PGP-Secret-Key-Packet
       secret-key) ;; native secret key.
    (final-class PGP-Compressed-Packet::PGP-Packet
       packets::pair-nil)
    (class PGP-Symmetrically-Encrypted-Packet::PGP-Packet
       data::bstring)
    (final-class PGP-Marker-Packet::PGP-Packet)
    (final-class PGP-Literal-Packet::PGP-Packet
       format::symbol
       for-your-eyes-only?::bool ;; sensitive.
       file-name ;; #f if 'for-your-eyes-only?'; bstring otherwise
       creation-date::date
       data::bstring)
    (final-class PGP-Trust-Packet::PGP-Packet)
    (final-class PGP-ID-Packet::PGP-Packet
       data::bstring)
    (final-class PGP-Attribute-Packet::PGP-Packet
       data::bstring)
    (final-class PGP-MDC-Symmetrically-Encrypted-Packet::PGP-Symmetrically-Encrypted-Packet
       version::long) ;; note: as of rfc4880 the only value is 1 (and not 4)
    (final-class PGP-MDC-Packet::PGP-Packet
       hash::bstring)

    (class PGP-Signature-Sub-Packet
       (critical?::bool (default #f)))
    ;; The following class is used for all packets that are not yet handled by
    ;; the decoder.
    (final-class PGP-Signature-Sub-Generic::PGP-Signature-Sub-Packet
       type::symbol
       data::bstring)
    (final-class PGP-Signature-Sub-Creation-Time::PGP-Signature-Sub-Packet
       creation-date::date)
    (final-class PGP-Signature-Sub-Expiration-Time::PGP-Signature-Sub-Packet
       expiration-date::date)
    (final-class PGP-Signature-Sub-Exportable::PGP-Signature-Sub-Packet
       exportable?::bool)
    (final-class PGP-Signature-Sub-Trust::PGP-Signature-Sub-Packet
       level::int
       amount::int)
    (final-class PGP-Signature-Sub-Revocable::PGP-Signature-Sub-Packet
       revocable?::bool)
    (final-class PGP-Signature-Sub-Key-Expiration-Time::PGP-Signature-Sub-Packet
       expiration-time::long) ;; in seconds or 0 for infinite
    (final-class PGP-Signature-Sub-Preferred-Symmetric::PGP-Signature-Sub-Packet
       algos::pair-nil)
    (final-class PGP-Signature-Sub-Revocation::PGP-Signature-Sub-Packet
       clazz::byte
       sensitive?::bool ;; redundant, as it's encoded inside 'class'
       algid::byte ;; no idea what this is
       fingerprint::bstring)
    (final-class PGP-Signature-Sub-ID::PGP-Signature-Sub-Packet
       key-id::bstring)
    (final-class PGP-Signature-Sub-Notation::PGP-Signature-Sub-Packet
       flags::bstring ;; length 4
       name::bstring value::bstring)
    (final-class PGP-Signature-Sub-Preferred-Hash::PGP-Signature-Sub-Packet
       algos::pair-nil)
    (final-class PGP-Signature-Sub-Preferred-Compression::PGP-Signature-Sub-Packet
       algos::pair-nil)
    (final-class PGP-Signature-Sub-Preferred-Key-Server::PGP-Signature-Sub-Packet
       server::bstring)
    (final-class PGP-Signature-Sub-Primary-ID::PGP-Signature-Sub-Packet
       primary?::bool)
    (final-class PGP-Signature-Sub-Policy::PGP-Signature-Sub-Packet
       url::bstring)
    (final-class PGP-Signature-Sub-Signer-ID::PGP-Signature-Sub-Packet
       id::bstring)
    (final-class PGP-Signature-Sub-Revocation-Reason::PGP-Signature-Sub-Packet
       code::symbol
       reason::bstring)
    ))
