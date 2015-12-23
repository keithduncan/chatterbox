# Chatterbox

Chatterbox is a chat message router, it allows your infrastructure to post
messages to topics and have them delivered to subscribers across multiple chat
platforms such as Campfire, Slack or IRC.

# TODO

- [ ] Authentication Systems
  - [ ] GitHub SHA1 HMAC for webhooks
  - [ ] For everything else, something signature oriented rather than a transmitted shared secret (aka Basic Auth)
    - [ ] asymmetric key-pair preferred over shared secret (aka HMAC)?
    - [ ] <https://tools.ietf.org/html/draft-cavage-http-signatures-05>?
      - Don't fall into the trap of trusting the given algorithm key per https://auth0.com/blog/2015/03/31/critical-vulnerabilities-in-json-web-token-libraries/

          The section on [verifying](https://tools.ietf.org/html/draft-cavage-http-signatures-05#section-2.5)
          actually proposes you use the given algorithm. Instead the `keyId`
          field should functionally determine the algorithm to use.

          I'm torn on whether the `algorithm` field should even exist, I like it
          because it self-describes what the signature is, but I dislike it
          because it is too likely an implementer will trust the field's value.

- [ ] Define serialisation formats -> Message decodings
  - [x] text/plain
  - [ ] application/json
  - [ ] text/html
  - [ ] GitHub WebHook

- [x] hworker implementation

- [ ] subscription management
  - [ ] HTTP API
  - [x] database storage
  - [x] auto-expiring subscription of an adapter to a topic

- [ ] database migrate executable

- [ ] chat adapters
  - [ ] campfire
  - [ ] slack
  - [ ] irc

- [ ] chatterbox-say
  - [ ] command line client to the server side API
