## eurl_codec

URL-encoded binary string encoding and decoding for erlang. See eunit tests in [`eurl.erl`](https://github.com/drfloob/eurl_codec/blob/master/src/eurl.erl) for usage examples.

### Example

From test suite.

```erlang
%% decodes percent-encoded values only, leaving html entities
<<"&#31616;&#20307;&#20013;&#25991;">> = eurl:percent_decode(<<"%26%2331616%3B%26%2320307%3B%26%2320013%3B%26%2325991%3B">>),

%% decodes character entity references/unicode codepoints from a percent-decoded string
<<"简体中文">> = eurl:entity_decode(<<"&#31616;&#20307;&#20013;&#25991;">>),

%% shortcut to doing both at once
<<"简体中文">> = eurl:decode(<<"%26%2331616%3B%26%2320307%3B%26%2320013%3B%26%2325991%3B">>.
```

### Thanks

Unicode implementation test strings were taken from [que que](http://stackoverflow.com/a/2562809)'s work.
