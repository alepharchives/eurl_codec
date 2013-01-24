%%%-------------------------------------------------------------------
%%% @author aj <AJ Heller <aj@drfloob.com>>
%%% @copyright (C) 2013, aj
%%%
%%% @doc Performs url (de|en)coding as well as html entity
%%% (de|en)coding on binaries for utf8 support.
%%% 
%%% Test strings from stack overflow's que que (http://stackoverflow.com/a/2562809)
%%%  
%%%
%%% See also: https://en.wikipedia.org/wiki/Url_encode
%%% 
%%% @end
%%% Created : 23 Jan 2013 by aj <AJ Heller <aj@drfloob.com>>
%%%-------------------------------------------------------------------
-module(eurl).
-export([decode/1, encode/1]).
-export([entity_decode/1, entity_decode/2, entity_encode/1]).


%% Decodes url-encoded binaries
decode(Bin) when is_binary(Bin) ->
    decode(Bin, <<>>).

decode(<<$%, Hi, Lo, Rest/binary>>, Acc) ->
    Chr = decode_chr([Hi, Lo]),
    decode(Rest, <<Acc/binary, Chr>>);
decode(<<$+, Rest/binary>>, Acc) ->
    decode(Rest, <<Acc/binary, $ >>);
decode(<<X:8, Rest/binary>>, Acc) ->
    decode(Rest, <<Acc/binary, X>>);
decode(<<>>, Acc) ->
    Acc.




encode(_Bin) ->
    not_done.



%% Decodes html entities from non-url-encoded binaries using the
%% specified encoding (utf8 by default).
%% 
%% Example: <<"ユ">> = entity_decode(<<"&#12518;">>).
entity_decode(DecodedBin) ->
    entity_decode(DecodedBin, utf8, <<>>).
entity_decode(DecodedBin, OutputEncoding) ->
    entity_decode(DecodedBin, OutputEncoding, <<>>).

%% where the work happens
entity_decode(<<"&#", Rest/binary>>, Encoding, Acc) ->
    {Code, Rest2} = find_entity_code(Rest),
    Chr = entity_code_to_chr(Code, Encoding),
    entity_decode(Rest2, Encoding, <<Acc/binary, Chr/binary>>);
entity_decode(<<X:8, Rest/binary>>, Encoding, Acc) ->
    entity_decode(Rest, Encoding, <<Acc/binary, X>>);
entity_decode(<<>>, _, Acc) ->
    Acc.
    

entity_encode(_Bin) ->
    not_done.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_chr(Code) ->
    httpd_util:hexlist_to_integer(Code).


find_entity_code(Bin) ->
    find_entity_code(Bin, <<>>).


find_entity_code(<<$;, Rest/binary>>, Acc) ->
    {Acc, Rest};
find_entity_code(<<X:8, Rest/binary>>, Acc) ->
    find_entity_code(Rest, <<Acc/binary, X>>).


entity_code_to_chr(Code, Encoding) ->
    %% io:format("entity_code_to_chr: ~p~n", [Code]),
    unicode:characters_to_binary([list_to_integer(binary_to_list(Code))], unicode, Encoding).

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


decode_test_() ->
    [
     ?_assertEqual(<<"a">>, decode(<<"a">>))
     , ?_assertEqual(<<>>, decode(<<>>))
     , ?_assertMatch(<<"arst&#12518;">>, decode(<<"arst%26%2312518%3B">>))
     , ?_assertMatch(<<"&#31616;&#20307;&#20013;&#25991;">>, decode(<<"%26%2331616%3B%26%2320307%3B%26%2320013%3B%26%2325991%3B">>))
     , ?_assertMatch(<<"&#931;&#8050; &#947;&#957;&#969;&#961;&#943;&#950;&#969; &#7936;&#960;&#8056;">>, decode(<<"%26%23931%3B%26%238050%3B+%26%23947%3B%26%23957%3B%26%23969%3B%26%23961%3B%26%23943%3B%26%23950%3B%26%23969%3B+%26%237936%3B%26%23960%3B%26%238056%3B">>))
     , ?_assertMatch(<<"&#3649;&#3612;&#3656;&#3609;&#3604;&#3636;&#3609;&#3630;&#3633;&#3656;&#3609;&#3648;&#3626;&#3639;&#3656;&#3629;&#3617;&#3650;&#3607;&#3619;&#3617;&#3649;&#3626;&#3609;&#3626;&#3633;&#3591;&#3648;&#3623;&#3594;">>, decode(<<"%26%233649%3B%26%233612%3B%26%233656%3B%26%233609%3B%26%233604%3B%26%233636%3B%26%233609%3B%26%233630%3B%26%233633%3B%26%233656%3B%26%233609%3B%26%233648%3B%26%233626%3B%26%233639%3B%26%233656%3B%26%233629%3B%26%233617%3B%26%233650%3B%26%233607%3B%26%233619%3B%26%233617%3B%26%233649%3B%26%233626%3B%26%233609%3B%26%233626%3B%26%233633%3B%26%233591%3B%26%233648%3B%26%233623%3B%26%233594%3B">>))
     %% , ?_assertMatch(<<"">>, decode(<<"">>))
].


entity_decode_test_() ->
    [?_assertMatch(<<"ユ">>, eurl:entity_decode(<<"&#12518;">>))
     , ?_assertMatch(<<"arstユ">>, eurl:entity_decode(<<"arst&#12518;">>))
     , ?_assertMatch(<<"简体中文">>, eurl:entity_decode(<<"&#31616;&#20307;&#20013;&#25991;">>))
     , ?_assertMatch(<<"Σὲ γνωρίζω ἀπὸ">>, eurl:entity_decode(<<"&#931;&#8050; &#947;&#957;&#969;&#961;&#943;&#950;&#969; &#7936;&#960;&#8056;">>))
     , ?_assertMatch(<<"แผ่นดินฮั่นเสื่อมโทรมแสนสังเวช">>, eurl:entity_decode(<<"&#3649;&#3612;&#3656;&#3609;&#3604;&#3636;&#3609;&#3630;&#3633;&#3656;&#3609;&#3648;&#3626;&#3639;&#3656;&#3629;&#3617;&#3650;&#3607;&#3619;&#3617;&#3649;&#3626;&#3609;&#3626;&#3633;&#3591;&#3648;&#3623;&#3594;">>))


     %% encoding tests
     , ?_assertMatch(<<"ユ">>, eurl:entity_decode(<<"&#12518;">>, utf8))
     , ?_assertMatch(<<"ユ">>, eurl:entity_decode(<<"&#12518;">>, unicode))
     , ?_assertEqual(unicode:characters_to_binary([12518]), eurl:entity_decode(<<"&#12518;">>, utf8))

     , ?_assertEqual(<<12518:1/integer-unit:16>>, eurl:entity_decode(<<"&#12518;">>, utf16))
     , ?_assertMatch(<<12518:1/integer-unit:32>>, eurl:entity_decode(<<"&#12518;">>, utf32))


     , ?_assertMatch(<<"ユーザ">>, eurl:entity_decode(<<"&#12518;&#12540;&#12470;">>, utf8))
     , ?_assertMatch(<<12518:1/little-integer-unit:16, 12540:2/little-integer-unit:8, 12470:2/little-integer-unit:8>>
			 , eurl:entity_decode(<<"&#12518;&#12540;&#12470;">>, {utf16, little}))
     , ?_assertMatch(<<12518:1/big-integer-unit:16, 12540:1/big-integer-unit:16, 12470:1/big-integer-unit:16>>
			 , eurl:entity_decode(<<"&#12518;&#12540;&#12470;">>, {utf16, big}))
     , ?_assertMatch(<<64321:1/little-integer-unit:32>>
			 , eurl:entity_decode(<<"&#64321;">>, {utf32, little}))
     , ?_assertMatch(<<64321:1/big-integer-unit:32>>
			 , eurl:entity_decode(<<"&#64321;">>, {utf32, big}))

     %% Latin1 doesn't understand this codepoint ...
     , ?_assertError(badarg, eurl:entity_decode(<<"&#12518;">>, latin1))
     %% ... but it undestands these just fine.
     , ?_assertMatch(<<"aj">>, eurl:entity_decode(<<"&#97;&#106;">>, latin1))
    ].


dummy_test_() ->
    [?_assertError(_, eurl:decode(1))
     , ?_assertError(_, eurl:decode("a"))

     , ?_assertError(_, eurl:entity_decode(1))
     , ?_assertError(_, eurl:entity_decode("a"))

    ].
-endif.
