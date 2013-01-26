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
-export([decode/1, decode/2, percent_decode/1, entity_decode/1, entity_decode/2]).


%% Decodes and resolved character entity references for url-encoded
%% binaries
%% 
%% Example: <<"ユ">> = eurl:decode(<<"%26%2312518%3B">>).
%% Example: <<"ユ">> = eurl:decode(<<"%26%2312518%3B">>, utf8).
decode(Bin) when is_binary(Bin) ->
    entity_decode(percent_decode(Bin)).
decode(Bin, Encoding) when is_binary(Bin) ->
    entity_decode(percent_decode(Bin), Encoding).



percent_decode(Bin) ->
    percent_decode(Bin, <<>>).

percent_decode(<<$%, Hi, Lo, Rest/binary>>, Acc) ->
    Chr = decode_chr([Hi, Lo]),
    percent_decode(Rest, <<Acc/binary, Chr>>);
percent_decode(<<$+, Rest/binary>>, Acc) ->
    percent_decode(Rest, <<Acc/binary, $ >>);
percent_decode(<<X:8, Rest/binary>>, Acc) ->
    percent_decode(Rest, <<Acc/binary, X>>);
percent_decode(<<>>, Acc) ->
    Acc.




%% Decodes html entity references from `eurl:decoded`ed binary-strings
%% and encodes the result into the specified Encoding (utf8 by
%% default).
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
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_chr(Code=[Hi, Lo]) ->
    decode_chr2(is_hex_string(Code), Code).

decode_chr2(true, Code) ->
    httpd_util:hexlist_to_integer(Code);
decode_chr2(false, Code) ->
    error({invalid_percent_encoded_value, list_to_binary(Code)}).


is_hex_string(Code) ->
    lists:all(fun is_hex_char/1, Code).

is_hex_char(X) when $0 =< X, X =< $9 -> true;
is_hex_char(X) when $A =< X, X =< $F -> true;
is_hex_char(X) when $a =< X, X =< $f -> true;
is_hex_char(_) -> false.
 
    
		      



find_entity_code(Bin) ->
    find_entity_code(Bin, <<>>).


find_entity_code(<<$;, Rest/binary>>, Acc) ->
    {Acc, Rest};
find_entity_code(<<X:8, Rest/binary>>, Acc) ->
    find_entity_code(Rest, <<Acc/binary, X>>);
find_entity_code(<<>>, _Acc) ->
    error({unterminated_entity_reference, _Acc}).



entity_code_to_chr(Code, Encoding) ->
    %% io:format("entity_code_to_chr: ~p~n", [Code]),
    Chr = try list_to_integer(binary_to_list(Code))
	  catch error:badarg -> error({invalid_entity_reference, Code})
	  end,
    unicode:characters_to_binary([Chr], unicode, Encoding).

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


percent_decode_test_() ->
    [
     ?_assertEqual(<<"a">>, eurl:percent_decode(<<"a">>))
     , ?_assertEqual(<<>>, eurl:percent_decode(<<>>))
     , ?_assertMatch(<<"arst&#12518;">>, eurl:percent_decode(<<"arst%26%2312518%3B">>))
     , ?_assertMatch(<<"&#31616;&#20307;&#20013;&#25991;">>, eurl:percent_decode(<<"%26%2331616%3B%26%2320307%3B%26%2320013%3B%26%2325991%3B">>))
     , ?_assertMatch(<<"&#931;&#8050; &#947;&#957;&#969;&#961;&#943;&#950;&#969; &#7936;&#960;&#8056;">>, eurl:percent_decode(<<"%26%23931%3B%26%238050%3B+%26%23947%3B%26%23957%3B%26%23969%3B%26%23961%3B%26%23943%3B%26%23950%3B%26%23969%3B+%26%237936%3B%26%23960%3B%26%238056%3B">>))
     , ?_assertMatch(<<"&#3649;&#3612;&#3656;&#3609;&#3604;&#3636;&#3609;&#3630;&#3633;&#3656;&#3609;&#3648;&#3626;&#3639;&#3656;&#3629;&#3617;&#3650;&#3607;&#3619;&#3617;&#3649;&#3626;&#3609;&#3626;&#3633;&#3591;&#3648;&#3623;&#3594;">>, eurl:percent_decode(<<"%26%233649%3B%26%233612%3B%26%233656%3B%26%233609%3B%26%233604%3B%26%233636%3B%26%233609%3B%26%233630%3B%26%233633%3B%26%233656%3B%26%233609%3B%26%233648%3B%26%233626%3B%26%233639%3B%26%233656%3B%26%233629%3B%26%233617%3B%26%233650%3B%26%233607%3B%26%233619%3B%26%233617%3B%26%233649%3B%26%233626%3B%26%233609%3B%26%233626%3B%26%233633%3B%26%233591%3B%26%233648%3B%26%233623%3B%26%233594%3B">>))
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


decode_test_() ->
    [
     ?_assertEqual(<<"a">>, eurl:decode(<<"a">>))
     , ?_assertEqual(<<>>, eurl:decode(<<>>))
     , ?_assertMatch(<<"arstユ">>, eurl:decode(<<"arst%26%2312518%3B">>))
     , ?_assertMatch(<<"简体中文">>, eurl:decode(<<"%26%2331616%3B%26%2320307%3B%26%2320013%3B%26%2325991%3B">>))
     , ?_assertMatch(<<"Σὲ γνωρίζω ἀπὸ">>, eurl:decode(<<"%26%23931%3B%26%238050%3B+%26%23947%3B%26%23957%3B%26%23969%3B%26%23961%3B%26%23943%3B%26%23950%3B%26%23969%3B+%26%237936%3B%26%23960%3B%26%238056%3B">>))
     , ?_assertMatch(<<"แผ่นดินฮั่นเสื่อมโทรมแสนสังเวช">>, eurl:decode(<<"%26%233649%3B%26%233612%3B%26%233656%3B%26%233609%3B%26%233604%3B%26%233636%3B%26%233609%3B%26%233630%3B%26%233633%3B%26%233656%3B%26%233609%3B%26%233648%3B%26%233626%3B%26%233639%3B%26%233656%3B%26%233629%3B%26%233617%3B%26%233650%3B%26%233607%3B%26%233619%3B%26%233617%3B%26%233649%3B%26%233626%3B%26%233609%3B%26%233626%3B%26%233633%3B%26%233591%3B%26%233648%3B%26%233623%3B%26%233594%3B">>))

     %% explicit encoding
     , ?_assertEqual(<<"a">>, eurl:decode(<<"a">>, utf8))
     , ?_assertEqual(<<>>, eurl:decode(<<>>, utf8))
     , ?_assertMatch(<<"arstユ">>, eurl:decode(<<"arst%26%2312518%3B">>, utf8))
     , ?_assertMatch(<<"简体中文">>, eurl:decode(<<"%26%2331616%3B%26%2320307%3B%26%2320013%3B%26%2325991%3B">>, utf8))
     , ?_assertMatch(<<"Σὲ γνωρίζω ἀπὸ">>, eurl:decode(<<"%26%23931%3B%26%238050%3B+%26%23947%3B%26%23957%3B%26%23969%3B%26%23961%3B%26%23943%3B%26%23950%3B%26%23969%3B+%26%237936%3B%26%23960%3B%26%238056%3B">>, utf8))
     , ?_assertMatch(<<"แผ่นดินฮั่นเสื่อมโทรมแสนสังเวช">>, eurl:decode(<<"%26%233649%3B%26%233612%3B%26%233656%3B%26%233609%3B%26%233604%3B%26%233636%3B%26%233609%3B%26%233630%3B%26%233633%3B%26%233656%3B%26%233609%3B%26%233648%3B%26%233626%3B%26%233639%3B%26%233656%3B%26%233629%3B%26%233617%3B%26%233650%3B%26%233607%3B%26%233619%3B%26%233617%3B%26%233649%3B%26%233626%3B%26%233609%3B%26%233626%3B%26%233633%3B%26%233591%3B%26%233648%3B%26%233623%3B%26%233594%3B">>, utf8))
].


invalid_encoding_test_() ->
    [
     ?_assertError({invalid_percent_encoded_value, <<"%2">>}, eurl:percent_decode(<<"%%2312518%3B">>))
     , ?_assertError({invalid_entity_reference, <<"1251A">>}, eurl:entity_decode(<<"&#1251A;">>))
     , ?_assertError({unterminated_entity_reference, <<"1251">>}, eurl:entity_decode(<<"&#1251">>))

     %% both unterminated and invalid; unterminated is found first
     , ?_assertError({unterminated_entity_reference, <<"1251A">>}, eurl:entity_decode(<<"&#1251A">>))

     %% ensure the shortcut method works identically
     , ?_assertError({invalid_percent_encoded_value, <<"%2">>}, eurl:decode(<<"%%2312518%3B">>))
     , ?_assertError({invalid_entity_reference, <<"1251A">>}, eurl:decode(<<"&#1251A;">>))
     , ?_assertError({unterminated_entity_reference, <<"1251">>}, eurl:decode(<<"&#1251">>))
    ].

dummy_test_() ->
    %% tl;dr: I crash
    [?_assertError(_, eurl:decode(1))
     , ?_assertError(_, eurl:decode("a"))

     , ?_assertError(_, eurl:entity_decode(1))
     , ?_assertError(_, eurl:entity_decode("a"))

    ].



-endif.
