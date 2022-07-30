-record(header_rec, {field :: atom()}).

-spec bad_fun() -> atom().
bad_fun() -> 1.

no_spec_fun() -> 1.

-type has_unbound() :: _A.

get_check_key() ->
    application:get_env(check, key).
