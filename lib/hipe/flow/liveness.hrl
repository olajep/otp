-compile({inline, [liveset_member/2, liveset_to_list/1]}).
-compile({nowarn_unused_function, [liveset_member/2, liveset_to_list/1]}).

-ifdef(LIVENESS).
-spec liveset_member(V, ?LIVENESS:liveset(E)) -> boolean() when V :: E.
-spec liveset_to_list(?LIVENESS:liveset(E)) -> [E].
-endif.

%% liveset_member(E, S) -> maps:is_key(E, S).
liveset_member(E, S) -> ordsets:is_element(E, S).

%% liveset_to_list(S) -> maps:keys(S).
liveset_to_list(S) -> S.
