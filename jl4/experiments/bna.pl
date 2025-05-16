commencement(bna,  1983).
commencement(bcia, 2009).

born_at(alice, 1970).
born_at(bob,   1995).
born_at(carol, 1975).

born_at(dave,  1950).
born_at(eva,   1955).
       
born_in(alice, uk).
born_in(bob,   uk).
born_in(carol, cayman).
born_in(dave,  france).
born_in(eva,   gibraltar).

is_settled_in(dave , uk, Y)        :- Y >  1960.
is_settled_in(carol, uk, Y)        :- Y >  1990.
is_settled_in(eva,   gibraltar, Y) :- Y >= 1955.

parentOf(alice, bob).
parentOf(carol, bob).
parentOf(dave, carol).
parentOf(eva,  carol).

qualifying_territory(cayman, T) :- T >= 1955.
qualifying_territory(gibraltar, T) :- T >= 1955.

byFiat(alice, T) :- T >= 1970.
is_British(P, T) :- byFiat(P, T), format('It is given that ~w is a British citizen, as at ~w.~n', [P, T]).

is_British(Person, At_Time) :-
  born_at(Person, Time_Of_Birth),
  ( At_Time >= Time_Of_Birth -> true
  ; format('~w was born in ~w, after our query time!~n', [Person, Time_Of_Birth]), false),

  ( uk_birth(Person) -> true
  ; (format('~w was not born in the UK after the act commenced.~n', [Person]), false)

  ; qt_birth(Person) -> true;
  (format('~w was not born in a qualifying territory.~n', [Person]), false)
  ) ,

  ( parentOf(Parent, Person)
    -> format('Let\'s consider ~w\'s parent ~w.~n', [ Person, Parent ]),
       ( is_British(Parent, Time_Of_Birth) -> format('the parent ~w is British.~n', [Parent])
       ; (format('~w is not British as at ~w.~n', [ Parent, Time_Of_Birth ]), false)
       ; is_settled_in(Parent, uk, Time_Of_Birth) -> format('~w was settled in the UK at ~w.~n', [ Parent, Time_Of_Birth ])
       ; (format('~w was NOT settled in the UK at ~w.~n', [ Parent, Time_Of_Birth ]), false)
       ; ( qt_birth(Person) -> born_in(Person, QT),
                               is_settled_in(Parent, QT, Time_Of_Birth) )
    )
  ),
  format('We conclude that ~w is British.~n', [Person]).

uk_birth(Person) :-
  born_in(Person, uk),
  born_at(Person, Time_Of_Birth),
  commencement(bna, T_Act_Commencement),
  Time_Of_Birth   > T_Act_Commencement,
  format('~w was born in the UK in ~w, after the Act commenced.~n', [Person, Time_Of_Birth]).

qt_birth(Person) :-
  born_in(Person, QT),
  born_at(Person, Time_Of_Birth),
  qualifying_territory(QT, Time_Of_Birth),
  format('~w was born in qualifying territory ~w in ~w.~n', [Person, QT, Time_Of_Birth]).
