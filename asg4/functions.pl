/*
* Matthew Tan
* mxtan
* cs112
* asg4: functions.pl
*/

haversine_distance(Lat1, Lon1, Lat2, Lon2, Distance_Miles) :-
    Dlon is Lon2 - Lon1,
    Dlat is Lat2 - Lat1,
    TmpA is sin(Dlat / 2) ** 2
       + cos(Lat1) * cos(Lat2) * sin(Dlon / 2) ** 2,
    Unit_Distance is 2 * atan2(sqrt(TmpA), sqrt(1 - TmpA)),
    Distance_Miles is Unit_Distance * 3961.

radians(degmin(Degrees, Minutes), Rads) :-
     Degs is Degrees + Minutes / 60,
     Rads is Degs * pi / 180.

distance(Airport1, Airport2, Distance) :-
    airport(Airport1, _, Lat1, Lon1),
    airport(Airport2, _, Lat2, Lon2),
    radians(Lat1, Lat1_float),
    radians(Lat2, Lat2_float),
    radians(Lon1, Lon1_float),
    radians(Lon2, Lon2_float),
    haversine_distance(Lat1_float, Lon1_float, 
        Lat2_float, Lon2_float, Distance).

time_in_hours(time(Hours, Mins), TimeInHours) :-
    TimeInHours is Hours + Mins / 60.

time_travelled(Miles, Hours) :-
    Hours is Miles / 500.

displayTime(Timedigits) :-
    Timedigits < 10, print(0), print(Timedigits).
displayTime(Timedigits) :-
    Timedigits >= 10, print(Timedigits).

print_time(TimeInHours) :-
    Hours is floor(TimeInHours * 60) // 60,
    Mins is floor(TimeInHours * 60) mod 60,
    displayTime(Hours), 
    print(':'), 
    displayTime(Mins).

% prolog not
not(X) :- X, !, fail.
not(_).

listpath(Node, Node, _, [Node], _).

listpath(Node, End, Tried, 
    [[Node, DepartTimeInHours, ArriveTime] | List], DepartTime) :-
    flight(Node, End, DepartTime),
    not(member(End, Tried)),
    time_in_hours(DepartTime, DepartTimeInHours),
    distance(Node, End, Distance),
    time_travelled(Distance, TravelTime),
    ArriveTime is DepartTimeInHours + TravelTime,
    ArriveTime < 24.0,
    listpath(End, End, [End | Tried], List, _).

listpath(Node, End, Tried, 
    [[Node, DepartTimeInHours, ArriveTime] | List], DepartTime) :-
    flight(Node, Next, DepartTime),
    not(member(Next, Tried)),
    time_in_hours(DepartTime, DepartTimeInHours),
    distance(Node, Next, Distance),
    time_travelled(Distance, TravelTime),
    ArriveTime is DepartTimeInHours + TravelTime,
    ArriveTime < 24.0,
    flight(Next, _, NextDepartTime),
    time_in_hours(NextDepartTime, NextDepartTimeInHours),
    TransitTime is NextDepartTimeInHours - ArriveTime - 0.5,
    TransitTime >= 0,
    listpath(Next, End, [Next | Tried], 
        List, NextDepartTime).

to_upper(Lower, Upper) :-
   atom_chars(Lower, Lowerlist),
   maplist(lower_upper, Lowerlist, Upperlist),
   atom_chars(Upper, Upperlist).

writepath([]) :-
    nl.

writepath([[Depart, DepartTime, ArriveTime], Arrive | []]) :-
    airport(Depart, DepartName, _, _), 
    airport(Arrive, ArriveName, _, _),
    to_upper(Depart, L1),
    to_upper(Arrive, L2),
    write('     '), write('depart  '), 
    write(L1), write('  '),
    write(DepartName), print_time(DepartTime), nl,
    write('     '), write( 'arrive  ' ),
    write(L2), write('  '),
    write(ArriveName), print_time(ArriveTime), nl,
    !, true.

writepath([[Depart, DepartTime, ArriveTime], 
    [Transit, TransitDepartureTime, TransitArrivalTime] | Arrive]) :-
    airport(Depart, DepartName, _, _), 
    airport(Transit, ArriveName, _, _),
    to_upper(Depart, L1),
    to_upper(Transit, L2),
    write('     '), write('depart  '),
    write(L1), write('  '),
    write(DepartName), print_time(DepartTime), nl,
    write('     '), write('arrive  '),
    write(L2), write('  '),
    write(ArriveName), print_time(ArriveTime), nl,
    !, writepath([[Transit, TransitDepartureTime, 
        TransitArrivalTime] | Arrive]).

fly(Depart, Depart) :-
    write('Error: departure: ' ), write(Depart),
    write(', arrival: '), write(Depart), write(' are the same.'),
    nl,
    !, fail.

fly(Depart, Arrive) :-
    listpath(Depart, Arrive, [Depart], List, _),
    !, nl,
    writepath(List),
    true.

fly(Depart, Arrive) :-
    airport(Depart, _, _, _ ),
    airport(Arrive, _, _, _ ),
    to_upper(Depart, L1),
    to_upper(Arrive, L2),
    write('Error: no flight from: '), write(L1),
    write(' to '), write(L2), write('.'),
    !, fail.

fly(_, _) :-
    write('Error: we do not fly to those airports.'), nl,
    !, fail.
