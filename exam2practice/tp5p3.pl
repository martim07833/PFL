%class(Course, ClassType, DayOfWeek, Time, Duration)

class(pfl, t, '2 Tue', 15, 2).
class(pfl, tp, '2 Tue', 10.5, 2).
class(lbaw, t, '3 Wed', 10.5, 2).
class(lbaw, tp, '3 Wed', 8.5, 2).
class(ipc, t, '4 Thu', 14.5, 1.5).
class(ipc, tp, '4 Thu', 16, 1.5).
class(fsi, t, '1 Mon', 10.5, 2).
class(fsi, tp, '5 Fri', 8.5, 2).
class(rc, t, '5 Fri', 10.5, 2).
class(rc, tp, '1 Mon', 8.5, 2).

same_day(Course, Course2) :-
	class(Course, _, Day, _, _),
	class(Course2, _, Day, _, _),
	Course @< Course2.

daily_courses(Day, Courses) :-
	findall(Course, class(Course, _, Day, _, _), List),
	sort(List, Courses).

short_classes(L) :-
	findall(Course, (class(Course, _, _, _, Time), Time < 2), List),
	sort(List, L).

course_classes(Course, Classes) :-
	findall(Day/Time-Type, class(Course, Type, Day, Time, _), List),
	sort(List, Classes).


courses(L) :-
	findall(Course, class(Course, _,_,_,_), List),
	sort(List, L).




schedule :-
	findall(Course-Type-Time-Duration, class(Course, Type, '1 Mon', Time, Duration), Monday),
	findall(Course2-Type2-Time2-Duration2, class(Course2, Type2, '2 Tue', Time2, Duration2), Tuesday),
	findall(Course3-Type3-Time3-Duration3, class(Course3, Type3, '3 Wed', Time3, Duration3), Wednesday),
	findall(Course4-Type4-Time4-Duration4, class(Course4, Type4, '4 Thu', Time4, Duration4), Thursday),
	findall(Course5-Type5-Time5-Duration5, class(Course5, Type5, '5 Fri', Time5, Duration5), Friday),
	sort(Monday, Mon),
	sort(Tuesday, Tue),
	sort(Wednesday, Wed),
	sort(Thursday, Thu),
	sort(Friday, Fri),
	format('Monday: ~w~n', [Mon]),
	format('Tuesday: ~w~n', [Tue]),
	format('Wednesday: ~w~n', [Wed]),
	format('Thursday: ~w~n', [Thu]),
	format('Friday: ~w~n', [Fri]).


find_class :-
	write('Enter Day: '),
	read(DayInput),

	write('Enter Time: '),
	read(Time),

	(
		class(Course, Type, DayInput, Time, Duration) 
	->  format('Class starting at ~w:~n', [Time]),
		format('Course: ~w (~w)~n', [Course, Type]),
		format('Start time: ~w~n', [Time]),
		format('Duration: ~w hours~n', [Duration]);

		find_ongoing_class(DayInput, Time, Course, Type, StartTime, Duration) 
	->  format('Class taking place at ~w (started at ~w): ~n', [Time, StartTime]),
		format('Course: ~w (~w) ~n', [Course, Type]),
		format('Started at: ~w~n', [StartTime]),
		format('Duration: ~w hours ~n', [Duration]),
		EndTime is StartTime + Duration;		

		format('No classes are taking place on ~w at time ~w. ~n', [DayInput, Time])
	).


string_to_number(String, Number) :-
	(	number_string(Number, String)
	-> true
	;
	atom_number(String, Number)
	).



find_ongoing_class(Day, CurrentTime, Course, Type, StartTime, Duration) :-
	class(Course, Type, Day, StartTime, Duration),
	StartTime < CurrentTime,
	EndTime is StartTime + Duration,
	CurrentTime < EndTime.
