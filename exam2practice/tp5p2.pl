teaches(adalberto, algorithms).
teaches(bernardete, databases).
teaches(capitolino, compilers).
teaches(diogenes, statistics).
teaches(ermelinda, networks).

attends(alberto, algorithms).
attends(bruna, algorithms).
attends(cristina, algorithms).
attends(diogo, algorithms).
attends(eduarda, algorithms).

attends(antonio, databases).
attends(bruno, databases).
attends(cristina, databases).
attends(duarte, databases).
attends(eduardo, databases).

attends(alberto, compilers).
attends(bernardo, compilers).
attends(clara, compilers).
attends(diana, compilers).
attends(eurico, compilers).

attends(antonio, statistics).
attends(bruna, statistics).
attends(claudio, statistics).
attends(duarte, statistics).
attends(eva, statistics).

attends(alvaro, networks).
attends(beatriz, networks).
attends(claudio, networks).
attends(diana, networks).
attends(eduardo, networks).


teachers(T) :-
	findall(Teacher, teaches(Teacher, _), Teachers),
	sort(Teachers, T).


students_of(Teacher, AllStudents) :-
	findall(Subject, teaches(Teacher, Subject), Subjects),
	findall(Student, (member(Subject, Subjects), attends(Student, Subject)), Students),
	sort(Students, AllStudents).

teachers_of(Student, AllTeachers) :-
	findall(Subject, attends(Student, Subject), Subjects),
	findall(Teacher, (member(Subject, Subjects), teaches(Teacher, Subject)), Teachers),
	sort(Teachers, AllTeachers).


common_courses(S1, S2, C) :-
	S1 \= S2,
	findall(Subject1, attends(S1, Subject1), Subjects1),
	findall(Subject2, attends(S2, Subject2), Subjects2),
	findall(Common, (member(Common, Subjects1), member(Common, Subjects2)), Commons),
	sort(Commons, C).



more_than_one_course(L) :-
	findall(Student,(attends(Student, Course1), attends(Student, Course2), Course1 \= Course2), Students),
	sort(Students, L).


strangers(L) :-
	findall(Student1-Student2, (attends(Student1, Subject1), attends(Student2, Subject2), Subject1 \= Subject2, Student1 \= Student2), Pairs),
	sort(Pairs, L).

good_groups(L) :-
	findall((S1, S2), 
	(attends(S1, C1), attends(S2, C2),
	S1 @< S2,
	C1 \= C2,
	attends(S1,C2), attends(S2, C2)), Pairs),
	sort(Pairs, L).