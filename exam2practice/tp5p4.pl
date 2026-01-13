%flight(origin, destination, company, code, hour, duration)
flight(porto, lisbon, tap, tp1949, 1615, 60).
flight(lisbon, madrid, tap, tp1018, 1805, 75).
flight(lisbon, paris, tap, tp440, 1810, 150).
flight(lisbon, london, tap, tp1366, 1955, 165).
flight(london, lisbon, tap, tp1361, 1630, 160).
flight(porto, madrid, iberia, ib3095, 1640, 80).
flight(madrid, porto, iberia, ib3094, 1545, 80).
flight(madrid, lisbon, iberia, ib3106, 1945, 80).
flight(madrid, paris, iberia, ib3444, 1640, 125).
flight(madrid, london, iberia, ib3166, 1550, 145).
flight(london, madrid, iberia, ib3163, 1030, 140).
flight(porto, frankfurt, lufthansa, lh1177, 1230, 165).


get_all_nodes(ListOfAirports) :-
	findall(Node, flight(Node, _,_,_,_,_), Nodes1),
	findall(Node, flight(_,Node,_,_,_,_), Nodes2),
	append(Nodes1, Nodes2, NodesFinal),
	sort(NodesFinal, ListOfAirports).




most_diversified(Company) :-
	findall(C, flight(_,_,C,_,_,_), C1),
	sort(C1, Companies),
	
	findall(Diversification-Company, 
	(member(Company, Companies),
	calculate_diversification(Company, Diversification)),
	CompanyDiversifications),

	findall(MaxDiv,
	(member(MaxDiv-_, CompanyDiversifications)),
	Diversifications),
	max_list(Diversifcations, MaxDiversification),

	member(MaxDiversification-Company, CompanyDiversifications).


calculate_diversification(Company, Diversification) :-
	findall(City, (flight(City, _, Company, _, _, _); flight(_, City, Company, _,_,_)), Cities),
	sort(Cities, UniqueCities),
	length(UniqueCities, Diversifcation).


find_flights(Origin, Destination, ListOfFlights) :-
	findall(Code, flight(Origin, Destination, _, Code, _, _), Codes),
	sort(Codes, ListOfFlights).

find_flights_bfs(Origin, Destination, Flights) :-
	Origin == Destination,
	!,
	Flights = [].



find_flights_bfs(Origin, Destination, Flights) :-
	bfs([[Origin]], Destination, Paths),
	extract_flgiht_codes(Paths, Flights).


