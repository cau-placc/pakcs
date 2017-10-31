%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module Time:
%

:- module(prim_time,
	  [prim_getClockTime/1,
	   prim_toCalendarTime/2, prim_toUTCTime/2,prim_toClockTime/2]).

:- (current_module(prologbasics) -> true ; use_module('../prologbasics')).

prim_getClockTime('Time.CTime'(CTime)) :- currentClockTime(CTime).

prim_toCalendarTime('Time.CTime'(ClockTime),
		    'Time.CalendarTime'(Year,Month,Day,Hour,Min,Sec,TZ)) :-
	clocktime2localtime(ClockTime,Year,Month,Day,Hour,Min,Sec,TZ).

prim_toUTCTime('Time.CTime'(ClockTime),
	       'Time.CalendarTime'(Year,Month,Day,Hour,Min,Sec,0)) :-
	clocktime2utctime(ClockTime,Year,Month,Day,Hour,Min,Sec).

prim_toClockTime('Time.CalendarTime'(Year,Month,Day,Hour,Min,Sec,TZ),
		 'Time.CTime'(CTime)) :-
	date2clocktime(Year,Month,Day,Hour,Min,Sec,TZ,CTime).
