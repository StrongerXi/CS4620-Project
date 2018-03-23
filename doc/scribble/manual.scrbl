#lang scribble/manual

@title{Traveler}

Welcome to the Traveler language documentation.
@author{Ryan Guo and Sam Gupta}


Traveler language is intended for flight plan arrangement. To use the language, one always needs to make two specifications: available database and plan requirements.


@section{Grammar}

@subsection{Overall Grammar}
@(racketgrammar*
   [program data-specs
            plan-specs]
   [time hour:min]
   [data mm/dd/yyyy])


@subsection{Database Grammar}
@(racketgrammar*
   #:literals (database)
   [data-specs (database city-clause ...)]
   [city-clause (id to-clauses ...)]
   [to-clause (id number time date time date id)])

@subsection{Plan-Specification Grammar}
@(racketgrammar*
   #:literals (data-from plan depart-time depart-date wait-time price duration timezone)
   [plan-specs (data-from string) plan-clause ...]
   [plan-clause (plan (id --> id)
                      (timezone number number)
                      constraint 
                      ...)]
   [constraint (depart-time time ~ time)
               (depart-date date ~ date)
               (wait-time number ~ number)
               (price number ~ number)
               (duration number ~ number)])



@section{Database Specification}
@subsection{Sample Program}
@codeblock{
#lang s-exp "traveler.rkt"

database

(Boston (UA1105 17:00 02/08/2018 7:30  02/10/2018 980  Shanghai)
        (SA207  19:00 02/09/2018 13:00 02/10/2018 1500 Beijing))
(Shanghai (SC770  20:30 02/11/2018 0:30  02/12/2018 300 Beijing)
          (JA3600 07:30 02/10/2018 12:40 02/09/2018 670 Tokyo)
          (SC1400 14:20 02/09/2018 18:45 02/09/2018 450 Beijing))
}


@section{Plan Specification}

@subsection{Sample Program}
@codeblock{ 
#lang s-exp "traveler.rkt"

(data-from "testData.rkt")

(plan myPlan
      (Boston --> Beijing)
      (timezone -4 +8)
      (depart-time 10:30 ~ 20:00)
      (depart-date 02/09/2018 ~ 02/10/2018)
      (wait-time 0 ~ 5)
      (price 700 ~ 1300)
      (duration 8 ~ 12))
}


@subsection{Syntaxes}

@defform[(plan id (id --> id) (timezone number number) constraint ...)]
Based on the database specified by the program, look for all the series of flights from
origin to destination that satisfies the all of the constraints. Print out the final result.

@defform[(timezone number number)]
Specifies the origin and destination timezone within a plan requirement.

@defform[(depart-time number number)]
Specifies departure time constraint within a plan requirement

@defform[(depare-date number number)]
Specifies departure date constraint within a plan requirement

@defform[(wait-time number number)]
Specifies constraint for total amount of time spent waiting in between flights within a plan requirement

@defform[(price number number)]
Specifies price constraint range within a plan requirement

@defform[(duration number number)]
Specifies constraint for total amount of time spent flying within a plan requirement
