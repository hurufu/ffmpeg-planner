:- use_module('AceRules/engine/acerules_processor').
:- use_module(fol).
:- use_module(library(readutil)).

main(InFileName) :-
    read_file_to_codes(InFileName, Codes, []),
    generate_output(Codes, court, [maxanswers=100], _, _, _, OutAnswerText, [OutPnf]),
    format(user_error, "~w~n", [OutAnswerText]),
    pnf_pddl(OutPnf, ProblemDefinition),
    format("~s~n", ProblemDefinition).
