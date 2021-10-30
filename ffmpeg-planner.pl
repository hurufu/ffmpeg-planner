:- use_module('AceRules/engine/acerules_processor').
:- use_module('./pnf_handling.pdb').
:- use_module(library(readutil)).

main(InFileName) :-
    read_file_to_codes(InFileName, Codes, []),
    generate_output(Codes, court, [maxanswers=10], _, _, _, OutAnswerText, [OutPnf]),
    format(user_error, "~w~n~w~n", [OutAnswerText, OutPnf]),
    pnf_pddl(OutPnf, ProblemDefinition),
    format("~s~n", ProblemDefinition).
