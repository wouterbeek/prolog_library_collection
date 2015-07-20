:- module(
  condorcet,
  [
    go/0,
    go/1, % +VotePredicate:atom
    go/2 % +ScoringPredicate:atom
         % +VotePredicate:atom
  ]
).

/** <module> Condorcet

@author Wouter Beek
@version 2011/09, 2012/09
*/

:- use_module(plc(generics/list_ext)).
:- use_module(plc(generics/meta_ext)).

:- dynamic(runner_opponent_result/3).
:- dynamic(runner_result/2).

:- meta_predicate condorcet_matrix(4,2).
:- meta_predicate go(2).
:- meta_predicate go(4,2).
:- meta_predicate margins_score(+,+,-,2).
:- meta_predicate number_of_votes_ranking_former_above_latter(+,+,-,2).
:- meta_predicate pairwise_opposition_score(+,+,-,2).
:- meta_predicate winning_votes_score(+,+,-,2).



% SPECIFICS

contestant(run).
contestant(rug).
contestant(um).
contestant(uva).
contestant(vu).

% From highest ranked (left) to lowest ranked (right).
%%%%vote0(run, [um,  rug,  uva, vu ]).
vote0(rug, [uva, run, um,  vu ]).
vote0(run, [rug, um,  uva, vu ]).
vote0(um,  [rug, vu,  run, uva]).
vote0(uva, [um,  rug, vu,  run]).
vote0(vu,  [run, um,  rug, uva]).



% GENERICS

clear:-
  retractall(runner_opponent_result(_Runner1, _Opponent, _RunnerOpponentScore)),
  retractall(runner_result(_Runner2, _RunnerScore)).

go:-
  go(vote0).

%! go(+VotePredicate:atom) is det.
% E.g. vote0/2.

go(VotePredicate):-
  write('Using margins score'),nl,
  go(margins_score, VotePredicate),
  nl,nl,write('Using pairwise opposition score'),nl,
  go(pairwise_opposition_score, VotePredicate),
  nl,nl,write('Using winning votes score'),nl,
  go(winning_votes_score, VotePredicate).

go(ScoringPredicate, VotePredicate):-
  % Clear any intermediate results.
  clear,
  condorcet_matrix(ScoringPredicate, VotePredicate),
  runner_results,
  write_condorcet_matrix,
  write_runner_scores,
  (
    % There is a unique Copeland winner.
    copeland_winner(CopelandWinner)
  ->
    format('The Copeland winner is %w.\n', [CopelandWinner])
  ;
    % There is a unique Copeland loser.
    copeland_loser(CopelandLoser)
  ->
    format('Remove Copeland loser %w and iterate.\n', [CopelandLoser])
  ;
    loser(Loser)
  ->
    format('Remove non-Copeland loser %w and iterate.\n', [Loser])
  ).

condorcet_matrix(ScoreMethod, DB):-
  forall(
    contestant(Runner),
    forall(
      (
        contestant(Opponent),
        \+(Opponent == Runner)
      ),
      (
        call(ScoreMethod, Runner, Opponent, RunnerOpponentScore, DB),
        assert(
          runner_opponent_result(
            Runner,
            Opponent,
            RunnerOpponentScore
          )
        )
      )
    )
  ).

number_of_votes_ranking_former_above_latter(X, Y, NumberOfVotes, DB):-
  aggregate_all(
    count,
    (
      call(DB, _Voter, Ranking),
      before(X, Y, Ranking)
    ),
    NumberOfVotes
  ).

winning_votes_score(X, Y, WinningVotesScore, DB):-
  number_of_votes_ranking_former_above_latter(
    X,
    Y,
    NumberOfVotesRankingXOverY,
    DB
  ),
  number_of_votes_ranking_former_above_latter(
    Y,
    X,
    NumberOfVotesRankingYOverX,
    DB
  ),
  (
    NumberOfVotesRankingXOverY > NumberOfVotesRankingYOverX
  ->
    WinningVotesScore = NumberOfVotesRankingXOverY
  ;
    WinningVotesScore = 0
  ).

pairwise_opposition_score(X, Y, PairwiseOppositionScore, DB):-
  number_of_votes_ranking_former_above_latter(
    X,
    Y,
    PairwiseOppositionScore,
    DB
  ).

margins_score(X, Y, MarginsScore, DB):-
  number_of_votes_ranking_former_above_latter(
    X,
    Y,
    NumberOfVotesRankingXOverY,
    DB
  ),
  number_of_votes_ranking_former_above_latter(
    Y,
    X,
    NumberOfVotesRankingYOverX,
    DB
  ),
  MarginsScore is NumberOfVotesRankingXOverY - NumberOfVotesRankingYOverX.

%! write_condorcet_matrix is det.
% Writes a simple representation of the Condorset matrix to
% the current output stream.

write_condorcet_matrix:-
  format('...'),
  forall(
    contestant(Runner),
    format('\t%w', [Runner])
  ),
  format('\n'),
  forall(
    contestant(Runner),
    (
      format('%w', [Runner]),
      forall(
        contestant(Opponent),
        write_condorcet_cell(Runner, Opponent)
      ),
      format('\n')
    )
  ),
  format('\n').

%! write_condorcet_cell(+Runner, +Opponent) is det.
% Writes a cell of the Condorcet matrix to the current output stream.

% No value is given for the runner against himself.
write_condorcet_cell(Runner, Runner):-
  format('\t-').
write_condorcet_cell(Runner, Opponent):-
  runner_opponent_result(Runner, Opponent, RunnerOpponentScore),
  format('\t%w', [RunnerOpponentScore]).

%! runner_results is det.
% Calculates and asserts the results per runner, based on the asserted
% results for runner/opponent pairs.

runner_results:-
  forall(
    contestant(Runner),
    (
      findall(
        RunnerOpponentScore,
        runner_opponent_result(
          Runner,
          _Opponent,
          RunnerOpponentScore
        ),
        RunnerOpponentScores
      ),
      sum_list(RunnerOpponentScores, RunnerScore),
      assert(runner_result(Runner, RunnerScore))
    )
  ).

%! write_runner_scores is det.
% Writes the asserted runner scores to the current output stream.

write_runner_scores:-
  forall(
    runner_result(Runner, RunnerScore),
    format('%w: %w\n', [Runner, RunnerScore])
  ).

%! runner_score_runner_pairs(-SortedRunnerScoreRunnerPairs) is det.
% Returns the pairs of runner scores and runners.

runner_score_runner_pairs(SortedRunnerScoreRunnerPairs):-
  findall(
    RunnerScore-Runner,
    runner_result(Runner, RunnerScore),
    RunnerScoreRunnerPairs
  ),
  keysort(RunnerScoreRunnerPairs, SortedRunnerScoreRunnerPairs).

%! copeland_winner(-CopelandWinner) is semidet.
% Returns the Copeland winner, if there is one. Fails otherwise.

copeland_winner(CopelandWinner):-
  runner_score_runner_pairs(RunnerScoreRunnerPairs),
  last(RunnerScoreRunnerPairs, WinnerScore-Winner),
  (
    member(WinnerScore-OtherWinner, RunnerScoreRunnerPairs),
    \+(Winner == OtherWinner)
  ->
    fail
  ;
    CopelandWinner = Winner
  ).

%! copeland_loser(-CopelandLoser) is det.
% Returns the Copeland lower, if there is one. Fails otherwise.

copeland_loser(CopelandLoser):-
  runner_score_runner_pairs(RunnerScoreRunnerPairs),
  first(RunnerScoreRunnerPairs, LoserScore-Loser),
  (
    member(LoserScore-OtherLoser, RunnerScoreRunnerPairs),
    \+(Loser == OtherLoser)
  ->
    fail
  ;
    CopelandLoser = Loser
  ).

loser(Loser):-
  runner_score_runner_pairs(RunnerScoreRunnerPairs),
  first(RunnerScoreRunnerPairs, _LoserScore-Loser).

