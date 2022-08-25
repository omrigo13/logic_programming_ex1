/**** I, Omri Goldberg (208938985) assert that the work I submitted is entirely my own.
I have not received any part from any other student in the class (or other source),
nor did I give parts of it for use to others. I have clearly marked in the comments
of my program any code taken from an external source. *****/

% Signature: nat(X)/2
% Purpose: N is a natural number in s(0) notation.
nat(0).
nat(s(X)) :- nat(X).


% Signature: add(X, Y, Z)/3
% Purpose: succeeds if and only
% if X and Y and Z are natural numbers in s(0) notation such that X+Y=Z.
add(0, X, X) :- nat(X).
add(s(X), Y, s(Z)) :- add(X,Y,Z).


% Signature: leq(X, Y)/2
% Purpose: succeeds if and only
% if X and Y are natural numbers in s(0) notation such that X<=Y.
leq(0,_).
leq(s(X),s(Y)) :- leq(X,Y).


% Signature: lt(X, Y)/2
% Purpose: succeeds if and only
% if X and Y are natural numbers in s(0) notation such that X<Y.
lt(0, s(_)).
lt(s(X), s(Y)) :- lt(X, Y).


% Signature: times(X, Y, Z)/3
% Purpose: succeeds if and only
% if X and Y and Z are natural numbers in s(0) notation such that X*Y=Z.
times(0, X, 0) :- nat(X).
times(s(X), Y, Z) :- lt(X, Z), leq(Y, Z), times(X, Y, W), add(W, Y, Z).
  

% Task 1: Unary Square Root
% Signature: unary_sqrt(N, K)/2
% Purpose: K is the square-root of N,
% where N and K, are unary numbers.
unary_sqrt(N, K) :- unary_sqrt_helper(N, K), times(s(K), s(K), M), lt(N, M).


% Signature: unary_sqrt_helper(N, K)/2
% Purpose: K is the square-root of N,
% where N and K, are unary numbers.
% helper to get only the biggest square root of n that k ∗ k <= n
% and (k+1) ∗ (k+1) > n
unary_sqrt_helper(0, 0).
unary_sqrt_helper(N, K) :- times(K, K, N), lt(0, N).
unary_sqrt_helper(s(N), K) :- unary_sqrt_helper(N, K), lt(0, N).


% Task 2: Unary Divisor
% Signature: unary_divisor(N, K)/2
% Purpose: integers N and K, K is a divisor of N,
% if there exists R such that N = K * R.
unary_divisor(0, s(X)) :- nat(X).
unary_divisor(N, K) :- lt(0, N), times(K, R, N), nat(R).


% Task 3: Binary Addition
% Signature: binary_plus(X, Y, Z)/3
% Purpose: succeeds if and only
% if X and Y and Z are natural numbers in binary notation such that X+Y=Z.
binary_plus([],[],[]).
binary_plus([], [0|Ys], Z) :- append([], [0|Ys], Z).
binary_plus([], [1|Ys], Z) :- append([], [1|Ys], Z).
binary_plus([0|Xs], [], Z) :- append([0|Xs], [], Z).
binary_plus([1|Xs], [], Z) :- append([1|Xs], [], Z).
binary_plus([0|Xs],[0|Ys], Z) :- binary_plus(Xs,Ys,Z1), append([0],Z1, Z).
binary_plus([0|Xs],[1|Ys], Z) :- binary_plus(Xs,Ys,Z1), append([1],Z1, Z).
binary_plus([1|Xs],[0|Ys], Z) :- binary_plus(Xs,Ys,Z1), append([1],Z1, Z).
binary_plus([1|Xs],[1|Ys], Z) :- binary_plus(Xs,Ys,Z1), binary_plus([1],Z1, Z2), append([0],Z2, Z). 


% Task 4: Binary multiplication
% Signature: binary_times(X, Y, Z)/3
% Purpose: succeeds if and only
% if X and Y and Z are natural numbers in binary notation such that X*Y=Z.	
binary_times([], [], []).
binary_times([], [_|_], []).
binary_times([_|_], [], []).
binary_times(X, [0|Ys], Z) :- length(X, X_SIZE), X_SIZE > 0, binary_times(X, Ys, Z1), append([0], Z1, Z).
binary_times(X, [1|Ys], Z) :- length(X, X_SIZE), X_SIZE > 0, binary_times(X, Ys, Z1), binary_plus([0|Z1], X, Z).


% Task 5: Primality Testing
% Signature: is_prime(N)/1
% Purpose: succeeds if and only
% if N is a prime number.																
is_prime(N) :-
	N > 2, 1 is mod(N, 2), M is sqrt(N), N_LIMIT is floor(M),
	my_for_all(N, N_LIMIT).
is_prime(2).


% Signature: my_for_all(N, N_LIMIT)/2
% Purpose: run over all the possible divisors
% if there is a divisor return false, otherwise true.																
my_for_all(N, N_LIMIT) :- N_LIMIT > 2, mod(N, N_LIMIT) > 0, NEXT_LIMIT is N_LIMIT - 1, my_for_all(N, NEXT_LIMIT).
my_for_all(3, _).
my_for_all(_, 2).
	
	
% Task 6: Test Right Truncatable Primes
% Signature: right_prime(N)/1
% Purpose: A prime number N is right truncatable if
% every prefix of N is also a prime number.	
right_prime(0).
right_prime(N) :- is_prime(N), N1 is N//10, right_prime(N1).
  

% Task 7: Generate Right Truncatable Primes
% Signature: right_prime_gen(X)/1
% Purpose: generates right truncatable primes upon calling.
right_prime_gen(X) :- counter(2, X), right_prime(X).


% Signature: counter(N, V)/2
% Purpose: V is increased evrey round from the number N by one. 
counter(N, N).
counter(N, V) :- NewN is N + 1, counter(NewN, V).


% Task 8: Generate Binary Tree from Traversals
% Signature: gentree(Preorder, Inorder, Tree)/3
% Purpose: succeeds if and only if the binary tree Tree 
% has preorder and inorder traversals that correspond
% to Preorder and Inorder.
gentree(Preorder, Inorder, Tree) :- preorder(Tree, Preorder), inorder(Tree, Inorder).


% Signature: preorder(Tree, List)/2
% Purpose: preorder traversal of a list and build a tree from it.
preorder(nil, []).
preorder(tree(L, S, R), Preorder) :- 
    append([S|Ls], Rs, Preorder), preorder(L, Ls), preorder(R, Rs).


% Signature: inorder(Tree, List)/2
% Purpose: inorder traversal of a list and build a tree from it.
inorder(nil, []).
inorder(tree(L, S, R), Inorder) :- 
    append(Ls, [S|Rs], Inorder), inorder(L, Ls), inorder(R, Rs).
	

% Task 9: Evaluate Expression (without parentheses)
% Signature: evaluate(Expression, Value)/2
% Purpose: evaluate arithmetic expression on Expression given as a list with numbers
% and operators *, + evaluates to Value.		
evaluate(VALUE, VALUE) :- number(VALUE).
evaluate([OP1, *, OP2|EXP], VALUE) :- evaluate(OP1, VAL_OP1), evaluate(OP2, VAL_OP2), TOTAL_VAL is VAL_OP1 * VAL_OP2, evaluate([TOTAL_VAL|EXP], VALUE).
evaluate([OP1, *, OP2], VALUE) :- evaluate(OP1, VAL_OP1), evaluate(OP2, VAL_OP2), VALUE is VAL_OP1 * VAL_OP2.
evaluate([OP1, +, OP2], VALUE) :- evaluate(OP1, VAL_OP1), evaluate(OP2, VAL_OP2), VALUE is VAL_OP1 + VAL_OP2.
evaluate([OP1, +|EXP], VALUE) :- evaluate(EXP, VAL_EXP), VALUE is OP1 + VAL_EXP.