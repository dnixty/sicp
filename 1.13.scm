;; Prove that Fib(n) is the closest integer to ϕⁿ/√5,
;; where ϕ=(1+√5)/2. Hint:
;; Let: ψ=(1-√5)/2. Use induction and the definition
;; of the Fibonacci numbers (see section 1.2.2) to prove that
;; Fib(n)=(ϕⁿ-ψⁿ)/√5.
;;
;; From the definition of Fibonacci numbers we have:
;; Fib(n+2) = Fib(n+1) + Fib(n)
;; Fib(n) = Fib(n+2) - Fib(n+1)
;;
;; Assume Fib(n)=(ϕⁿ-ψⁿ)/√5 holds, thus:
;; (ϕ^n-ψ^n)/√5 = (ϕ^(n+2)-ψ^(n+2))/√5 - (ϕ^(n+1)-ψ^(n+1))√5 | * √5
;; ϕ^n-ψ^n = ϕ^(n+2)-ψ^(n+2) - ϕ^(n+1)-ψ^(n+1)
;; ϕ^n-ψ^n = ϕ^n*(ϕ^2-ϕ) - ψ^n*(ψ^2-ψ)
;;
;; We need to prove that ϕ^2-ϕ=1 and ψ^2-ψ=1. Knowing that
;; ϕ=(1+√5)/2 and ψ=(1-√5)/2 we have:
;; ((1+√5)/2)^2 - (1+√5)/2 = 1
;; (1 + 2√5 + 5)/4 - (2+2√5)/4 = 1
;; (6+2√5-2-2√5)/4 = 1
;; 4/4 = 1
;; 1 = 1
;; and
;; ((1-√5)/2)^2-(1-√5)/2 = 1
;; (1-2√5+5)/4 - (2-2√5)/4 = 1
;; (6-2√5-2+2√5)/4 = 1
;; 4/4 = 1
;; 1 = 1
;;
;; Thus proving that Fib(n)=ϕⁿ/√5-ψⁿ/√5 is true. Or:
;; Fib(n)+ψⁿ/√5 = ϕⁿ/√5
;;
;; Since 0 < |ψ| < 1 then 0 < |ψ^n/√5| < 1/√5 < 1.
;; We can add Fib(n) to each side of: 0 < ψ^n/√5 < 1.
;; Fib(n) < Fib(n) + |ψ^n/√5| < Fib(n) + 1
;; Which means:
;; Fib(n) < |ϕⁿ/√5| < Fib(n) + 1
;; This shows that ϕⁿ/√5 will never be further away from the value
;; of Fib(n) than 1.
