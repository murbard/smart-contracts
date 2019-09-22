# Exponential with Michelson

## Approximation

We start by creating an approximation which can be easily and
efficiently computed using fractions.

$$\begin{eqnarray}
\exp{x} & = & 2^{x / \log 2 }\\
& = & 2^{[ x / \log{2}]} 2^{(x / \log{2}) - [x / \log{2}]}\\
& = & 2^{[ x / \log{2} ]} \exp{\left(x - (\log{2}) [x / (\log{2})]\right)}
& = & 2^{[ x / \log{2} ]} \exp{\left(x - (\log{2}) [x / (\log{2}) ]\right)}
\end{eqnarray}$$

Let $i = [ x / \log{2}]$ be the nearest integer and $f = x - (\log {2}) i$ the fractional part multiplied by $\log{2}$.

$$\exp{x} = 2^i \frac{1 + f/2 + f^2/12}{1 - f/2 + f^2/12}r(f)$$

where $|r(f) - 1| < \frac{1}{142,975}$

## Integer and fractional part

We start with $x = p/q$. Take $\log{2} \simeq \frac{1143}{1649}$, then
$x / \log{2} \simeq (1649 \times p)/(1143 \times q)$.

We perform the euclidian division:

$$(a, b) = \textrm{ediv_rem}(1649 \times p, 1143 \times q)$$

which satisfies $1649 \times p = 1143 \times q \times a + b$ where $0 \leq b \lt 1143 \times q$.

We're interested in the closest integer value to the fraction, therefore if $2 \times b < \leq 1143 \times q$, we write

$$(i, f) = (a,  b / (1649 \times q))$$

otherwise if $2 \times b \gt 1143 \times q$, we write

$$(i, f) = (a + 1,  (b - 1143 \times q)/ (1649 \times q))$$

## Exponential

We compute $2^i$ by repeated squaring.

## Padde approximant

We now need to compute $$e(f) = \frac{1 + f/2 + f^2/12}{1 - f/2 + f^2/12}$$

Write $f = u/v$.

$$e(f) = \frac{u^2 + 6  u  v + 12 v^2}{u^2 - 6  u  v + 12  v^2}$$


