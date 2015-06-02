function mtriang
n = 4000;
A = triu(randn(n));
B = triu(randn(n));

tic;
  F = A * B;
toc

tic
  % Your code here...
  C = ...
toc

error = norm(F - C, 1)
if error > 1e-10
  warning(['The error is large. error = ', num2str(error)])
end

