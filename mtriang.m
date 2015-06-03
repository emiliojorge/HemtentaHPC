function mtriang 
n = 400;
A = triu(randn(n));
B = triu(randn(n));

tic;
  F = A * B;
toc

tic
C=zeros(n);
  for j=1:n
	for i=1:n
%	C(i,j)=A(i,:)*B(:,j);
	end
  end
toc

tic
  C=zeros(n);
   %utilizing columnfirst.
  for j=1:n %columnfirst
    for i=1:j
    C(i,j)=dot(A(i,i:j),B(i:j,j));
    end
  end
toc

tic
D=zeros(n);

A=A';
      for J=1:n
	  for I=1:J
             for K=I:J
                D(I,J)=D(I,J)+A(K,I)*B(K,J);	
             end
          end
       end
toc


error = norm(F - C, 1)
error = norm(F - D, 1)
if error > 1e-10
  warning(['The error is large. error = ', num2str(error)])
end

