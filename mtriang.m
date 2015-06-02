function mtriang
n = 400;
A = triu(randn(n));
B = triu(randn(n));

tic;
  F = A * B;
toc

tic
  C=zeros(n);
   %utilizing columnfirst.
  for j=1:n %columnfirst
    for i=1:j
    C(i,j)=A(i,i:j)*B(i:j,j);
    end
  end
toc
C;
tic
  D=zeros(4000);
  A=A';
  for j=1:4000
    for i=1:4000
    end
end   
toc



tic
       DOALL J=1,N
          DO I=1,J
             DO K=I,J
                A(I,J)=A(I,J)+B(I,K)*C(K,J)
             ENDDO
          ENDDO
       ENDDO
toc
error = norm(F - C, 1)
if error > 1e-10
  warning(['The error is large. error = ', num2str(error)])
end

