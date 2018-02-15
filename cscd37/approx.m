function [c, flag] = approx(f,a,b,n,m)
  j = 0:m;
  t = (a + ((b-a)*j/m));
  g = feval(f, t(1:m+1));
  V = zeros(m+1,n+1);
  c = zeros(1,n+1);
  for i = 0:n
    V(:,(i+1)) = (t').^i;
  end
  [Q,R] = qr(V);
  if rank(R) < n + 1
    flag = 1;
  else
    R1 = R(1:n+1,:);
    gtilde = (Q')*(g');
    opts.UT = true;
    c = linsolve(R1,gtilde(1:n+1),opts);
    flag = 0;
  end
  return
 end
 

 