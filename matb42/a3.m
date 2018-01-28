gamma = @(t) [3*t*t, -sin(t), -exp(t)];
dgamma = @(t) [6*t, -cos(t), -exp(t)];
tgamma = @(t, x) gamma(t) + dgamma(x);
tangentline = @(x) tgamma(1/2);
tpnt = gamma(1/2);
dvec = dgamma(1/2);

t = 0:0.1:10;
plot3(3.*(t.^2), -sin(t), -exp(t),
      tpnt(1).+dvec(1).*t, tpnt(2).+dvec(2).*t, tpnt(3).+dvec(3).*t);
zlim([-100,0]);
grid on