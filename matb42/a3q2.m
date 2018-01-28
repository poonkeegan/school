t = 0:0.1:10;
x = y = linspace(-50, 50, 300);
[xx, yy] = meshgrid(x, y);
z1 = sqrt(xx.^2+yy.^2);
z2 = sqrt(-xx.^2-yy.^2.+2*xx);
figure;
hold on;
plot3((1/2).*sin(t).+(1/2), (1/2).*cos(t), (1/2).*(sin(t).+1))
mesh(x, y, z1);
ellipsoid(1, 0, 0, 1, 1, 1)
xlim([-5,5]);
ylim([-5,5]);
zlim([0,2]);
#plot3(3.*(t.^2), -sin(t), -exp(t), tpnt(1).+dvec(1).*t, tpnt(2).+dvec(2).*t, tpnt(3).+dvec(3).*t);
grid on