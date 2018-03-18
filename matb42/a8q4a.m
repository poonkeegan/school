uv = linspace(0,1);
vv = linspace(1,2);
thetav = linspace(0,2*pi);
[u,theta1] = meshgrid(uv,thetav);
x = cos(theta1);
y = sin(theta1);
z = u;

[v,theta2] = meshgrid(vv,thetav);
x1 = v.*cos(theta2);
y2 = v.*sin(theta2);
z3 = v;

hold on;
mesh(x,y,z);
mesh(x1,y2,z3);
#mesh(xtang,ytang,ztang);
axis equal;
box on;
xlabel('x-axis')
ylabel('y-axis')
zlabel('z-axis')
xlim([-5,5]);
ylim([-5,5]);
zlim([0,3]);