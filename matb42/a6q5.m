
% a)
%{
xv = linspace(-1,3);
thetav = linspace(0,2*pi);
[x,theta] = meshgrid(xv,thetav);
y = cos(theta);
z = sin(theta);
%}
% b)
%{
rv = linspace(0,1);
thetav = linspace(0,2*pi);
[r,theta] = meshgrid(rv,thetav);
x = r.*cos(theta);
y = r.*sin(theta);
z = r.*(cos(theta) + sin(theta)) + 5;
%}
% c)
%{
uv = linspace(0,2*pi);
vv = linspace(0,(pi/3));
[u,v] = meshgrid(uv,vv);
x = 2*cos(u).*sin(v);
y = 2*sin(u).*sin(v);
z = 2*cos(v);
%}
% d)

uv = linspace(0,1);
vv = linspace(0,1);
[u,v] = meshgrid(uv,vv);
x = (v-u)/3;
y = (2*u + v)/3;
z = (3 - 2*v - u)/3;

mesh(x,y,z);
axis equal
box on
xlabel('x-axis')
ylabel('y-axis')
zlabel('z-axis')