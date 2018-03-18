uv = linspace(0,1);
vv = linspace(0,2*pi);
[u,v] = meshgrid(uv,vv);
x = (1-u).*(cos(v)) + 2*u;
y = (1-u).*(sin(v))+ 3*u;
z = 1 + 2*u;


hold on;
mesh(x,y,z);
#mesh(xtang,ytang,ztang);
axis equal
box on
xlabel('x-axis')
ylabel('y-axis')
zlabel('z-axis')
xlim([-2,3]);
ylim([-2,4]);
zlim([-2,5]);