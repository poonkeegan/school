uv = linspace(1,10);
vv = linspace(1,10);
[u,v] = meshgrid(uv,vv);
x = u;
y = v;
z = (u.^2)./(v.^2);


hold on;
mesh(x,y,z);
#mesh(xtang,ytang,ztang);
axis equal
box on
xlabel('x-axis')
ylabel('y-axis')
zlabel('z-axis')