uv = linspace(-4,4);
vv = linspace(-4,4);
[u,v] = meshgrid(uv,vv);
x = (u.^2 + v);
y = v;
z = (u+v.^2);

size = 7;
xtangv = linspace(-size+2,size+2);
ytangv = linspace(-size,size);
[xtang, ytang] = meshgrid(xtangv,ytangv);
ztang = (xtang-ytang+9)/6;

hold on;
mesh(x,y,z);
mesh(xtang,ytang,ztang);
axis equal
box on
xlabel('x-axis')
ylabel('y-axis')
zlabel('z-axis')