uv = linspace(0,1);
vv = linspace(0,1);
[uv,vv] = meshgrid(u,v);
x = u + v;
y = u.*v.*v;
z = u.*u + v.*v;

fig = figure('visible','off');
  hold on;
  mesh(x,y,z);
  axis equal;
  box on;
  xlabel('x-axis')
  ylabel('y-axis')
  zlabel('z-axis')
  xlim([-2,2]);
  ylim([-2,2]);
  zlim([-2,2]);
  view([1,1,0.5]);
  
print(fig, 'b42-a10-5c','-dpng');

