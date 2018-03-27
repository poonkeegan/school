uv = linspace(0,1);
vv = linspace(0,1);
[u,v] = meshgrid(uv,vv);
x = u + v;
y = u.*(v.^2);
z = (u.^2) + (v.^2);

fig = figure('visible','off');
  hold on;
  mesh(x,y,z);
  axis equal;
  box on;
  xlabel('x-axis')
  ylabel('y-axis')
  zlabel('z-axis')
  xlim([-4,4]);
  ylim([-4,4]);
  zlim([-4,4]);
  view([1,1,0.5]);
  
%print(fig, 'b42-a10-5d','-dpng');

