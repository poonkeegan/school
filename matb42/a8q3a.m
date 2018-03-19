uv = linspace(-10,10);
vv = linspace(-10,10);
[u,v] = meshgrid(uv,vv);
x = u;
y = v;
z = (u.^2)./(v.^2);

fig = figure('visible','off');
  hold on;
  mesh(x,y,z);
  axis equal
  box on
  xlabel('x-axis')
  ylabel('y-axis')
  zlabel('z-axis')
  xlim([-10,10]);
  ylim([-10,10]);
  zlim([0,20]);
  view([1,1,0.5]);

print(fig, 'b42-a8-3a','-dpng');