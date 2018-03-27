xv = linspace(0,1);
yv = linspace(0,1);
[x,y] = meshgrid(xv,yv);
z = 1 - x - y;

fig = figure('visible','off');
  hold on;
  mesh(x,y,z);
  axis equal;
  box on;
  xlabel('x-axis')
  ylabel('y-axis')
  zlabel('z-axis')
  xlim([0,2]);
  ylim([0,2]);
  zlim([0,2]);
  view([1,1,0.5]);
  
print(fig, 'b42-a10-5b','-dpng');

