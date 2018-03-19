uv = linspace(0,1);
vv = linspace(0,2*pi);
[u,v] = meshgrid(uv,vv);
x = (1-u).*(cos(v)) + 2*u;
y = (1-u).*(sin(v))+ 3*u;
z = 3*u;

fig = figure('visible','off');
  hold on;
  mesh(x,y,z);
  axis equal
  box on
  xlabel('x-axis')
  ylabel('y-axis')
  zlabel('z-axis')
  xlim([-1,4]);
  ylim([-1,4]);
  zlim([0,4]);
  view([1,-1,0.5]);
  
print(fig, 'b42-a8-2b','-dpng');