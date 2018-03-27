thetav = linspace(0,2*pi);
rv = linspace(1,3);
[theta,r] = meshgrid(thetav,rv);
x = r.*cos(theta);
y = r.*sin(theta);
z = r;

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
  view([1,1,1.5]);
  
print(fig, 'b42-a10-5c','-dpng');


