thetav = linspace(0,2*pi);
phiv = linspace(0,pi/2);
[theta,phi] = meshgrid(thetav,phiv);
x = 2*sin(phi).*cos(theta);
y = 2*sin(phi).*sin(theta);
z = 2*cos(phi);

fig = figure('visible','off');
  hold on;
  mesh(x,y,z);
  axis equal;
  box on;
  xlabel('x-axis')
  ylabel('y-axis')
  zlabel('z-axis')
  xlim([-3,3]);
  ylim([-3,3]);
  zlim([0,3]);
  view([1,1,0.5]);
  
% print(fig, 'b42-a8-4a','-dpng');
