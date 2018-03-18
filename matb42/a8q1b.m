u1v = linspace(-1/3,0);
u2v = linspace(0,1/3);
thetav = linspace(0,2*pi);
phiv = linspace(pi/2,3*pi/2);
[theta,phi] = meshgrid(thetav,phiv);
[theta1,u1] = meshgrid(thetav,u1v);
[theta2,u2] = meshgrid(thetav,u2v);
x1 = (3-3*u2).*cos(theta1);
y1 = (3-3*u2).*sin(theta1);
z1 = -3*u2;


x2 = (3-3*u2).*cos(theta1);
y2 = (3-3*u2).*sin(theta1);
z2 = 3*u2;

x3 = (2 + cos(phi)).*cos(theta);
y3 = (2 + cos(phi)).*sin(theta);
z3 = sin(phi);

fig = figure('visible','off');
  hold on;
  mesh(x1,y1,z1);
  mesh(x2,y2,z2);
  mesh(x3,y3,z3);
  axis equal;
  box on;
  xlabel('x-axis')
  ylabel('y-axis')
  zlabel('z-axis')
  xlim([-5,5]);
  ylim([-5,5]);
  zlim([-2,2]);
  view([1,1,0.5]);

print(fig, 'b42-a8-1b','-dpng');
  