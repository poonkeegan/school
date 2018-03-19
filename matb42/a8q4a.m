uv = linspace(0,1);
vv = linspace(1,2);
thetav = linspace(0,2*pi);
[u,theta1] = meshgrid(uv,thetav);
x = cos(theta1);
y = sin(theta1);
z = u;

[v,theta2] = meshgrid(vv,thetav);
x1 = v.*cos(theta2);
y2 = v.*sin(theta2);
z3 = v;

fig = figure('visible','off');
  hold on;
  mesh(x,y,z);
  mesh(x1,y2,z3);
  axis equal;
  box on;
  xlabel('x-axis')
  ylabel('y-axis')
  zlabel('z-axis')
  xlim([-3,3]);
  ylim([-3,3]);
  zlim([0,3]);
  view([1,1,0.5]);
  
print(fig, 'b42-a8-4a','-dpng');