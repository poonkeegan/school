rv = linspace(0,2);
thetav = linspace(0,2*pi);
[r,theta] = meshgrid(rv,thetav);
x = r.*cos(theta);
y = r.*sin(theta);
z = r.^2 - 4;

fig = figure('visible','off');
  hold on;
  mesh(x,y,z);
  % Draw vectors
  normvec = quiver3(0,0,-4,0,0,-1);
  tangent = quiver3(0,2,0,2,0,0);
  tnormvc = quiver3(0,2,0,0,0,-2);
  set(normvec, 'LineWidth', 1);
  set(tangent, 'LineWidth', 1);
  set(tnormvc, 'LineWidth', 1);
  set(normvec, 'Color', 'blue');
  set(tangent, 'Color', 'red');
  set(tnormvc, 'Color', 'black');
  textnorm = text(1, 0, -4, 'Normal');
  texttang = text(2, 2, 1, 'Tangent');
  texttnrm = text(1, 4, 0, 'Normal on Curve');
  set(textnorm, 'Color', 'blue');
  set(texttang, 'Color', 'red');
  set(texttnrm, 'Color', 'black');
  axis equal;
  box on;
  xlabel('x-axis')
  ylabel('y-axis')
  zlabel('z-axis')
  xlim([-4,4]);
  ylim([-4,4]);
  zlim([-5,2]);
  view([1,1,0.5]);
print(fig, 'b42-a10-6','-dpng');
