# Set bounds
uv = linspace(0,1);
vv = linspace(0,1);
[u,v] = meshgrid(uv,vv);

# Set equations
z = u .* v;
x = 1-u;
y = 1-v;

fig = figure;
  hold on;
  # Draw x,y,z
  mesh(x,y,z);
  axis equal;
  box on;
  xlabel('x-axis')
  ylabel('y-axis')
  zlabel('z-axis')
  # Boundaries of the plot
  xlim([0,2]);
  ylim([0,2]);
  zlim([0,2]);
  # The camera angle looks
  # from this direction to the origin 
  view([1,1,0.5]);

  
