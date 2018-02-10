% tests
f = {@cos, @exp, @log, @atan};
a = [0, -1, 1, -5];
b = [pi, 1, 3, 5];
x1 = [-pi,-2,0.5,-10];
x2 = [2*pi,2,5,10];
y1 = [-10, -1, -1, -5];
y2 = [10, 8, 2, 5];
m = [20, 15, 10, 30];
n = [4, 3, 2, 9];
i = [horzcat(0:49,51:100);-49:50;51:150;horzcat(-50:-1,1:50)];
samplepts = [i(1,:).*(0.01).*pi; i(2,:).*0.02; i(3,:).*0.02; i(4,:).*0.1];
% Stored tables
Tables = cell(4,1);
for i = 1:4
  % Plot tests
  x = linspace(x1(i),x2(i));
  [c, flag] = approx(f{i},a(i),b(i),n(i),m(i));
  subplot(2,2,i)
  hold on
  plot_title = sprintf("f:%s, a:%.3f, b:%.3f, n:%d, m:%d", func2str(f{i}), a(i), b(i), n(i), m(i));
  title(plot_title);
  plot(x, polyval(flipud(c),x));
  plot(x , f{i}(x));
  hold off
  xlim([x1(i),x2(i)]);
  ylim([y1(i),y2(i)]);
  % Write tables
  xi = samplepts(i,:)';
  pxi = polyval(flipud(c), xi);
  fxi = feval(f{i}, xi);
  RelativeError = ((pxi-fxi)./fxi);
  Tables{i} = table(xi, pxi, fxi, RelativeError);
  tableTitle = convertStringsToChars(sprintf("%s_xi", func2str(f{i})));
  Tables{i}.Properties.VariableNames = {'xi', 'pxi', tableTitle, 'RelativeError'};
  % Table 1 is cos
  %       2 is exp
  %       3 is log
  %       4 is arctan
end
 