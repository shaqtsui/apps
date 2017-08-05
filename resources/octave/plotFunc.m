x =linspace(-100, 100, 16);
y =linspace(-100, 100, 16);
[xx, yy]=meshgrid(x, y);
bowl = xx.^2 + 2 * yy.^2;

mesh(xx, yy, bowl);
#surfl(xx, yy, bowl);
#waterfall(xx, yy, bowl);
#scatter3(xx, yy, bowl);

hold on;

x =linspace(0, 100, 10);
y =linspace(0, 0, 10);
line=100*x;
#plot3(x, y, line);


x =linspace(-100, 100, 2);
y =linspace(-100, 100, 2);
[xx, yy]=meshgrid(x, y);
plane=100*xx;
#surf(xx, yy, plane);


#ribbon([1:10
#       11:20]);

