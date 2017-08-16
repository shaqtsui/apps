x =linspace(-100, 100, 16);
y =linspace(-100, 100, 16);
[xx, yy]=meshgrid(x, y);
bowl = xx.^2 + 2 * yy.^2;

mesh(xx, yy, bowl);
#surfl(xx, yy, bowl);
#waterfall(xx, yy, bowl);
#scatter3(xx, yy, bowl);

hold on;


# gradient is 2 * xx, 4 * yy


x =linspace(0, 100, 2);
y =linspace(0, 0, 2);
line=x.^2 + 2 * y.^2;
plot3(x, y, line);


x =linspace(-100, 100, 2);
y =linspace(-100, 100, 2);
[xx, yy]=meshgrid(x, y);
plane=100*xx;
#surf(xx, yy, plane);


#ribbon([1:10
#       11:20]);

