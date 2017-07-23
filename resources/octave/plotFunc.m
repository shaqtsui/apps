x =linspace(-1000, 1000, 100)
y =linspace(-1000, 1000, 100)
[xx, yy]=meshgrid(x, y)
zz = 2*(xx+3*yy).^2 - 6*yy.^2
meshc(xx, yy, zz)