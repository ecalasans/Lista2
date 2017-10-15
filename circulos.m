for k=1:0.5:10
    grid
    viscircles([-1 0], k*0.5, 'Color','r')
    viscircles([1 0], k*0.5, 'Color','b')
end