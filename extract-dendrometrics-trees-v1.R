library(TreeLS)

# importar a nuvem da parcela, fatiada para 0.1 a 2.5 metros.
rioPreto = readTLS("thin_cut.las")

# localizar as árvores
x = plot(rioPreto)
map = treeMap(rioPreto, map.hough(min_h = 0.1, max_h = 1, h_step = 0.5,
                                  max_d = 0.35, pixel_size = 0.05,
                                  min_density = 0.3, min_votes = 2), 0)

add_treeMap(x, map, color='white')

# calcular as regiões das árvores identificadas
rioPreto = treePoints(rioPreto, map, trp.crop())
add_treePoints(x, rioPreto, size=4)
#add_treeIDs(x, rioPreto, cex=2, col='yellow')

# classicar o fuste
rioPreto = stemPoints(rioPreto, stm.hough())
add_stemPoints(x, rioPreto, color='red', size=8)

# reconstruir o diâmetro
inv = tlsInventory(rioPreto, d_method = shapeFit(shape='circle', algorithm = 'ransac'))
add_tlsInventory(x, inv)

# segmentar o fuste da árvore
seg = stemSegmentation(rioPreto, sgt.ransac.circle(n=20))
add_stemSegments(x, seg, color='white',fast=T)