############# SCRIPT CRIADO POR MARCELO NUNES VILAS BOAS ##############
#2023/24
#PARA REALZAÇÃO DE TRABALHO DE CONCLUSÃO DE CURSO


###### INSERÇÃO DOS DADOS #######
require(dplyr)
require(lidR)
library(TreeLS)
require(progress)
library(ggplot2)
library(tidyr)

####### Caminho do arquivo para o seu arquivo LAS #######
file <- "C:/Users/Luiana/Desktop/ic_eric/testes_4/Nuvens/riopreto_cut.las"

# Carregue o arquivo LAS
tls <- readLAS(file)
print(tls)
#thin <- tlsSample(tls, smp.voxelize(0.05))
thin <- readLAS("C:/Users/Luiana/Desktop/ic_eric/testes_4/Nuvens/thin.las")
print(thin)

analise<-read.csv("C:/Users/Luiana/Desktop/ic_eric/testes_4/resultados_TreeTools/resultados_map.hough/results/analise.csv")

#writeLAS(thin,"~/UFVJM/TCC/dados_campo_rio_preto_2022/mls/nuvens/thin.las",index = TRUE)
thin_cut<-filter_poi(thin, Z >= 0.1 & Z <= 2.5)

writeLAS(thin_cut,"C:/Users/Luiana/Desktop/ic_eric/testes_4/Nuvens/thin_cut.las",index = TRUE)

################################## TESTE DOS PARÂMETROS DE BUSCA POR ÁVORES FUSTES #########################
######## MAP.HOUGH #####
# Perform garbage collection
thin_cut<-readLAS("C:/Users/Luiana/Desktop/ic_eric/testes_4/Nuvens/thin_cut.las")

gc()

# Registre o tempo inicial
start_time <- Sys.time()

##deixar só os ótimos em um dos scripts#
# Defina os valores dos parâmetros de pesquisa de grade
min_h_values <- c(0.3)
max_h_values <- c(1)  # Defina um valor inicial maior que min_h
h_step_values <- c(0.5)
max_d_values <- 0.35
pixel_size_values <- c(0.16) # O maior objeto a ser identificado tem 0.32
min_density_values <- c(0.3)
min_votes_values <- c(3)

 
# Calcule o total de combinações
total_combinations <- length(min_h_values) * length(max_h_values) * length(h_step_values) *
  length(max_d_values) * length(pixel_size_values) * length(min_density_values) *
  length(min_votes_values)
summary(total_combinations)

# Inicialize a barra de progresso
pb <- progress_bar$new(
  format = "  progress [:bar] :percent :elapsedfull :eta",
  total = total_combinations
)

# Especificar o diretório de exportação para resultados e erros
export_dir_results <- file.path("C:/Users/Luiana/Desktop/ic_eric/testes_4/resultados_TreeTools/resultados_map.hough/results")


# Criar diretórios se não existirem
dir.create(export_dir_results, recursive = TRUE)


# Inicializar listas para armazenar resultados e erros
results_list <- list()

# Loop da pesquisa de grade
for (min_h in min_h_values) {
  for (max_h in max_h_values) {
    # Garantir que min_h seja sempre menor que max_h
    if (min_h >= max_h) next
    
    for (h_step in h_step_values) {
      for (max_d in max_d_values) {
        for (pixel_size in pixel_size_values) {
          for (min_density in min_density_values) {
            for (min_votes in min_votes_values) {
              #  tryCatch para lidar com erros
              tryCatch({
                # Rodar treeMap 
                map.D1 <- treeMap(thin_cut, map.hough(min_h = min_h, max_h = max_h, h_step = h_step,
                                                  max_d = max_d, pixel_size = pixel_size,
                                                  min_density = min_density, min_votes = min_votes))
                
                xymap.D1 = treeMap.positions(map.D1)
                
                # Criar uma lista com informações da combinação de parâmetros
                params_info <- data.frame(
                  combination = paste("min_h", min_h, "max_h", max_h, "h_step", h_step,
                                      "max_d", max_d, "pixel_size", pixel_size,
                                      "min_density", min_density, "min_votes", min_votes),
                  min_h = min_h,
                  max_h = max_h,
                  h_step = h_step,
                  max_d = max_d,
                  pixel_size = pixel_size,
                  min_density = min_density,
                  min_votes = min_votes
                )
                
                # Repetir as informações da combinação até que os parâmetros de teste mudem
                params_info <- params_info[rep(1, nrow(xymap.D1)), ]
                
                # Adicionar informações da combinação de parâmetros ao data frame
                result_data <- cbind(params_info, xymap.D1)
                
                # Adicionar resultados à lista
                results_list[[length(results_list) + 1]] <- result_data
                
                # Exportar results para o diretório especificado
                export_path <- file.path(export_dir_results, paste("result_", min_h, "_", max_h, "_", h_step, "_",
                                                                   max_d, "_", pixel_size, "_", min_density, "_",
                                                                   min_votes, ".csv", sep = ""))
                write.table(xymap.D1, file = export_path, sep = ",")
                
                # Atualizar barra de progresso com estimativa de tempo restante
                pb$tick()
                
              }, error = function(e) {
                # Imprimir mensagem de erro
                cat("Error:", conditionMessage(e), "\n")
                # Criar data frame com informações sobre a combinação de parâmetros que causou o erro
                error_info <- data.frame(
                  min_h = min_h,
                  max_h = max_h,
                  h_step = h_step,
                  max_d = max_d,
                  pixel_size = pixel_size,
                  min_density = min_density,
                  min_votes = min_votes,
                  error_message = conditionMessage(e)
                )
                
                # Adicionar data frame à lista de erros
                
              })
            }
          }
        }
      }
    }
  }
}

# Finalizar barra de progresso
pb$terminate()

# Combinar todos os resultados em um único data frame
all_results <- do.call(rbind, results_list)

# Exportar para arquivo CSV
export_path_results <- file.path(export_dir_results, "all_results_teste.csv")
write.table(all_results, file = export_path_results, sep = ",", row.names = FALSE)


# Registar tempo final
end_time <- Sys.time()

# Calcular tempo total decorrido
total_time <- end_time - start_time

# Imprimir tempo total
cat("Tempo total decorrido:", total_time, "\n")



########################################### ANÁLISE DE DADOS 1 ################################################

n_obs <- 69


###### MAP.HOUGH#####
map.hough_data <- read.csv2("C:/Users/Luiana/Desktop/ic_eric/testes_4/resultados_TreeTools/resultados_map.hough/results/all_results_teste.csv", sep = ",")
map.hough_narv_comb <- map.hough_data %>%
  group_by(combination, min_h, min_density, min_votes, pixel_size, h_step, max_h, max_d) %>%
  summarize(n.est = n_distinct(TreeID))%>%
  mutate( n.obs = n_obs)



map.hough_narv_comb <- map.hough_narv_comb%>%
  mutate(erro_quad = sqrt((as.numeric(n.est) - n.obs)^2)) #erro quadratico

str(map.hough_narv_comb)
# Converter para num
columns_to_convert <- c("min_h", "min_density", "pixel_size", "h_step", "max_h", "max_d")

map.hough_narv_comb <- map.hough_narv_comb %>%
  mutate_at(columns_to_convert, as.numeric)

write.csv(map.hough_narv_comb, "C:/Users/Luiana/Desktop/ic_eric/testes_4/resultados_TreeTools/resultados_map.hough/results/combinacoes.csv")

#filtrando dados 50% mais e 50% menos
map.hough_analise <- map.hough_narv_comb %>%
  filter(n.est >= 34.5  & n.est <= 103.5)
write.csv(map.hough_analise, "C:/Users/Luiana/Desktop/ic_eric/testes_4/resultados_TreeTools/resultados_map.hough/results/analise.csv")
######### PLOTAGEM DE LOCALIZAÇÃO DOS SELECIONADOS ######## DOS MELHORES PARÂMETROS# EXPORTOU OS MAPAS EM jpeg da localização dos mapas- entrou no tcc figura 5 do tcc##

# Iterar sobre as combinações de parâmetros em map.hough_analise

map.hough_analise <- read.csv("C:/Users/Luiana/Desktop/ic_eric/testes_4/resultados_TreeTools/resultados_map.hough/results/analise.csv")
head(map.hough_analise)
for (i in 1:nrow(map.hough_analise)) {
  # Extrair os valores de min_h, max_h, e h_step, min_density, min_votes, pixel_size, max_d_value
  min_h_value <- map.hough_analise$min_h[i]
  max_h_value <- map.hough_analise$max_h[i]
  h_step_value <- map.hough_analise$h_step[i]
  min_density_value <- map.hough_analise$min_density[i]
  min_votes_value <- map.hough_analise$min_votes[i]
  pixel_size_value <- map.hough_analise$pixel_size[i]
  max_d_value <- map.hough_analise$max_d[i]
  
  # Executar treeMap com os parâmetros extraídos
  map.D1 <- treeMap(thin_cut, map.hough(min_h = min_h_value, max_h = max_h_value, 
                                        h_step = h_step_value, max_d = max_d_value, 
                                        pixel_size = pixel_size_value, 
                                        min_density = min_density_value, 
                                        min_votes = min_votes_value))
  nome_arquivo <- paste("C:/Users/Luiana/Desktop/ic_eric/testes_4/resultados_TreeTools/resultados_map.hough/results/graficos/map_", map.hough_analise$R[i], ".jpeg", sep = "")
  jpeg(nome_arquivo)
  xymap.D1 = treeMap.positions(map.D1)
  dev.off()
}

######### CRIANDO PLOT PARA PONTOS DO DF DO INVENTARIO ######


# Ler o shapefile#da localização do inventário#
shp_data <- st_read("C:/Users/Luiana/Desktop/ic_eric/testes_4/Parcelas/SHP/parcela 2 rio preto 2022 epsg 31983.shp")
coords <- st_coordinates(shp_data)

# Definir manualmente os limites dos eixos
x_min <- 675335
x_max <- 675360
y_min <- 7996130
y_max <- 7996155

# Caminho do arquivo de saída JPEG
output_file <- "C:/Users/Luiana/Desktop/ic_eric/testes_4/resultados_TreeTools/resultados_map.hough/results/graficos/inventario_plot.jpeg"

# Configurações iniciais do gráfico e salvar como JPEG
jpeg(output_file, width = 2480, height = 2480, res = 300)  # Resolução para A4 em 300 dpi
par(xaxs="i", yaxs="i")
plot(coords[, 1], coords[, 2], main="Inventário", xlab="X", ylab="Y", pch=19, col='black', 
     xlim=c(x_min, x_max), ylim=c(y_min, y_max), axes=FALSE)
grid()
axis(side=1, col="red", lwd=3)
axis(side=2, col="red", lwd=3)
axis(side=3, col="red", lwd=3, labels=FALSE)
axis(side=4, col="red", lwd=3)
dev.off()
