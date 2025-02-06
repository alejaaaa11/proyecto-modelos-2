set terminal png
set output 'generos.png'
set title 'Géneros más prestadas'
set xlabel 'genero / libro'
set ylabel 'Número de Préstamos'
set style data histograms
set style fill solid
set boxwidth 0.5
set xtic rotate by -45
plot 'generos_data.txt' using 2:xtic(1) with histogram
