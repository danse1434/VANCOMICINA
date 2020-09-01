# Ejecución de archivos para evaluación de convergencia
$mainDir = "C:\Users\Daniel\OneDrive\Documents\(Proyecto)_Estudio_PKPD\VANCOMICINA\" 
$auxDir = "$mainDir\05_MINIMIZACION\01_evaluacionFactorial\evaluacion\"

for ($i=1; $i -le 81; $i++) 
{
  $StartTime = $(get-date)
  "Carpeta $i iniciada"
  $Var = "$auxDir\evaluacion$i\M2CPTM_nobs_1_prop.mlxtran"
  C:\ProgramData\Lixoft\MonolixSuite2019R2\bin\monolix.bat --no-gui --thread 8 --mode "none" -p $Var
  $elapsedTime = $(get-date) - $StartTime
  $totalTime = "{0:HH:mm:ss}" -f ([datetime]$elapsedTime.Ticks)
  $totalTime
}
