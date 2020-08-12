$directorioPrincipal = "C:\Users\Daniel\OneDrive\Documents\(Proyecto)_Estudio_PKPD"
$subdirectorio = "$directorioPrincipal\VANCOMICINA\03_RESIDUAL\M2CPTM_nobs_2_comb1\results\boot"

for ($i=1; $i -le 1000; $i++) 
  {
    $StartTime = $(get-date)
    "Carpeta $i iniciada"
    $Var = "$subdirectorio\B$i\M2CPTM_nobs_2_comb1.mlxtran"
    C:\ProgramData\Lixoft\MonolixSuite2019R2\bin\monolix.bat --no-gui --thread 8 --mode "none" -p $Var
  
    $elapsedTime = $(get-date) - $StartTime
    $totalTime = "{0:HH:mm:ss}" -f ([datetime]$elapsedTime.Ticks)
    $totalTime
  }