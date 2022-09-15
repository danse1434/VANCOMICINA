## 
## Control de ejecución de bootstrap no paramétrico
## 

$startTime = Get-Date;
$N = 1000

$directorioPrincipal = "C:\Users\Daniel\OneDrive\Documents\(Proyecto)_Estudio_PKPD\VANCOMICINA\07_FINAL\50_Quimioluminiscencia"
$corrida = "run200"
$subdirectorio = "$directorioPrincipal\$corrida\boot_results"

for ($i=1; $i -le $N; $i++) 
  {
    #$StartTime = $(get-date)

    $elapsedTime = $(Get-Date) - $startTime
    $estimTotalSeconds1 = $elapsedTime.TotalSeconds
    $estimTaskTime = [Math]::Round(($N - $i) * ($estimTotalSeconds1/$i))
    $percentComplete = [Math]::Round(($i)*100/$N, 3)

    Write-Progress -Activity "Bootstrap en Progreso" -Status "$percentComplete % Completo:" -PercentComplete $percentComplete -SecondsRemaining $estimTaskTime;
    
    if([Math]::Round($i%50) -eq 0){
        "Carpeta $i iniciada"
    }
    
    $id = '{0:d3}' -f [int]$i
    
    $Var = "$subdirectorio\B$id\$corrida.mlxtran"
    C:\ProgramData\Lixoft\MonolixSuite2021R1\bin\monolix.bat --no-gui --thread 8 --mode "none" -p $Var
  
    #$elapsedTime = $(get-date) - $StartTime
    #$totalTime = "{0:HH:mm:ss}" -f ([datetime]$elapsedTime.Ticks)
    #$totalTime
  }