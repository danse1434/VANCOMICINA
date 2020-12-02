# Directorio principal
$main_dir = "C:\Users\Daniel\OneDrive\Documents\(Proyecto)_Estudio_PKPD\VANCOMICINA\05_MINIMIZACION\03_mapeoFuncionLL\results"
 
            
function Analizar-LL {
<#
.SYNOPSIS
    Hacer un análisis de verosimilitud univariado
.DESCRIPTION
    Esta función ejecuta las carpetas prediseñadas para hacer evaluación verosimilitud.
.PARAMETER parameter_name
    Nombre del parámetro donde están las tareas
.PARAMETER n_folders
    Nombre del parámetro donde están las tareas
.EXAMPLE
    Analizar-LL("Cl_pop")
.INPUTS
    string
.OUTPUTS
    Ninguno, efecto secundario llena las carpetas con un análisis
#>
    param (
        [Parameter(Mandatory=$true)][string]$parameter_name,
        [Parameter(Mandatory=$false)][int]$n_folders = 100
    )
    $aux_dir = "$main_dir\$parameter_name"

    for ($i=1; $i -le $n_folders; $i++) 
    {
      $StartTime = $(get-date)
      "Proyecto $i  =>  iniciado"
      $Var = "$aux_dir\A$i\M2CPTM_nobs_1_prop.mlxtran"
      C:\ProgramData\Lixoft\MonolixSuite2019R2\bin\monolix.bat --no-gui --nosplash --thread 8 --mode "none" -p $Var
      $elapsedTime = $(get-date) - $StartTime
      $totalTime = "{0:HH:mm:ss}" -f ([datetime]$elapsedTime.Ticks)
      $totalTime
    }

    [console]::Beep(1000,1000)
}

# Aplicar el análisis multivariado
Analizar-LL "Cl_pop" 100
Analizar-LL "Q_pop" 100
Analizar-LL "V1_pop" 100
Analizar-LL "V2_pop" 100