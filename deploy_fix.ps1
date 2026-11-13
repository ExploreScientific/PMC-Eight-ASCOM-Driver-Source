# Script para instalar el driver ASCOM modificado con permisos de Administrador
$sourceDll = "C:\Users\agusb\PMC-Eight-ASCOM-Driver-Source\bin\Release\ASCOM.ES_PMC8.Telescope.dll"
$targetDir = "C:\Program Files (x86)\Common Files\ASCOM\Telescope"
$targetDll = Join-Path $targetDir "ASCOM.ES_PMC8.Telescope.dll"

Write-Host "Copiando driver corregido a Common Files..." -ForegroundColor Cyan
Copy-Item -Path $sourceDll -Destination $targetDll -Force

Write-Host "Registrando en COM con RegAsm..." -ForegroundColor Cyan
& "C:\Windows\Microsoft.NET\Framework\v4.0.30319\RegAsm.exe" /codebase $targetDll

Write-Host "`n¡Driver instalado y registrado con éxito!" -ForegroundColor Green
pause
