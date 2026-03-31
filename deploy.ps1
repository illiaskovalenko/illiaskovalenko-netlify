# Go to project root (in case script is run from elsewhere)
Set-Location $PSScriptRoot

Write-Host "Rendering Quarto..."
quarto render --profile def --no-clean
quarto render --profile fr --no-clean
quarto render --profile en --no-clean

Write-Host "Git add..."
git add .

Write-Host "Git commit..."
git commit -m "Update site"

Write-Host "Git push..."
git push

Write-Host "Done."