@echo off
chcp 65001 >nul
cd /d "%~dp0"
set "PATH=%PATH%;C:\ghcup\bin;C:\cabal\bin;%APPDATA%\cabal\bin;%LOCALAPPDATA%\ghcup\bin"

where cabal >nul 2>nul
if not errorlevel 1 goto have_haskell
echo Haskell не найден, устанавливаю GHCup. Это займёт несколько минут.
powershell -NoProfile -ExecutionPolicy Bypass -Command "[Net.ServicePointManager]::SecurityProtocol=[Net.ServicePointManager]::SecurityProtocol -bor 3072; Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true"
where cabal >nul 2>nul
if not errorlevel 1 goto have_haskell
echo Установка завершена. Закройте это окно и запустите run.bat ещё раз.
pause
exit /b 1

:have_haskell
if exist token.txt goto have_token
set /p "BOT_TOKEN=Вставьте токен бота от BotFather: "
>token.txt echo(%BOT_TOKEN%

:have_token
set /p BOT_TOKEN=<token.txt

if exist admins.txt goto have_admins
set /p "BOT_ADMINS=Ваш Telegram ID для команды /stats (Enter, чтобы пропустить): "
>admins.txt echo(%BOT_ADMINS%

:have_admins
set /p BOT_ADMINS=<admins.txt

echo Обновляю список пакетов...
call cabal update
if errorlevel 1 goto failed

echo Собираю проект. В первый раз это долго.
call cabal build
if errorlevel 1 goto failed

echo Бот запущен. Чтобы остановить, нажмите Ctrl+C.
call cabal run deckbot
goto done

:failed
echo.
echo Сборка не удалась. Пришлите текст ошибки выше.

:done
pause
