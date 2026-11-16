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

if exist apikey.txt goto have_key
echo.
echo Чтобы бот сам писал слайды по теме, нужен ключ нейросети. Подходит любой из них:
echo   Groq (console.groq.com), ключ начинается с gsk_
echo   OpenRouter (openrouter.ai), ключ начинается с sk-or-
echo   Anthropic (console.anthropic.com), ключ начинается с sk-ant-
echo Без ключа бот работает только в ручном режиме. Можно пропустить.
set /p "AI_API_KEY=Ключ (Enter, чтобы пропустить): "
>apikey.txt echo(%AI_API_KEY%

:have_key
set /p AI_API_KEY=<apikey.txt

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
