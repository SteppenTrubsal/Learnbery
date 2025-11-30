#!/usr/bin/env bash
set -euo pipefail

PG_SYSTEM_USER="postgres"
PGDATA="/var/lib/postgres/data"
PG_LOCALE="en_US.UTF-8"
PG_ENCODING="UTF8"

DB_USER="testUser"
DB_PASSWORD="12345678"
DB_NAME="learnbery"

SCHEMA_FILE="./init.sql"

echo "[*] Проверяем наличие файла схемы: ${SCHEMA_FILE}"
if [ ! -f "$SCHEMA_FILE" ]; then
  echo "[!] Файл схемы '${SCHEMA_FILE}' не найден. Прерываемся."
  exit 1
fi

echo "[*] Инициализация кластера в ${PGDATA}"

if [ -d "$PGDATA" ] && [ "$(ls -A "$PGDATA" 2>/dev/null | wc -l)" -ne 0 ]; then
  echo "[!] Каталог ${PGDATA} уже существует и не пустой. Прерываемся."
  exit 1
fi

mkdir -p "$PGDATA"
chown "$PG_SYSTEM_USER":"$PG_SYSTEM_USER" "$PGDATA"

echo "[*] Запускаем initdb"
sudo -u "$PG_SYSTEM_USER" initdb \
  --locale="$PG_LOCALE" \
  -E "$PG_ENCODING" \
  -D "$PGDATA"

echo "[*] Запускаем временный сервер PostgreSQL"

LOGFILE="${PGDATA}/initdb.log"

sudo -u "$PG_SYSTEM_USER" pg_ctl -D "$PGDATA" \
  -o "-k /tmp" \
  -l "$LOGFILE" \
  start

echo "[*] Ждём, пока сервер будет готов..."
for i in {1..30}; do
  if sudo -u "$PG_SYSTEM_USER" pg_isready -q -d "postgres" -h /tmp; then
    break
  fi
  sleep 1
done

if ! sudo -u "$PG_SYSTEM_USER" pg_isready -q -d "postgres" -h /tmp; then
  echo "[!] Сервер так и не поднялся, смотри лог: $LOGFILE"
  sudo -u "$PG_SYSTEM_USER" pg_ctl -D "$PGDATA" stop || true
  exit 1
fi

echo "[*] Создаём пользователя БД '${DB_USER}' (если нет)"

sudo -u "$PG_SYSTEM_USER" psql -h /tmp -d postgres <<EOF
DO \$\$
BEGIN
  IF NOT EXISTS (
    SELECT FROM pg_catalog.pg_roles WHERE rolname = '$DB_USER'
  ) THEN
    CREATE ROLE "$DB_USER" WITH LOGIN PASSWORD '$DB_PASSWORD';
  END IF;
END
\$\$;
EOF

echo "[*] Создаём базу данных '${DB_NAME}' (если нет)"

sudo -u "$PG_SYSTEM_USER" psql -h /tmp -d postgres <<EOF
DO \$\$
BEGIN
  IF NOT EXISTS (
    SELECT FROM pg_database WHERE datname = '$DB_NAME'
  ) THEN
    CREATE DATABASE "$DB_NAME" OWNER "$DB_USER";
  END IF;
END
\$\$;
EOF

echo "[*] Настраиваем права на схему public в базе '${DB_NAME}'"

sudo -u "$PG_SYSTEM_USER" psql -h /tmp -d "$DB_NAME" <<EOF
ALTER SCHEMA public OWNER TO "$DB_USER";
GRANT ALL ON SCHEMA public TO "$DB_USER";
EOF

echo "[*] Накатываем схему из '${SCHEMA_FILE}' в базу '${DB_NAME}'"

sudo -u "$PG_SYSTEM_USER" psql -h /tmp -d "$DB_NAME" -f "$SCHEMA_FILE"

echo "[*] Останавливаем временный сервер"

sudo -u "$PG_SYSTEM_USER" pg_ctl -D "$PGDATA" stop

echo "[✓] Готово."
echo "    Кластер:   $PGDATA"
echo "    Пользователь: $DB_USER"
echo "    База данных:  $DB_NAME"
echo "    Схема:        $SCHEMA_FILE применена"
