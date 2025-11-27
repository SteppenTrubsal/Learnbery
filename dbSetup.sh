#!/usr/bin/env bash
set -euo pipefail

### ========= НАСТРОЙКИ =========

# Имя системного пользователя PostgreSQL (обычно postgres)
PG_SYSTEM_USER="postgres"

# Каталог для кластера
PGDATA="/var/lib/postgres/data"

# Локаль и кодировка (под себя подправь при желании)
PG_LOCALE="en_US.UTF-8"
PG_ENCODING="UTF8"

# Пользователь БД, которого хотим создать
DB_USER="library_user"
DB_PASSWORD="super_secret_password"

### ======== КОНЕЦ НАСТРОЕК ========

echo "[*] Инициализация кластера в ${PGDATA}"

if [ -d "$PGDATA" ] && [ "$(ls -A "$PGDATA" 2>/dev/null | wc -l)" -ne 0 ]; then
  echo "[!] Каталог ${PGDATA} уже существует и не пустой. Прерываемся."
  exit 1
fi

# Создаём каталог, если его нет
mkdir -p "$PGDATA"
chown "$PG_SYSTEM_USER":"$PG_SYSTEM_USER" "$PGDATA"

# initdb
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

# Ждём, пока сервер поднимется
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

echo "[*] Создаём пользователя БД '${DB_USER}'"

# Важно: пароль не должен содержать одиночную кавычку, иначе надо экранировать сложнее
sudo -u "$PG_SYSTEM_USER" psql -h /tmp -d postgres <<EOF
DO
\$
BEGIN
  IF NOT EXISTS (
    SELECT FROM pg_catalog.pg_roles WHERE rolname = '$DB_USER'
  ) THEN
    CREATE ROLE "$DB_USER" WITH LOGIN PASSWORD '$DB_PASSWORD';
  END IF;
END
\$
;
EOF

echo "[*] Останавливаем временный сервер"

sudo -u "$PG_SYSTEM_USER" pg_ctl -D "$PGDATA" stop

echo "[✓] Готово."
echo "    Кластер: $PGDATA"
echo "    Пользователь БД: $DB_USER"
