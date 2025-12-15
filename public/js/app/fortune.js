function easeOutQuart(t) {
  return 1 - Math.pow(1 - t, 4);
}

function normDeg(a) {
  a = a % 360;
  return a < 0 ? a + 360 : a;
}

export class FortuneWheel {
  constructor() {
    this.chart = null;
    this.labels = [];
    this.ids = [];

    this.rotation = 0;      // сохраняется между спинами
    this.isSpinning = false;

    this.canvas = null;
    this.btn = null;
    this.result = null;

    this.isLoaded = false;  // книги загружены
    this.isBuilt = false;   // chart создан
  }

  init() {
    // только навешиваем “вход на панель”
    // PanelManager управляет классом is-active.
    document.addEventListener('click', () => {}); // no-op, чтобы модуль был загружен

    // подписка на показ панели: просто проверяем по hash и по активному классу
    // Надёжнее: слушать BUS_EVENTS.UI.NAV.GOTO — но ты уже используешь panelManager.
    window.addEventListener('hashchange', () => this.onPanelMaybeShown());
    // и первый раз
    this.onPanelMaybeShown();
  }

  async onPanelMaybeShown() {
    // панель должна существовать и быть активной
    const panel = document.getElementById('fortune');
    if (!panel || !panel.classList.contains('is-active')) return;

    // инициализация DOM ссылок
    if (!this.canvas) this.canvas = document.getElementById('fortuneChart');
    if (!this.btn) this.btn = document.getElementById('fortuneSpin');
    if (!this.result) this.result = document.getElementById('fortuneResult');

    if (!this.canvas || !this.btn || !this.result) return;

    // Загружаем книги один раз
    if (!this.isLoaded) {
      const data = await this.fetchBooks();
      this.labels = data.map((x) => x.title);
      this.ids = data.map((x) => x.id);
      this.isLoaded = true;

      if (!this.labels.length) {
        this.result.textContent = 'В базе пока нет книг для колеса.';
        this.btn.disabled = true;
        return;
      }
    }

    // Создаём chart один раз
    if (!this.isBuilt) {
      this.chart = this.makeChart(this.canvas, this.labels);
      this.isBuilt = true;

      this.result.textContent = 'Нажми кнопку, чтобы выбрать книгу случайно.';
      this.btn.addEventListener('click', () => this.spin());
    } else {
      // Если уже был создан — просто убедимся, что rotation восстановлен
      this.chart.data.datasets[0].rotation = this.rotation;
      this.chart.update('none');
    }
  }

  async fetchBooks() {
    const r = await fetch('/fortune', { method: 'GET' });
    if (!r.ok) return [];
    const arr = await r.json();
    return Array.isArray(arr) ? arr : [];
  }

  makeChart(canvas, labels) {
    const values = labels.map(() => 1);

    return new Chart(canvas.getContext('2d'), {
      type: 'doughnut',
      data: {
        labels,
        datasets: [
          {
            data: values,
            borderWidth: 1,
            rotation: this.rotation, // ✅ rotation на dataset
          },
        ],
      },
      options: {
        responsive: true,
        maintainAspectRatio: true,
        cutout: '55%',
        animation: false, // ✅ не даём Chart анимировать
        transitions: {
          active: { animation: { duration: 0 } },
        },
        plugins: {
          legend: { display: false },
          tooltip: {
            callbacks: {
              label: (ctx) => ctx.label || '',
            },
          },
        },
      },
    });
  }


  spin() {
    if (!this.chart || this.isSpinning) return;

    const n = this.labels.length;
    if (n === 0) return;

    this.isSpinning = true;
    this.btn.disabled = true;
    this.result.textContent = 'Крутим…';

    const sectorDeg = 360 / n;

    const startDeg = this.rotationDeg ?? 0;

    const randomEndDeg = Math.random() * 360;
    const extraSpins = 18 + Math.floor(Math.random() * 10); // 18..27 (подстрой)
    const endDeg = startDeg + extraSpins * 360 + randomEndDeg;

    const duration = 3200;
    const t0 = performance.now();

    const tick = (now) => {
      const t = Math.min(1, (now - t0) / duration);
      const k = easeOutQuart(t);

      const curDeg = startDeg + (endDeg - startDeg) * k;

    // ✅ Chart.js rotation = градусы
      this.rotationDeg = curDeg;
      this.chart.data.datasets[0].rotation = this.rotationDeg;
      this.chart.update('none');

      if (t < 1) {
        requestAnimationFrame(tick);
      } else {
        // фиксируем финальный угол
        const rot = normDeg(curDeg);
        this.rotationDeg = rot;

      // верх = -90deg (т.е. 12 часов), что на верху сейчас:
        const topDeg = normDeg(-90 - rot);
        const winnerIndex = Math.floor(topDeg / sectorDeg) % n;

      // применяем нормализованный угол (без скачка!)
        this.chart.data.datasets[0].rotation = this.rotationDeg;
        this.chart.update('none');

        this.isSpinning = false;
        this.btn.disabled = false;

        const title = this.labels[winnerIndex] || 'Без названия';
        this.result.textContent = `Выпало: ${title}`;
      }
    };

    requestAnimationFrame(tick);
  }
}

export const fortuneWheel = new FortuneWheel();
