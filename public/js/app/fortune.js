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

    this.rotationDeg = 0;
    this.rotationDegDisplay = 0;
    this.isSpinning = false;

    this.canvas = null;
    this.btn = null;
    this.result = null;

    this.isLoaded = false;
    this.isBuilt = false;
  }

  toChartRotation(deg) {
    return deg;
  }

  init() {
    document.addEventListener('click', () => {});
    window.addEventListener('hashchange', () => this.onPanelMaybeShown());
    this.onPanelMaybeShown();
  }

  async onPanelMaybeShown() {
    const panel = document.getElementById('fortune');
    if (!panel || !panel.classList.contains('is-active')) return;

    if (!this.canvas) this.canvas = document.getElementById('fortuneChart');
    if (!this.btn) this.btn = document.getElementById('fortuneSpin');
    if (!this.result) this.result = document.getElementById('fortuneResult');
    if (!this.canvas || !this.btn || !this.result) return;

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

    if (!this.isBuilt) {
      this.chart = this.makeChart(this.canvas, this.labels);
      this.isBuilt = true;

      this.result.textContent = 'Нажми кнопку, чтобы выбрать книгу случайно.';
      this.btn.addEventListener('click', () => this.spin());
    } else {
      this.rotationDegDisplay = this.rotationDeg;
      this.chart.data.datasets[0].rotation = this.rotationDegDisplay;
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
            rotation: this.rotationDegDisplay,
          },
        ],
      },
      options: {
        responsive: true,
        maintainAspectRatio: true,
        cutout: '55%',
        animation: false,
        transitions: { active: { animation: { duration: 0 } } },
        plugins: {
          legend: { display: false },
          tooltip: { callbacks: { label: (ctx) => ctx.label || '' } },
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

    const startDeg = this.rotationDegDisplay || 0;

    const randomEndDeg = Math.random() * 360;
    const extraSpins = 8 + Math.floor(Math.random() * 10);
    const endDeg = startDeg + extraSpins * 360 + randomEndDeg;

    const duration = 3200;
    const t0 = performance.now();

    const tick = (now) => {
      const t = Math.min(1, (now - t0) / duration);
      const k = easeOutQuart(t);

      const curDeg = startDeg + (endDeg - startDeg) * k;

      this.rotationDegDisplay = curDeg;
      this.chart.data.datasets[0].rotation = this.rotationDegDisplay;
      this.chart.update('none');

      if (t < 1) {
        requestAnimationFrame(tick);
        return;
      }

      const rot = normDeg(this.rotationDegDisplay);
      this.rotationDeg = rot;

      const pointerDeg = 0;

      const topDeg = normDeg(pointerDeg - rot);

      const eps = 1e-9;
      const winnerIndex = Math.floor((topDeg + eps) / sectorDeg) % n;

      this.isSpinning = false;
      this.btn.disabled = false;

      const title = this.labels[winnerIndex] || 'Без названия';
      this.result.textContent = `Выпало: ${title}`;
    };
    requestAnimationFrame(tick);
  }
}

export const fortuneWheel = new FortuneWheel();