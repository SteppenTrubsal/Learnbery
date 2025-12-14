import { bus } from '../core/bus.js';
import { BUS_EVENTS } from '../core/types.js';
import { bookDetailsPopup } from './popup.js';

class PanelManager {
  init() {
    this.panels = Array.from(document.querySelectorAll('.app-panel'));
    if (!this.panels.length) return;

    // клики по меню
    document.querySelectorAll('[data-panel]').forEach((el) => {
      el.addEventListener('click', (e) => {
        e.preventDefault();
        const panel = el.dataset.panel;
        bus.emit(BUS_EVENTS.UI.NAV.GOTO, { panel });
        history.replaceState(null, '', `#${panel}`);
      });
    });

    // слушаем шину
    bus.on(BUS_EVENTS.UI.NAV.GOTO, ({ panel }) => {
      bookDetailsPopup.hide();   // <-- закрываем попап при смене панели
      this.show(panel);
    });

    // начальный выбор
    const initial = (location.hash || '#home').slice(1);
    this.show(initial);
  }

  show(panelId) {
    const id = panelId || 'home';
    for (const p of this.panels) {
      p.classList.toggle('is-active', p.id === id);
    }
  }
}

export const panelManager = new PanelManager();
