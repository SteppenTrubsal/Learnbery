import { bus } from '../core/bus.js';
import { BUS_EVENTS } from '../core/types.js';
import { bookDetailsPopup } from './popup.js';
import { fortuneWheel } from './fortune.js';

class PanelManager {
  init() {
    this.panels = Array.from(document.querySelectorAll('.app-panel'));
    if (!this.panels.length) return;

    document.querySelectorAll('[data-panel]').forEach((el) => {
      el.addEventListener('click', (e) => {
        e.preventDefault();
        const panel = el.dataset.panel;
        bus.emit(BUS_EVENTS.UI.NAV.GOTO, { panel });
        history.replaceState(null, '', `#${panel}`);
      });
    });

    bus.on(BUS_EVENTS.UI.NAV.GOTO, ({ panel }) => {
      bookDetailsPopup.hide();
      this.show(panel);
      fortuneWheel.onPanelMaybeShown?.();
    });

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
