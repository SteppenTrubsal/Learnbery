import { bus } from '../core/bus.js';
import { router } from '../core/router.js';
import { protocolHub } from './protocol.js';
import { bookDetailsPopup } from './popup.js';
import { panelManager } from './panels.js';
import { BUS_EVENTS } from '../core/types.js';

import { catalogLoader } from './catalogLoader.js';
import { catalogFilters } from './catalogFilters.js';

import { fortuneWheel } from './fortune.js';

function initApp() {
  router.init();
  protocolHub.init();
  bookDetailsPopup.init();
  panelManager.init();

  catalogLoader.init();
  catalogFilters.init();

  fortuneWheel.init();
}

window.initApp = initApp;

// window.onBookClick = function (bookId, el) {
//   if (!bookId) return;
//   bus.emit(BUS_EVENTS.UI.BOOK.CLICK, { id: bookId, el });
// };

window.onBookHover = function (bookId, el) {
  if (!bookId) return;
  bus.emit(BUS_EVENTS.UI.BOOK.HOVER, { id: bookId, el });
};

document.addEventListener('DOMContentLoaded', initApp);
