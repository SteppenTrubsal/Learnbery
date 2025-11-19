import { bus } from '../core/bus.js';
import { router } from '../core/router.js';
import { protocolHub } from './protocol.js';
import { bookDetailsPopup } from './popup.js';
import { BUS_EVENTS } from '../core/types.js';

function initApp() {
  router.init();
  protocolHub.init();
  bookDetailsPopup.init();
}

window.initApp = initApp;

window.onBookClick = function (bookId, el) {
  if (!bookId) return;
  bus.emit(BUS_EVENTS.UI.BOOK.CLICK, { id: bookId, el });
};

document.addEventListener('DOMContentLoaded', initApp);
