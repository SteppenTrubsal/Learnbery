import { bus } from './bus.js';
import { router } from './router.js';
import { protocolHub } from './protocol.js';
import { bookDetailsPopup } from './popup.js';
import { BUS_EVENTS } from './types.js';

function initApp() {
  router.init();
  protocolHub.init();
  bookDetailsPopup.init();
}

window.initApp = initApp;

window.onBookClick = function (bookId) {
  if (!bookId) return;
  bus.emit(BUS_EVENTS.UI.BOOK.CLICK, { id: bookId });
};

document.addEventListener('DOMContentLoaded', initApp);
