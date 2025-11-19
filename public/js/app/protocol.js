import { bus } from './bus.js';
import { BUS_EVENTS } from './types.js';
import { ajaxClient } from './ajax.js';

class ProtocolHub {
  constructor() {
    this.initialized = false;
  }

  init() {
    if (this.initialized) return;
    this.initialized = true;

    bus.on(BUS_EVENTS.UI.BOOK.CLICK, ({ id }) => {
      if (!id) return;

      ajaxClient.request({
        path: `/books/${encodeURIComponent(id)}`,
        method: 'GET',
        type: 'book:details',
      });
    });
  }
}

export const protocolHub = new ProtocolHub();
