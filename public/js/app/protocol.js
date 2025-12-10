import { bus } from '../core/bus.js';
import { BUS_EVENTS } from '../core/types.js';
import { ajaxClient } from '../core/ajax.js';

class ProtocolHub {
  constructor() {
    this.initialized = false;
  }

  init() {
    if (this.initialized) return;
    this.initialized = true;

    // bus.on(BUS_EVENTS.UI.BOOK.CLICK, ({ id }) => {
    //   if (!id) return;
    //   ajaxClient.request({
    //     path: `/api/book/${encodeURIComponent(id)}`,
    //     method: 'GET',
    //     type: 'book:details',
    //   });
    // });

    bus.on(BUS_EVENTS.UI.BOOK.HOVER, ({ id }) => {
      if (!id) return;
      ajaxClient.request({
        path: `/book/${encodeURIComponent(id)}`,
        method: 'GET',
        type: 'book:details',
      });
    });
  }
}

export const protocolHub = new ProtocolHub();
