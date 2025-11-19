import { bus } from './bus.js';
import { BUS_EVENTS } from './types.js';

class AjaxClient {
  constructor(baseUrl = '') {
    this.baseUrl = baseUrl;
  }

  /**
   * options:
   *   path:   '/books/42'
   *   method: 'GET' | 'POST' | ...
   *   payload: объект, который пойдет в body (для не-GET)
   *   type:  логический тип сообщения, например 'book:details'
   */
  async request({ path, method = 'GET', payload = null, type }) {
    const url = this.baseUrl + path;

    const options = {
      method,
      headers: {
        'Accept': 'application/json',
      },
    };

    if (method !== 'GET' && payload != null) {
      options.headers['Content-Type'] = 'application/json';
      options.body = JSON.stringify(payload);
    }

    const resp = await fetch(url, options);
    const data = await resp.json().catch(() => ({}));

    const msg = {
      type,
      payload: data,
    };

    bus.emit(BUS_EVENTS.AJAX.RESPONSE, msg);

    return msg;
  }
}

export const ajaxClient = new AjaxClient('/api');