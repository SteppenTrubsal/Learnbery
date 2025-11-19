import { bus } from './bus.js';
import { BUS_EVENTS, msgEvent } from './types.js';

class MessageRouter {
  constructor() {
    this.initialized = false;
    this.handlers = new Map();
  }

  init() {
    if (this.initialized) return;
    this.initialized = true;

    bus.on(BUS_EVENTS.AJAX.RESPONSE, this.handleMessage.bind(this));
  }

  handleMessage(msg) {
    if (Array.isArray(msg)) {
      for (const m of msg) this._handleOne(m);
      return;
    }
    this._handleOne(msg);
  }

  _handleOne(msg) {
    const norm = normalizeMessage(msg);
    if (!norm) {
      return;
    }

    const { type, payload } = norm;

    bus.emit(msgEvent(type), { type, payload, raw: msg });

    const set = this.handlers.get(type);
    if (set) {
      for (const fn of Array.from(set)) {
        fn({ type, payload, raw: msg });
      }
    }
  }

  registerHandler(type, handler) {
    if (!this.handlers.has(type)) {
      this.handlers.set(type, new Set());
    }
    const set = this.handlers.get(type);
    set.add(handler);

    return () => {
      set.delete(handler);
      if (set.size === 0) {
        this.handlers.delete(type);
      }
    };
  }
}

export const router = new MessageRouter();

function normalizeMessage(msg) {
  if (!msg || typeof msg !== 'object' || Array.isArray(msg)) return null;
  if (typeof msg.type !== 'string') return null;
  const type = msg.type.trim();
  if (!type.includes(':')) return null;

  const payload =
    msg.payload && typeof msg.payload === 'object'
      ? msg.payload
      : {};

  return { type, payload };
}
