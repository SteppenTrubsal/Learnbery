class Bus {
  constructor() {
    this.listeners = new Map();
  }

  on(event, callback) {
    if (!this.listeners.has(event)) {
      this.listeners.set(event, new Set());
    }
    const set = this.listeners.get(event);
    set.add(callback);

    return () => set.delete(callback);
  }

  emit(event, data) {
    const set = this.listeners.get(event);
    if (!set) return;

    for (const cb of Array.from(set)) {
      cb(data);
    }
  }
}

export const bus = new Bus();
