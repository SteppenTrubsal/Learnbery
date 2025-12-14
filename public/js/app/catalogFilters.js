import { bus } from '../core/bus.js';
import { ajaxClient } from '../core/ajax.js';
import { BUS_EVENTS, msgEvent } from '../core/types.js';

function debounce(fn, ms) {
  let t = null;
  return (...args) => {
    clearTimeout(t);
    t = setTimeout(() => fn(...args), ms);
  };
}

class CatalogFilters {
  init() {
    this.qEl = document.getElementById('catalog-q');

    // INPUT + DATALIST
    this.authorEl = document.getElementById('catalog-author');
    this.genreEl = document.getElementById('catalog-genre');
    this.authorListEl = document.getElementById('catalog-author-list');
    this.genreListEl = document.getElementById('catalog-genre-list');

    this.yFromEl = document.getElementById('catalog-year-from');
    this.yToEl = document.getElementById('catalog-year-to');
    this.resetEl = document.getElementById('catalog-reset');

    if (!this.qEl || !this.authorEl || !this.genreEl) return;

    this.authorByName = new Map(); // lower(name) -> id
    this.genreByName = new Map();

    // ВАЖНО: у тебя loader ходит на /books, значит фильтры тоже берём без /api
    ajaxClient.request({
      path: '/catalog/filters',
      method: 'GET',
      type: 'catalog:filters',
    });

    bus.on(msgEvent('catalog:filters'), ({ payload }) => {
      const authors = Array.isArray(payload?.authors) ? payload.authors : [];
      const genres  = Array.isArray(payload?.genres) ? payload.genres : [];

      this.fillDatalist(this.authorListEl, authors, this.authorByName);
      this.fillDatalist(this.genreListEl,  genres,  this.genreByName);

      const years = payload?.years || {};
      if (this.yFromEl && years.min != null) this.yFromEl.placeholder = String(years.min);
      if (this.yToEl && years.max != null) this.yToEl.placeholder = String(years.max);
    });

    const emit = () => this.emitCurrent();

    this.qEl.addEventListener('input', debounce(emit, 250));
    this.authorEl.addEventListener('input', debounce(emit, 200));
    this.genreEl.addEventListener('input', debounce(emit, 200));
    if (this.yFromEl) this.yFromEl.addEventListener('change', emit);
    if (this.yToEl) this.yToEl.addEventListener('change', emit);

    if (this.resetEl) {
      this.resetEl.addEventListener('click', () => {
        this.qEl.value = '';
        this.authorEl.value = '';
        this.genreEl.value = '';
        if (this.yFromEl) this.yFromEl.value = '';
        if (this.yToEl) this.yToEl.value = '';
        this.emitCurrent();
      });
    }

    this.emitCurrent();
  }

  fillDatalist(listEl, items, mapByName) {
    if (!listEl) return;
    listEl.innerHTML = '';
    mapByName.clear();

    for (const it of items) {
      const name = String(it.name ?? '').trim();
      const id = Number(it.id);
      if (!name || Number.isNaN(id)) continue;

      mapByName.set(name.toLowerCase(), id);

      const opt = document.createElement('option');
      opt.value = name;
      listEl.appendChild(opt);
    }
  }

  resolveExactId(value, mapByName) {
    const key = (value || '').trim().toLowerCase();
    if (!key) return null;
    return mapByName.get(key) ?? null;
  }

  emitCurrent() {
    const q = (this.qEl.value || '').trim();

    const authorText = (this.authorEl.value || '').trim();
    const genreText  = (this.genreEl.value || '').trim();

    const authorId = this.resolveExactId(authorText, this.authorByName);
    const genreId  = this.resolveExactId(genreText,  this.genreByName);

    // если точного совпадения нет — шлём текстовый фильтр author_q/genre_q
    const filters = {
      q,

      author: authorId,
      genre: genreId,

      author_q: authorId == null && authorText ? authorText : null,
      genre_q:  genreId  == null && genreText  ? genreText  : null,

      year_from: this.yFromEl && this.yFromEl.value ? Number(this.yFromEl.value) : null,
      year_to:   this.yToEl   && this.yToEl.value   ? Number(this.yToEl.value)   : null,
    };

    bus.emit(BUS_EVENTS.UI.CATALOG.FILTER_CHANGED, filters);
  }
}

export const catalogFilters = new CatalogFilters();
