import { bus } from '../core/bus.js';
import { ajaxClient } from '../core/ajax.js';
import { msgEvent, BUS_EVENTS } from '../core/types.js';

class CatalogLoader {
  constructor() {
    this.offset = 0;
    this.limit = 20;
    this.loading = false;
    this.allLoaded = false;
    this.container = null;

    this.filters = {
      q: '',
      author: null,
      genre: null,
      year_from: null,
      year_to: null,
    };
  }

  init() {
    this.container = document.getElementById('book-list');
    if (!this.container) return;

    // Слушаем изменения фильтров (panel/форма)
    bus.on(BUS_EVENTS.UI.CATALOG.FILTER_CHANGED, (filters) => {
      this.applyFilters(filters);
    });

    // первая загрузка (если фильтры не пушнутся сами)
    this.loadMore();

    window.addEventListener('scroll', () => {
      if (this.loading || this.allLoaded) return;
      const nearBottom =
        window.innerHeight + window.pageYOffset >= document.body.offsetHeight - 100;
      if (nearBottom) this.loadMore();
    });

    bus.on(msgEvent('books:list'), ({ payload }) => {
      this.loading = false;

      if (!Array.isArray(payload) || payload.length === 0) {
        this.allLoaded = true;
        return;
      }

      for (const book of payload) {
        const col = document.createElement('article');
        col.className = 'col';

        const card = document.createElement('div');
        card.className = 'card h-100';
        card.dataset.bookId = String(book.id);

// hover
        card.addEventListener('mouseenter', () => {
          bus.emit(BUS_EVENTS.UI.BOOK.HOVER, { id: book.id, el: card });
        });

// click (оставим на всякий)
        card.addEventListener('click', () => {
          bus.emit(BUS_EVENTS.UI.BOOK.CLICK, { id: book.id, el: card });
        });

        const img = document.createElement('img');
        img.src = book.cover;
        img.alt = `Обложка книги '${book.title}'`;
        img.className = 'img-fluid';

        card.appendChild(img);
        col.appendChild(card);
        this.container.appendChild(col);
      }

      this.offset += payload.length;
      if (payload.length < this.limit) this.allLoaded = true;
    });
  }

  applyFilters(filters) {
    this.filters = {
      q: (filters.q || '').trim(),
      author: filters.author ?? null,
      genre: filters.genre ?? null,
      year_from: filters.year_from ?? null,
      year_to: filters.year_to ?? null,
    };

    // Сброс каталога
    this.offset = 0;
    this.allLoaded = false;
    this.loading = false;
    this.container.innerHTML = '';

    // Первая страница по новым фильтрам
    this.loadMore();
  }

    loadMore() {
    if (this.loading || this.allLoaded) return;
    this.loading = true;

    const params = new URLSearchParams();
    params.set('offset', String(this.offset));
    params.set('limit', String(this.limit));

    if (this.filters?.q) params.set('q', this.filters.q);

    if (this.filters?.author != null) params.set('author', String(this.filters.author));
    if (this.filters?.genre != null) params.set('genre', String(this.filters.genre));

    if (this.filters?.author_q) params.set('author_q', this.filters.author_q);
    if (this.filters?.genre_q) params.set('genre_q', this.filters.genre_q);

    if (this.filters?.year_from != null) params.set('year_from', String(this.filters.year_from));
    if (this.filters?.year_to != null) params.set('year_to', String(this.filters.year_to));

    ajaxClient.request({
      path: `/books?${params.toString()}`,
      method: 'GET',
      type: 'books:list',
    });
  }
}

export const catalogLoader = new CatalogLoader();
